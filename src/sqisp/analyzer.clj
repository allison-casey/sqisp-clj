(ns sqisp.analyzer
  (:import [java.io PushbackReader])
  (:require
   [sqisp.env :as env]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as readers]
   [cuerdas.core :as cuerdas]
   [clojure.edn :as edn]))

(declare analyze)

(def specials '#{if do let global})

(defn munge-global
  [sym-name]
  (-> sym-name
      str
      (cuerdas/strip-prefix "_")
      munge))

(defn munge-local
  [sym-name]
  (->> sym-name
       str
       (str "_")
       munge))

(defn rotate-by-key
  [v key]
  (into {} (map (juxt #(get % key) identity) v)))

(def builtins (reduce-kv
               (fn [m k v] (let [munged-name (str "bis/" (cuerdas/kebab k))]
                            (assoc m (symbol munged-name) v)))
               {}
               (-> "resources/commands.edn"
                   slurp
                   edn/read-string
                   (rotate-by-key :name))))

(defn error
  [msg]
  (ex-info msg {:tag :sqp/analysis-error}))

(defn compile-syntax-error
  [msg]
  (ex-info nil {} (RuntimeException. msg)))

(defn forms-seq*
  [rdr]
  (let [eof-sentinel (Object.)
        opts {:eof eof-sentinel}
        pbr (readers/indexing-push-back-reader (PushbackReader. rdr) 1 nil)
        forms-seq_
        (fn forms-seq_ []
          (lazy-seq
           (let [form (reader/read opts pbr)]
             (if (identical? form eof-sentinel)
               (.close rdr)
               (cons form (forms-seq_))))))]
    (forms-seq_)))

(defn empty-env
  []
  {:context :statement
   :locals {}})

(defmulti parse (fn [op & rest] op))

(defmethod parse 'if
  [op env [_ test then else :as form]]
  (when (< (count form) 3)
    (throw (compile-syntax-error "Too few arguments to if")))
  (when (> (count form) 4)
    (throw (compile-syntax-error "Too many arguments to if")))
  (let [test-expr (analyze (assoc env :context :expr) test)
        then-expr (analyze env then)
        else-expr (analyze env else)]
    {:op :if :form form
     :env env
     :test test-expr :then then-expr :else else-expr
     :children [:test :then :else]}))

(defmethod parse 'do
  [op env [_ & exprs :as form]]
  (let [statements (mapv #(analyze env %) (butlast exprs))]
    (if (<= (count exprs) 1)
      (let [ret (analyze env (first exprs))]
        {:op :do :form form
         :env env
         :statements statements
         :ret ret
         :children [:statements :ret]})
      (let [ret (analyze env (last exprs))]
        {:op :do :form form
         :env env
         :statements statements
         :ret ret
         :children [:statements :ret]}))))

(defmethod parse 'global
  [op env form]
  (let [[_ sym init] form
        var-name (analyze (-> env
                              (dissoc :locals)
                              (assoc :context :expr)
                              (assoc :def-var? true))
                          sym)
        init-expr (do
                    (swap! env/*compiler* assoc-in [:defs sym]
                           {:name var-name})
                    (analyze (assoc env :context :expr) init))]
    {:env env
     :form form
     :op :global
     :name sym
     :var var-name
     :init init-expr
     :children [:var :init]}))

(defn analyze-let-bindings
  [encl-env bindings]
  (loop [bes []
         env (assoc encl-env :context :expr)
         bindings (seq (partition 2 bindings))]
    (if-some [[name init] (first bindings)]
      (let [init-expr (analyze env init)
            be {:name (munge-local name)
                :init init-expr
                :op :binding}]
        (recur (conj bes be)
               (assoc-in env [:locals name] be)
               (next bindings)))
      [bes env])))

(defmethod parse 'let
  [op encl-env [_ bindings & exprs :as form]]
  (let [context (:context encl-env)
        [bes env] (analyze-let-bindings encl-env bindings)
        expr (analyze (assoc env :context (if (= :expr context) :return context))
                      `(do ~@exprs))]
    {:op :let
     :env encl-env
     :bindings bes
     :body expr
     :form form
     :children [:bindings :body]}))

(defn parse-builtin
  [env op [f & args :as form]]
  (let [enve (assoc env :context :expr)
        builtin-name (symbol (:name (get builtins op)))]
    {:op :builtin
     :form form
     :env env
     :fn (analyze enve builtin-name)
     :args (mapv #(analyze enve %) args)
     :builtin (get builtins f)
     :children [:fn :args :builtin]}))

(defn parse-invoke
  [env [f & args :as form]]
  (let [enve (assoc env :context :expr)]
    {:op :invoke
     :form form
     :env env
     :fn (analyze enve f)
     :args (mapv #(analyze enve %) args)
     :children [:fn :args]}))

(defn analyze-keyword
  [env sym]
  {:op :const, :env env, :val sym, :form sym, :tag 'sqp/Keyword})

(defn resolve-var
  [env sym]
  (let [s (str sym)
        locals (:locals env)
        local-bind (get locals sym)
        pre (->> (cuerdas/split (name sym) #"\.") (map symbol) vec)
        sym-name (if (:def-var? env)
                   (munge-global sym))]
    {:name (or sym-name s)
     :op (if (:def-var? env) :var :sqf-var)}))

(defn analyze-symbol
  [env sym]
  (let [ret {:env env :form sym}
        info (resolve-var env sym)]
    (if-some [local-bind (get (:locals env) sym)]
      (merge
       (assoc ret :op :local :info local-bind)
       (select-keys local-bind [:name :local :init]))
      (if (true? (:def-var? env))
        (merge
         (assoc ret :op :var :info info)
         (select-keys info [:op :name]))
        (merge
         (assoc ret :info info)
         (select-keys info [:op :name]))))))

(defn analyze-set
  [env form]
  (let [items (mapv #(analyze env %) form)]
    {:op :set :env env :form form :items items :children [:items] :tag 'sqp/Set}))

(defn analyze-vector
  [env form]
  (let [items (mapv #(analyze env %) form)]
    {:op :vector :env env :form form
     :items items :children [:items] :tag 'sqp/Vector}))

(defn analyze-map
  [env form]
  (let [ks (mapv #(analyze env %) (keys form))
        vs (mapv #(analyze env %) (vals form))]
    {:op :map
     :env env
     :form form
     :keys ks
     :vals vs
     :children [:keys :vals]
     :tag 'sqp/Map}))

(defn analyze-seq*
  [env op form]
  (cond
    (contains? specials op) (parse op env form)
    (contains? builtins op) (parse-builtin env op form)
    :else (parse-invoke env form)))

(defn analyze-seq
  [env form]
  (let [op (first form)]
    (when (nil? op)
      (throw (error "Can't call nil")))
    (analyze-seq* env op form)))

(defn analyze-form
  [env form]
  (cond
    (symbol? form) (analyze-symbol env form)
    (and (seq? form) (seq form)) (analyze-seq env form)
    (map? form) (analyze-map env form)
    (keyword? form) (analyze-keyword env form)
    (set? form) (analyze-set env form)
    (vector? form) (analyze-vector env form)
    :else
    (let [tag (cond
                (string? form) 'string
                (number? form) 'number
                (boolean? form) 'boolean
                (nil? form) 'sqp-nil)]
      (cond-> {:op :const :env env :val form :form form}
        tag (assoc :tag tag)))))

(defn analyze
  [env form] (analyze-form env form))
