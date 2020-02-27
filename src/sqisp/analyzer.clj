(ns sqisp.analyzer
  (:import [java.io PushbackReader])
  (:require
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as readers]
   [cuerdas.core :as cuerdas]
   [clojure.edn :as edn]))

(defn rotate-by-key
  [v key]
  (into {} (map (juxt #(get % key) identity) v)))

(def builtins (reduce-kv
               (fn [m k v] (assoc m (-> k cuerdas/kebab symbol) v))
               {}
               (-> "resources/commands.edn"
                   slurp
                   edn/read-string
                   (rotate-by-key :name))))

(declare analyze)

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

(defn analyze-keyword
  [env sym]
  {:op :const, :env env, :val sym, :form sym, :tag 'sqp/Keyword})

(defn analyze-symbol
  [env sym]
  {:op :var :env env :form sym :name sym})

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

(defn error
  [msg]
  (ex-info msg {:tag :sqp/analysis-error}))

;; (fn analyze-seq*
;;   [[f & args :as form]]
;;   (let [fexpr (analyze f)
;;         argexprs (mapv analyze args)]
;;     (if (contains? builtins f)
;;       {:op :builtin :form form
;;        :fn fexpr :args argexprs
;;        :builtin (get builtins f)
;;        :children [:fn :args]}
;;       {:op :invoke :form form
;;        :fn fexpr :args argexprs
;;        :children [:fn :args]})))

(defn compile-syntax-error
  [msg]
  (ex-info nil {} (RuntimeException. msg)))

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

(defn parse-builtin [env op [f & args :as form]]
  (let [enve (assoc env :context :expr)]
    {:op :builtin :form form
     :env env
     :fn (analyze enve op) :args (mapv #(analyze enve %) args)
     :builtin (get builtins f)
     :children [:fn :args :builtin]}))

(defn parse-invoke [env [f & args :as form]]
  (let [enve (assoc env :context :expr)]
    {:op :invoke :form form
     :env env
     :fn (analyze enve f) :args (mapv #(analyze enve %) args)
     :children [:fn :args]}))

(def specials '#{if do})

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
