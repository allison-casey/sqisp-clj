(ns sqisp.analyzer
  (:import [java.io PushbackReader])
  (:require
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as readers]
   [clojure.edn :as edn]))

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

(defn analyze-keyword
  [sym]
  {:op :const, :val sym, :form sym, :tag 'sqp/Keyword})

(defn analyze-symbol
  [sym]
  {:op :var :form sym :name sym})

(defn analyze-set
  [form]
  (let [items (mapv #(analyze %) form)]
    {:op :set :form form :items items :children [:items] :tag 'sqp/Set}))

(defn analyze-vector
  [form]
  (let [items (mapv #(analyze %) form)]
    {:op :vector :form form :items items :children [:items] :tag 'sqp/Vector}))

(defn analyze-map
  [form]
  (let [ks (mapv #(analyze %) (keys form))
        vs (mapv #(analyze %) (vals form))]
    {:op :map
     :form form
     :keys ks
     :vals vs
     :children [:keys :vals]
     :tag 'sqp/Map}))

(defn analyze-seq
  [[f & args :as form]]
  (let [fexpr (analyze f)
        argexprs (mapv analyze args)]
    {:op :invoke :form form
     :fn fexpr :args argexprs
     :children [:fn :args]}))

(defn analyze-form
  [form]
  (cond
    (symbol? form) (analyze-symbol form)
    (and (seq? form) (seq form)) (analyze-seq form)
    (map? form) (analyze-map form)
    (keyword? form) (analyze-keyword form)
    (set? form) (analyze-set form)
    (vector? form) (analyze-vector form)
    :else
    (let [tag (cond
                (string? form) 'string
                (number? form) 'number
                (boolean? form) 'boolean
                (nil? form) 'sqp-nil)]
      (cond-> {:op :const :val form :form form}
        tag (assoc :tag tag)))))

(defn analyze
  [form] (analyze-form form))
