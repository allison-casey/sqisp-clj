(ns sqisp.core
  (:require
   [sqisp.analyzer :as ana]
   [cuerdas.core :as cuerdas]
   [taoensso.timbre :as timbre]
   [clojure.java.io :as io])
  (:import [java.io File Writer PushbackReader]))

;; (def ^:dynamic *out* nil)
(declare emit)

(defn compilation-error [cause]
  (ex-info nil {:sqisp.error/phase :compilation} cause))

(defn rename-to-sqf
  [file-str]
  (cond
    (cuerdas/ends-with? file-str ".sqp")
    (cuerdas/replace file-str #"\.sqp$" ".sqf")

    :else
    (throw (compilation-error (IllegalArgumentException.
                               (str "Invalid source file extension " file-str))))))

(defmulti emit* :op)

(defn emits
  ([])
  ([^Object a]
   (cond
     (nil? a) nil
     (map? a) (emit a)
     (seq? a) (apply emits a)
     (fn? a) (a)
     :else (let [s (cond-> a (not (string? a)) .toString)]
             (.write *out* s)))
   nil)
  ([a b]
   (emits a) (emits b))
  ([a b c]
   (emits a) (emits b) (emits c))
  ([a b c d]
   (emits a) (emits b) (emits c) (emits d))
  ([a b c d e]
   (emits a) (emits b) (emits c) (emits d) (emits e))
  ([a b c d e & xs]
   (emits a) (emits b) (emits c) (emits d) (emits e)
   (doseq [x xs] (emits x))))

(defn emit [ast]
  (emit* ast))

(defn wrap-in-double-quotes [x]
  (str \" x \"))

(defmulti emit-constant* class)

(defmethod emit-constant* String
  [x]
  (emits (wrap-in-double-quotes x)))

(defmethod emit-constant* Long [x] (emits x))

(defmethod emit-constant* Double [x] (emits x))

(defmethod emit-constant* Boolean [x] (emits (if x "true" "false")))

(defmethod emit-constant* nil [x] (emits "null"))

(defmethod emit-constant* clojure.lang.Keyword [x]
  (emits (wrap-in-double-quotes (str x))))

(defmethod emit* :const
  [{:keys [form]}]
  (emit-constant* form))

(defmethod emit* :invoke
  [{f :fn :keys [args env] :as expr}]
  (emits "(" "[" (comma-sep args) "]" " call " f ")"))

(defmethod emit* :map
  [{:keys [keys vals]}]
  (emits "[" "["
         (comma-sep keys)
         "]" "," "["
         (comma-sep vals)
         "]" "]"
         " call "
         (munge "hash-map")))

(defmethod emit* :set
  [{:keys [items]}]
  (emits "[" "[" (comma-sep items) "]" "]"
         " call " (munge "hash-set")))

(defn munge
  [s]
  (as-> s $
    (cuerdas/replace $ #"-" "_")
    (if (cuerdas/ends-with? $ "?") (str "is_" (cuerdas/rstrip $ "?")) $)))

(defmethod emit* :var [{:keys [form name]}]
  (emits (munge (str name))))

(defn comma-sep [xs]
  (interpose "," xs))

(defmethod emit* :vector
  [{:keys [items]}]
  (if (empty? items)
    (emits "[]")
    (emits "[" (comma-sep items) "]")))

(defn emit-source
  [^File src ^File dest]
  (timbre/debug "compiling file")
  (with-open [out ^Writer (io/make-writer dest {})]
    (binding [*out* out]
      (with-open [rdr (io/reader src)]
        (loop [forms (ana/forms-seq* rdr)]
          (if (seq forms)
            (let [form (ana/analyze (first forms))]
              (emit form)
              (recur (rest forms)))
            (let [ret {:file dest
                       :out-file (.toString dest)
                       :source-file src}]
              ret)))))))

(defn compile-file*
  [^File src ^File dest]
  (emit-source src dest))

(defn compile-file
  ([src] (compile-file src (rename-to-sqf src)))
  ([src dest]
   (let [src-file (io/file src)
         dest-file (io/file dest)]
     (compile-file* src-file dest-file))))

(comment (compile-file "resources/test.sqp"))
