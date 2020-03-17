(ns sqisp.core
  (:require
   [sqisp.analyzer :as ana]
   [sqisp.env :as env]
   [cuerdas.core :as cuerdas]
   [clojure.pprint :refer [pprint]]
   [taoensso.timbre :as timbre]
   [clojure.java.io :as io])
  (:import [java.io File Writer PushbackReader]))


;; (def ^:dynamic *out* nil)
(declare emit)

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     ~@body
     (when-not (= :expr (:context env#)) (emitln ";"))))

(defn comma-sep [xs]
  (interpose "," xs))

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

(defn emitln
  ([] (newline))
  ([a]
   (emits a) (newline))
  ([a b]
   (emits a) (emits b) (newline))
  ([a b c]
   (emits a) (emits b) (emits c) (newline))
  ([a b c d]
   (emits a) (emits b) (emits c) (emits d) (newline))
  ([a b c d e]
   (emits a) (emits b) (emits c) (emits d) (emits e) (newline))
  ([a b c d e & xs]
   (emits a) (emits b) (emits c) (emits d) (emits e)
   (doseq [x xs] (emits x))
   (newline)))

(defn wrap-in-double-quotes [x]
  (str \" x \"))

(defmulti emit-constant* class)

(defmethod emit-constant* String
  [x]
  (emits (wrap-in-double-quotes x)))

(defmethod emit-constant* Long [x] (emits "(" x ")"))

(defmethod emit-constant* Double [x] (emits "(" x ")"))

(defmethod emit-constant* Boolean [x] (emits (if x "true" "false")))

(defmethod emit-constant* nil [x] (emits "null"))

(defmethod emit-constant* clojure.lang.Keyword [x]
  (emits (wrap-in-double-quotes (str x))))

(defmethod emit* :const
  [{:keys [form env]}]
  (when-not (= :statement (:context env))
    (emit-constant* form)))

(defmethod emit* :invoke
  [{f :fn :keys [args env] :as expr}]
  (emit-wrap env (emits "(" "[" (comma-sep args) "]" " call " f ")")))

(defmethod emit* :builtin
  [{f :fn :keys [args env builtin] :as expr}]
  (emit-wrap
   env
   (case (count args)
     0 (emits "(" (:name builtin) ")")
     1 (emits "(" f " " (first args) ")")
     2 (emits "(" (first args) " " f " " (second args) ")"))))

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

(defn truthy-constant?
  [{:keys [op form]}]
  (and (= op :const)
       form
       (not (or (and (string? form) (= form ""))
                (and (number? form) (zero? form))))))

(defn falsey-constant?
  [{:keys [op form]}]
  (and (= op :const)
       (or (false? form) (nil? form))))

(defmethod emit* :if
  [{:keys [env test then else]}]
  (let [context (:context env)]
    (emit-wrap
     env
     (cond
       (truthy-constant? test) (emitln then)
       (falsey-constant? test) (emitln else)
       :else (do
               (when (= context :expr) (emits "("))
               (emitln "if (" test ") then {")
               (emit then)
               (emitln "} else {")
               (emit else)
               (emits "}")
               (when (= context :expr) (emits ")")))))))

(defmethod emit* :do
  [{:keys [statements ret env]}]
  (let [context (:context env)]
    (when (and (seq statements) (= :expr context)) (emitln "([] call {"))
    (doseq [s statements] (emitln s))
    (emit ret)
    (emitln ";")
    (when (and (seq statements) (= :expr context)) (emitln "})"))))

(defmethod emit* :global
  [{:keys [name var init env] :as form}]
  (let [mname (munge name)]
    (emits var)
    (emits " = " init)
    (emitln ";")))

(defmethod emit* :var [{:keys [form name]}]
  (emits (munge (str name))))

(defmethod emit* :sqf-var [{:keys [form name]}]
  (emits (munge (str name))))

(defmethod emit* :local [{:keys [form name] :as env}]
  (emits (str name)))

(defmethod emit* :vector
  [{:keys [items]}]
  (if (empty? items)
    (emits "[]")
    (emits "[" (comma-sep items) "]")))

(defmethod emit* :let
  [{expr :body :keys [bindings env]}]
  (let [context (:context env)]
    (when (= :expr context) (emits "(call {"))
    (doseq [{:keys [init] :as binding} bindings]
      (emits "private ")
      (emit binding)
      (emitln " = " init ";"))
    (emits expr)
    (when (= :expr context) (emits "})"))))

(defmethod emit* :binding
  [{:keys [form name]}]
  (emits (munge (str name))))

(defn emit-source
  [^File src ^File dest]
  (timbre/debug "compiling file")
  (env/ensure
   (with-open [out ^Writer (io/make-writer dest {})]
     (binding [*out* out]
       (with-open [rdr (io/reader src)]
         (let [env (ana/empty-env)]
           (loop [forms (ana/forms-seq* rdr)]
             (if (seq forms)
               (let [form (ana/analyze env (first forms))]
                 (emit form)
                 (recur (rest forms)))
               (let [ret {:file dest
                          :out-file (.toString dest)
                          :source-file src}]
                 ret)))))))))

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
