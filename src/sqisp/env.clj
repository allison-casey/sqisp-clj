(ns sqisp.env)

(def ^:dynamic *compiler* nil)

(defmacro with-compiler-env
  "Evaluates [body] with [env] bound as the value of the `*compiler*` var in
   this namespace."
  [env & body]
  `(let [env# ~env
         env# (cond
                (map? env#) (atom env#)
                (and (instance? clojure.lang.Atom env#)
                     (map? @env#)) env#
                :default (throw (IllegalArgumentException.
                                 (str "Compiler environment must be a map or atom containing a map, not "
                                      (class env#)))))]
     (binding [*compiler* env#] ~@body)))

(defmacro ensure
  [& body]
  `(let [val# *compiler*]
     (if (nil? val#)
       (push-thread-bindings
        (hash-map (var *compiler*) (atom {}))))
     (try
       ~@body
       (finally
         (if (nil? val#)
           (pop-thread-bindings))))))
