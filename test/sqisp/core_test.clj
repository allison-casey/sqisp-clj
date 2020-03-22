(ns sqisp.core-test
  (:use midje.sweet)
  (:require [sqisp.core :as sqisp]
            [sqisp.analyzer :as ana]
            [sqisp.env :as env]
            [clojure.pprint :refer [pprint]]
            [sqisp.analyzer :refer [empty-env]]))

;; (sqisp/emit {:op :if :env (empty-env) :form '(if true "hello")
;;   :test {:op :const :val 'true :env (empty-env) :form 'true :tag 'boolean}
;;   :then {:op :const :val '"hello" :env (empty-env) :form '"hello" :tag 'string}
;;   :else {:op :const :val 'nil :env (empty-env) :form 'nil :tag 'sqp-nil}
;;   :children [:test :then :else]})

(facts
 "about `emit`"
 (fact "strings"
       (with-out-str
         (sqisp/emit {:op :const, :val '"foo", :form '"foo", :tag 'string}))
       => "\"foo\""
       (with-out-str
         (sqisp/emit {:op :const, :val '"bar", :form '"bar", :tag 'string}))
       => "\"bar\""
       (with-out-str
         (sqisp/emit {:op :const, :val '"", :form '"", :tag 'string}))
       => "\"\"")
 (fact "numbers"
       (with-out-str
         (sqisp/emit {:op :const, :val '42, :form '42, :tag 'number}))
       => "(42)"
       (with-out-str
         (sqisp/emit {:op :const, :val '-3.14, :form '-3.14, :tag 'number}))
       => "(-3.14)")
 (fact "boolean"
       (with-out-str
         (sqisp/emit {:op :const, :val 'true, :form 'true, :tag 'boolean}))
       => "true"
       (with-out-str
         (sqisp/emit {:op :const, :val 'false, :form 'false, :tag 'boolean}))
       => "false")
 (fact "nil"
       (with-out-str
         (sqisp/emit {:op :const, :val 'nil, :form 'nil, :tag 'sq-nil}))
       => "null")
 (fact "keywords"
       (with-out-str
         (sqisp/emit {:op :const, :val ':foo, :form ':foo, :tag 'sqp/Keyword}))
       => "\":foo\"")
 (fact "symbols"
       (with-out-str
         (sqisp/emit {:op :var :form 'foo :name 'foo}))
       => "foo"
       (with-out-str
         (sqisp/emit {:op :var :form 'foo-bar :name 'foo-bar}))
       => "foo_bar"
       (with-out-str
         (sqisp/emit {:op :var :form 'foo? :name 'foo?}))
       => "foo_QMARK_")
 (fact "vectors"
       (with-out-str
         (sqisp/emit {:op :vector, :val '[], :form '[], :tag 'sq-nil}))
       => "[]"
       (with-out-str
         (sqisp/emit {:op :vector
                      :form '[1 nil]
                      :items [{:op :const :val '1 :form '1 :tag 'number}
                              {:op :const :val 'nil :form 'nil :tag 'sqp-nil}]
                      :children [:items]
                      :tag 'sqp/Vector}))
       => "[(1),null]"
       (with-out-str
         (sqisp/emit {:op :vector
                      :form '["hello" [false]]
                      :items [{:op :const :val '"hello"
                               :form '"hello" :tag 'string}
                              {:op :vector, :form '[1, false]
                               :items [{:op :const :val 'false
                                        :form 'false :tag 'boolean}]
                               :children [:items]
                               :tag 'sqp/Vector}]
                      :children [:items]
                      :tag 'sqp/Vector}))
       => "[\"hello\",[false]]")
 (fact "invoke"
       (fact "special"
             (with-out-str
               (sqisp/emit {:op :if
                            :env (empty-env)
                            :form '(if true "hello")
                            :test {:op :const :env (empty-env)
                                   :form 'true :tag 'boolean}
                            :then {:op :const :env (empty-env)
                                   :form '"hello" :tag 'string}
                            :else {:op :const :env (empty-env)
                                   :form 'nil :tag 'sqp-nil}}))
             => "\n;\n")
       (fact "invoke"
             (with-out-str
               (sqisp/emit {:op :invoke
                            :form '(print "foo")
                            :fn {:op :var :form 'print :name 'print}
                            :args [{:op :const :form '"foo" :tag 'string}]}))
             => "([\"foo\"] call print);\n")
       (fact "builtins"
             (with-out-str
               (sqisp/emit {:op :builtin
                            :form '(select [:foo] 0)
                            :fn {:op :var :form 'select :name 'select}
                            :builtin {:name "select"}
                            :args [{:op :vector :form '[:foo]
                                    :items [{:op :const, :val ':foo,
                                             :form ':foo, :tag 'sqp/Keyword}]}
                                   {:op :const, :val '0
                                    :form '0, :tag 'number}]}))
             => "([\":foo\"] select (0));\n"
             (with-out-str
               (sqisp/emit {:op :builtin
                            :form '(all-units)
                            :builtin {:name "allUnits",
                                      :variants
                                      [{:parameters (),
                                        :syntax {:center "allUnits"},
                                        :returns {:type "Array"}}]}
                            :fn {:op :var :form 'all-units :name 'all-units}
                            :args []}))
             => "(allUnits);\n"
             (with-out-str
               (sqisp/emit {:op :builtin
                            :form '(hint "foo")
                            :fn {:op :var :form 'hint :name 'hint}
                            :args [{:op :const :form '"foo" :tag 'string}]}))
             => "(hint \"foo\");\n"))
 (fact "maps"
       (with-out-str
         (sqisp/emit {:op :map, :form '{:hello 42}
                      :keys [{:op :const :form ':hello
                              :val ':hello :tag 'sqp/Keyword}]
                      :vals [{:op :const :form '42
                              :val '42 :tag 'number}]
                      :tag 'sqp/Map}))
       => "[[\":hello\"],[(42)]] call hash_map")
 (fact "sets"
       (with-out-str
         (sqisp/emit {:op :set, :form '{:hello 42}
                      :items [{:op :const :form ':hello
                               :val ':hello :tag 'sqp/Keyword}
                              {:op :const :form '42
                               :val '42 :tag 'number}]
                      :tag 'sqp/Set}))
       => "[[\":hello\",(42)]] call hash_set"))


;; Special Forms
(fact "global"
      (-> (env/ensure
           (ana/analyze
            (empty-env)
            '(global x-something!! nil)))
          sqisp/emit
          with-out-str)
      => "x_something_BANG__BANG_ = null;\n")

(fact "if"
      ;; context statement
      (-> (env/ensure
           (ana/analyze
            (empty-env)
            '(if (bis/all-units)
               (bis/hint :a)
               (bis/hint :b))))
          sqisp/emit
          with-out-str)
      => (str "if ((allUnits)) then {\n"
              "(hint \":a\");\n"
              "} else {\n"
              "(hint \":b\");\n"
              "};\n")
      ;; context expression
      (-> (env/ensure
           (ana/analyze
            (empty-env)
            '(bis/hint (if (bis/all-units) :a :b))))
          sqisp/emit
          with-out-str)
      => (str "(hint (if ((allUnits)) then {\n"
              "\":a\"} else {\n"
              "\":b\"}));\n"))


(comment
  (-> (env/with-compiler-env {}
       (ana/analyze
        (empty-env)
        '(global something "hello")))
      sqisp/emit
      with-out-str
      println
      ;; pprint
      ))
