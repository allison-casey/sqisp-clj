(ns sqisp.core-test
  (:use midje.sweet)
  (:require [sqisp.core :as sqisp]))


(facts "about `emit`"
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
             => "42"
             (with-out-str
               (sqisp/emit {:op :const, :val '-3.14, :form '-3.14, :tag 'number}))
             => "-3.14")
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
             => "is_foo")
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
             => "[1,null]"
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
             (with-out-str
               (sqisp/emit {:op :invoke
                            :form '(print "foo")
                            :fn {:op :var :form 'print :name 'print}
                            :args [{:op :const :form '"foo" :tag 'string}]}))
             => "([\"foo\"] call print)"
             (with-out-str
               (sqisp/emit {:op :invoke
                            :form '(hint "foo")
                            :fn {:op :var :form 'hint :name 'hint}
                            :args [{:op :const :form '"foo" :tag 'string}]}))
             => "(hint \"foo\")")
       (fact "maps"
             (with-out-str
               (sqisp/emit {:op :map, :form '{:hello 42}
                            :keys [{:op :const :form ':hello
                                    :val ':hello :tag 'sqp/Keyword}]
                            :vals [{:op :const :form '42
                                    :val '42 :tag 'number}]
                            :tag 'sqp/Map}))
             => "[[\":hello\"],[42]] call hash_map")
       (fact "sets"
             (with-out-str
               (sqisp/emit {:op :set, :form '{:hello 42}
                            :items [{:op :const :form ':hello
                                     :val ':hello :tag 'sqp/Keyword}
                                    {:op :const :form '42
                                     :val '42 :tag 'number}]
                            :tag 'sqp/Set}))
             => "[[\":hello\",42]] call hash_set"))

