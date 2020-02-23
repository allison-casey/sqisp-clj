(ns sqisp.analyzer-test
  (:use midje.sweet)
  (:require [sqisp.analyzer :as ana]))

(facts "about `analyze`"
       (fact "consts"
             (ana/analyze '"hello world")
             => {:op :const, :val '"hello world", :form '"hello world", :tag 'string}
             (ana/analyze '42)
             => {:op :const :val '42 :form '42 :tag 'number}
             (ana/analyze 'true)
             => {:op :const :val 'true :form 'true :tag 'boolean}
             (ana/analyze 'false)
             => {:op :const :val 'false :form 'false :tag 'boolean}
             (ana/analyze 'nil)
             => {:op :const :val 'nil :form 'nil :tag 'sqp-nil})
       (fact "keywords"
             (ana/analyze ':something)
             => {:op :const, :val ':something, :form ':something, :tag 'sqp/Keyword})
       (fact "set"
             (ana/analyze '#{"hello" true})
             => {:op :set
                 :form '#{"hello" true}
                 :items [{:op :const, :val '"hello", :form '"hello", :tag 'string}
                         {:op :const :val 'true :form 'true :tag 'boolean}]
                 :children [:items]
                 :tag 'sqp/Set})
       (fact "vector"
             (ana/analyze '[1 nil])
             => {:op :vector
                 :form '[1 nil]
                 :items [{:op :const :val '1 :form '1 :tag 'number}
                         {:op :const :val 'nil :form 'nil :tag 'sqp-nil}]
                 :children [:items]
                 :tag 'sqp/Vector})
       (fact "maps"
             (ana/analyze '{:foo 1, "bar" "hello"})
             => {:op :map
                 :form '{:foo 1, "bar" "hello"}
                 :keys [{:op :const, :val ':foo, :form ':foo, :tag 'sqp/Keyword}
                        {:op :const, :val '"bar", :form '"bar", :tag 'string}]
                 :vals [{:op :const :val '1 :form '1 :tag 'number}
                        {:op :const, :val '"hello", :form '"hello", :tag 'string}]
                 :children [:keys :vals]
                 :tag 'sqp/Map})
       (fact "symbols"
             (ana/analyze 'foo) => {:op :var :form 'foo :name 'foo})
       (fact "invoke"
             (ana/analyze '(print "foo"))
             => {:op :invoke :form '(print "foo")
                 :fn {:op :var :form 'print :name 'print}
                 :args [{:op :const, :val '"foo", :form '"foo", :tag 'string}]
                 :children [:fn :args]})
       ;; (fact "seqs"
       ;;       (ana/analyze '(print "hello"))
       ;;       => {:op :invoke
       ;;           :form '(print "hello")
       ;;           :fn {:op :symbo}})
       )
