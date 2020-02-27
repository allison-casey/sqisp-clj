(ns sqisp.analyzer-test
  (:use midje.sweet)
  (:require [sqisp.analyzer :refer [empty-env] :as ana]
            [sqisp.core :as sqisp]))

(facts "about `analyze`"
       (fact "consts"
             (ana/analyze (empty-env) '"hello world")
             => {:op :const, :val '"hello world", :env (empty-env) :form '"hello world", :tag 'string}
             (ana/analyze (empty-env) '42)
             => {:op :const :val '42 :env (empty-env) :form '42 :tag 'number}
             (ana/analyze (empty-env) 'true)
             => {:op :const :val 'true :env (empty-env) :form 'true :tag 'boolean}
             (ana/analyze (empty-env) 'false)
             => {:op :const :val 'false :env (empty-env) :form 'false :tag 'boolean}
             (ana/analyze (empty-env) 'nil)
             => {:op :const :val 'nil :env (empty-env) :form 'nil :tag 'sqp-nil})
       (fact "keywords"
             (ana/analyze (empty-env) ':something)
             => {:op :const, :val ':something, :env (empty-env) :form ':something, :tag 'sqp/Keyword})
       (fact "set"
             (ana/analyze (empty-env) '#{"hello" true})
             => {:op :set
                 :env (empty-env) :form '#{"hello" true}
                 :items [{:op :const, :val '"hello", :env (empty-env) :form '"hello", :tag 'string}
                         {:op :const :val 'true :env (empty-env) :form 'true :tag 'boolean}]
                 :children [:items]
                 :tag 'sqp/Set})
       (fact "vector"
             (ana/analyze (empty-env) '[1 nil])
             => {:op :vector
                 :env (empty-env) :form '[1 nil]
                 :items [{:op :const :val '1 :env (empty-env) :form '1 :tag 'number}
                         {:op :const :val 'nil :env (empty-env) :form 'nil :tag 'sqp-nil}]
                 :children [:items]
                 :tag 'sqp/Vector})
       (fact "maps"
             (ana/analyze (empty-env) '{:foo 1, "bar" "hello"})
             => {:op :map
                 :env (empty-env) :form '{:foo 1, "bar" "hello"}
                 :keys [{:op :const, :val ':foo, :env (empty-env) :form ':foo, :tag 'sqp/Keyword}
                        {:op :const, :val '"bar", :env (empty-env) :form '"bar", :tag 'string}]
                 :vals [{:op :const :val '1 :env (empty-env) :form '1 :tag 'number}
                        {:op :const, :val '"hello", :env (empty-env) :form '"hello", :tag 'string}]
                 :children [:keys :vals]
                 :tag 'sqp/Map})
       (fact "symbols"
             (ana/analyze (empty-env) 'foo) => {:op :var :env (empty-env) :form 'foo :name 'foo})
       (fact "invoke"
             (ana/analyze (empty-env) '(print "foo"))
             => {:op :invoke :env (empty-env) :form '(print "foo")
                 :fn {:op :var :env (empty-env) :form 'print :name 'print}
                 :args [{:op :const, :val '"foo", :env (empty-env) :form '"foo", :tag 'string}]
                 :children [:fn :args]})
       (fact "builtins"
             (ana/analyze (empty-env) '(hint "foo"))
             => {:op :builtin :env (empty-env) :form '(hint "foo")
                 :fn {:op :var :env (empty-env) :form 'hint :name 'hint}
                 :builtin {:name "hint",
                           :description
                           "Outputs a hint message to the right of the screen (left of the screen in Operation Flashpoint) with a sound (except in Armed Assault). Use hintSilent for soundless hint. To split message in multiple lines either use Structured Text or \\n (in lower case).",
                           :variants
                           [{:parameters
                             '({:name "message",
                                :optional false,
                                :type "String or structured text",
                                :description "the message to display."}),
                             :syntax {:center "hint", :right "message"},
                             :returns {:type "Nothing"}}]}
                 :args [{:op :const, :val '"foo", :env (empty-env) :form '"foo", :tag 'string}]
                 :children [:fn :args :builtin]})
       (facts "specials"
              (fact "if"
                    (ana/analyze (empty-env) '(if true "hello"))
                    => {:op :if :env (empty-env)
                        :form '(if true "hello")
                        :test {:op :const :val 'true :env (assoc (empty-env) :context :expr)
                               :form 'true :tag 'boolean}
                        :then {:op :const :val '"hello" :env (empty-env) :form '"hello" :tag 'string}
                        :else {:op :const :val 'nil :env (empty-env) :form 'nil :tag 'sqp-nil}
                        :children [:test :then :else]}
                    (ana/analyze (empty-env) '(if true "hello" "world"))
                    => {:op :if :env (empty-env)
                        :form '(if true "hello" "world")
                        :test {:op :const :val 'true :env (assoc (empty-env) :context :expr)
                               :form 'true :tag 'boolean}
                        :then {:op :const :val '"hello" :env (empty-env) :form '"hello" :tag 'string}
                        :else {:op :const :val '"world" :env (empty-env) :form '"world" :tag 'string}
                        :children [:test :then :else]}
                    (ana/analyze (empty-env) '(if true)) => (throws Exception)
                    (ana/analyze (empty-env) '(if true :a :b :c)) => (throws Exception))
              (fact "do"
                    (ana/analyze (empty-env) '(do "hello"))
                    => {:op :do :env (empty-env) :form '(do "hello")
                        :statements []
                        :ret {:op :const :val '"hello" :env (empty-env) :form '"hello" :tag 'string}
                        :children [:statements :ret]}
                    (ana/analyze (empty-env) '(do "hello" "world"))
                    => {:op :do :env (empty-env) :form '(do "hello" "world")
                        :statements [{:op :const :val '"hello" :env (empty-env) :form '"hello" :tag 'string}]
                        :ret {:op :const :val '"world" :env (empty-env) :form '"world" :tag 'string}
                        :children [:statements :ret]})))

