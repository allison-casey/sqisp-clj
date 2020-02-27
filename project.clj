(defproject sqisp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/tools.reader "1.3.2"]
                 [org.clojure/clojurescript "1.10.597"]
                 [midje "1.9.9"]
                 [funcool/cuerdas "2.2.0"]]
  :repl-options {:init-ns sqisp.core})
