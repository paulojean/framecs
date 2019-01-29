(defproject framecs "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clomacs "0.0.3-SNAPSHOT"]]

  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}
             :test {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :repl-options {:init-ns framecs.core})
