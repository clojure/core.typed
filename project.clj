(defproject org.clojure/core.typed "0.2.51-SNAPSHOT"
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/jvm.tools.analyzer "0.6.1"
                  :exclusions [org.clojure/clojure
                               org.clojure/clojurescript]]
                 [org.clojure/tools.analyzer.jvm "0.1.0-20140604.170146-234"]
                 [org.clojure/tools.reader "0.8.4"]
                 [org.clojure/core.contracts "0.0.4"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.0.2"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojurescript "0.0-2227"]
                 [org.clojure/tools.trace "0.7.5"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/core.cache "0.6.3"
                  :exclusions [org.clojure/data.priority-map]]
                 [org.clojure/data.priority-map "0.0.4"]
                 ; CLJS fireplace REPL
                 [com.cemerick/piggieback "0.1.3"
                  :exclusions [org.clojure/tools.reader
                               org.clojure/clojurescript]]
                 ]

  ; fireplace repl middleware
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :plugins [[lein-typed "0.3.1"]]
  :core.typed {:check [clojure.core.typed.test.records]
               :check-cljs []}

  :global-vars {*warn-on-reflection* true}

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  :source-paths ["src/main/clojure"
                 "src/main/cljs"
                 #_"../clojurescript/src/clj"
                 #_"../clojurescript/src/cljs"]
  :test-paths ["src/test/clojure"
               "src/test/cljs"]

  :profiles {:dev {:repl-options {:port 64464}}}

  :cljsbuild {:builds {}}

  :dev-dependencies [])
