(defproject org.clojure/core.typed "0.2.90-SNAPSHOT"
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/jvm.tools.analyzer "0.6.1"
                  :exclusions [org.clojure/clojure
                               org.clojure/clojurescript]]
                 [org.clojure/tools.analyzer.jvm "0.3.0"]
                 [org.clojure/tools.reader "0.9.2"]
                 [org.clojure/core.contracts "0.0.4"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.1.1"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojurescript "0.0-2268"]
                 [org.clojure/tools.trace "0.7.5"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojure "1.7.0-RC2"]
                 [org.clojure/tools.namespace "0.2.5"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [org.clojure/core.cache "0.6.4"
                  :exclusions [org.clojure/data.priority-map]]
                 [org.clojure/data.priority-map "0.0.4"]
                 ; CLJS fireplace REPL
                 [com.cemerick/piggieback "0.1.3"
                  :exclusions [org.clojure/tools.reader
                               org.clojure/clojurescript]]
                 ]

  ; fireplace repl middleware
  :repl-options {:nrepl-middleware [#_cemerick.piggieback/wrap-cljs-repl
                                    clojure.core.typed.repl/wrap-clj-repl]}

  :plugins [[lein-typed "0.3.1"]]
  :core.typed {:check [clojure.core.typed.test.records]
               :check-cljs []}

  :global-vars {*warn-on-reflection* true}

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  :source-paths ["module-check/src/main/clojure"
                 "module-check/src/main/cljs"
                 "module-rt/src/main/clojure"
                 "module-rt/src/main/cljs"
                 #_"../clojurescript/src/clj"
                 #_"../clojurescript/src/cljs"]
  :test-paths ["module-check/src/test/clojure"
               "module-check/src/test/cljs"
               "module-rt/src/test/clojure"
               "module-rt/src/test/cljs"]

  :profiles {:dev {:repl-options {:port 64499}}}

  :cljsbuild {:builds {}}

  :dev-dependencies [])
