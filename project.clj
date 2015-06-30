(defproject org.clojure/core.typed "0.2.90-SNAPSHOT"
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0-RC2"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [org.clojure/tools.trace "0.7.5" :exclusions [org.clojure/clojure]]
                 ; CLJS fireplace REPL
                 [com.cemerick/piggieback "0.1.3" :exclusions [org.clojure/tools.reader
                                                               org.clojure/clojurescript]]
                 ^:source-dep 
                 [org.clojure/jvm.tools.analyzer "0.6.1" :exclusions [org.clojure/clojure 
                                                                      org.clojure/clojurescript]]
                 ^:source-dep 
                 [org.clojure/tools.analyzer.jvm "0.3.0" :exclusions [org.ow2.asm/asm-all]]
                 ^:source-dep 
                 [org.ow2.asm/asm-all "4.2" :classifier "sources"]
                 ^:source-dep
                 [org.clojure/tools.reader "0.9.2"]
                 ^:source-dep
                 [org.clojure/core.contracts "0.0.4" :exclusions [org.clojure/clojure]]
                 ^:source-dep
                 [org.clojure/math.combinatorics "0.1.1" :exclusions [org.clojure/clojure]]
                 ^:source-dep
                 [org.clojure/tools.namespace "0.2.5"]
                 ^:source-dep
                 [org.clojure/core.cache "0.6.4"]
                 ]
  ;; for tools.reader 0.9.2
  :aot [clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo]

  ; fireplace repl middleware
  :repl-options {:nrepl-middleware [#_cemerick.piggieback/wrap-cljs-repl
                                    clojure.core.typed.repl/wrap-clj-repl]}

  :plugins [[lein-typed "0.3.1"]
            [thomasa/mranderson "0.4.4-ambrosebs-SNAPSHOT"]]
  :core.typed {:check [clojure.core.typed.test.records]
               :check-cljs []}

  :global-vars {*warn-on-reflection* true}

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  :java-source-paths ["module-check/src/main/java"]
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
