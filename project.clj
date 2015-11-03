(defproject org.clojure/core.typed "0.2.90-SNAPSHOT" ;; ignore this version, see pom.xml
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-2268"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [org.clojure/core.async "0.2.371"]
                 [org.clojure/tools.trace "0.7.5" :exclusions [org.clojure/clojure]]
                 ; CLJS fireplace REPL
                 [com.cemerick/piggieback "0.1.3" :exclusions [org.clojure/tools.reader
                                                               org.clojure/clojurescript]]
                 [org.clojure/jvm.tools.analyzer "0.6.1" :exclusions [org.clojure/clojure 
                                                                      org.clojure/clojurescript]]
                 [org.clojure/tools.analyzer.jvm "0.6.8"]
                 [org.clojure/tools.reader "0.9.2"]
                 [org.clojure/core.contracts "0.0.4" :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.1.1" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.namespace "0.2.5"]
                 [org.clojure/core.cache "0.6.4"]
                 ]
  ;; for tools.reader 0.9.2
  :aot [#_.clojure.tools.reader.impl.ExceptionInfo
        ;; for asm-all
        #_.org.objectweb.asm.Type
        #_.org.objectweb.asm.Opcodes]

  ; fireplace repl middleware
  :repl-options {:nrepl-middleware [#_cemerick.piggieback/wrap-cljs-repl
                                    #_clojure.core.typed.repl/wrap-clj-repl]}

  :plugins [[lein-typed "0.3.1"]
            [org.typedclojure/mranderson "0.4.4"]]
  :core.typed {:check [clojure.core.typed.test.records]
               :check-cljs []}

;  :injections [;; ExceptionInfo must be AOT compiled at this point
;               ;; If not, run `lein repl` with the follow two lines
;               ;; commented out, then don't `lein clean` after they are
;               ;; readded.
;               (require 'clojure.core.typed.load)
;               (require 'clojure.core.typed.lang)
;               (clojure.core.typed.load/install-typed-load)
;               (clojure.core.typed.lang/monkey-patch-extensible-load)]

  :global-vars {*warn-on-reflection* true}

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  ;:java-source-paths ["module-check/src/main/java"]
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
