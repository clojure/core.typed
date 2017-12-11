(defproject org.clojure/core.typed "0.2.90-SNAPSHOT" ;; ignore this version, see pom.xml
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 ;[org.clojure/clojurescript "0.0-SNAPSHOT"]
                 [org.clojure/clojurescript "1.9.946"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/tools.trace "0.7.5" :exclusions [org.clojure/clojure]]
                 [org.clojure/jvm.tools.analyzer "0.6.2" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.analyzer.jvm "0.7.0"]
                 [org.clojure/tools.reader "1.0.0-beta3"]
                 [org.clojure/math.combinatorics "0.1.3" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.namespace "0.3.0-alpha3"]
                 [org.clojure/core.cache "0.6.4"]
                 [com.gfredericks/test.chuck "0.2.6"]
                 [org.clojure/test.check "0.9.0"]
                 [rhizome "0.2.5"]
                 ]

  :repl-options {:port 64540
                 :timeout 6645464645555}

  ; fireplace repl middleware
  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.1"]
                                  [org.clojure/tools.nrepl "0.2.10"]]
                   ; CLJS fireplace REPL
                   :repl-options {:port 64540
                                  :timeout 6645464644444455
                                  ;:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                                  }
                   }
             }

  :injections [(require 'clojure.core.typed)
               (clojure.core.typed/install
                 #{:load})]

  :global-vars {*warn-on-reflection* true}

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  :java-source-paths ["module-check/src/main/java"]
  :source-paths [#_"module-cljs/src/main/clojure"
                 #_"module-cljs/src/main/cljs"
                 "module-check/src/main/clojure"
                 "module-check/src/main/cljs"
                 "module-rt/src/main/clojure"
                 "module-rt/src/main/cljs"]
  :test-paths [#_"module-cljs/src/test/clojure"
               #_"module-cljs/src/test/cljs"
               "module-check/src/test/clojure"
               "module-check/src/test/cljs"
               "module-check/src/test/resources"
               "module-rt/test/clojure"
               "module-rt/test/cljs"]

  :jvm-opts ["-Xss4m"]
  :cljsbuild {:builds {}})


