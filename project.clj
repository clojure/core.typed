(defproject core.typed "0.1.8-SNAPSHOT"
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[analyze "0.3.0"]
                 [net.intensivesystems/arrows "1.3.0"
                  :exclusions [org.clojure/clojure]] ;for testing conduit, lein test wants it here?
                 [org.clojure/core.contracts "0.0.3"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.0.2"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojurescript "0.0-1450"]
                 [org.clojure/tools.trace "0.7.3"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojure "1.5.0"]
                 ]

  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]

  :dev-dependencies [[org.clojure/tools.macro "0.1.0"] ;for algo.monads
                     ])
