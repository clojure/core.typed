(defproject typed "0.1.7-SNAPSHOT"
  :description "Gradual typing for Clojure"
  :dependencies [[analyze "0.2.7-SNAPSHOT"]
                 [net.intensivesystems/arrows "1.3.0"
                  :exclusions [org.clojure/clojure]] ;for testing conduit, lein test wants it here?
                 [trammel "0.7.0"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.0.2"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojurescript "0.0-1450"]
                 [org.clojure/tools.trace "0.7.3"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojure "1.5.0-RC1"]
                 ]
  :dev-dependencies [[org.clojure/tools.macro "0.1.0"] ;for algo.monads
                     ])
