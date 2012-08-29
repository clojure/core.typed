(defproject typed "0.1-alpha5"
  :description "Optional static type system"
  :dependencies [[analyze "0.1.7.2-SNAPSHOT"]
                 [vimclojure/server "2.3.3"
                  :exclusions [org.clojure/clojure]]
                 ;testing conduit
                 #_[net.intensivesystems/arrows "1.3.0"
                  :exclusions [org.clojure/clojure]]
                 [trammel "0.7.0" 
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.0.2"
                  :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.trace "0.7.3"
                  :exclusions [org.clojure/clojure]]])
