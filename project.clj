(defproject org.typedclojure/core.typed.lang.jvm "0.7.0-SNAPSHOT"
  :description "Extensible languages in Clojure, a la Racket's #lang."
  :url "https://github.com/typedclojure/core.typed.lang.jvm"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["releases"  {#_#_:sign-releases false :url "https://clojars.org/repo"}]
                        ["snapshots" {#_#_:sign-releases false :url "https://clojars.org/repo"}]]
  :source-paths ["src/main/clojure/"]
  :test-paths ["src/test/clojure/"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]])
