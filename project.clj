(defproject org.typedclojure/core.typed.lang.jvm "0.7.1-SNAPSHOT"
  :description "Extensible languages in Clojure, a la Racket's #lang."
  :url "https://github.com/typedclojure/core.typed.lang.jvm"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories [["releases"  {:sign-releases false :url "https://clojars.org/repo"}]
                        ["snapshots" {:sign-releases false :url "https://clojars.org/repo"}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"]
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :source-paths ["src/main/clojure/"]
  :test-paths ["src/test/clojure/"]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]])
