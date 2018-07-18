(defn find-file [d f]
  (.trim (:out (clojure.java.shell/sh
                "bash" "-c" (format "find %s -name %s" d f)))))

(defn yjp-file [f]
  (find-file "/Applications/YourKit*" f))

(defn libyjp-jar-path []
  (yjp-file "yjp-controller-api-redist.jar"))

(defn libyjp-agent-path []
  (yjp-file "libyjpagent.jnilib"))

(defproject org.clojure/core.typed "0.2.90-SNAPSHOT" ;; ignore this version, see pom.xml
  :description "Gradual typing for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "0.0-SNAPSHOT"]
                 [org.clojure/clojurescript "1.10.373"]
                 [com.taoensso/timbre "2.1.2"]
                 [org.clojure/core.match "0.2.0-alpha12"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/tools.trace "0.7.5" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.analyzer.jvm "0.7.0"]
                 [org.clojure/tools.reader "1.1.1"]
                 [org.clojure/math.combinatorics "0.1.4" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.namespace "0.3.0-alpha4"]
                 [org.clojure/core.cache "0.6.5"]
                 [com.gfredericks/test.chuck "0.2.6"]
                 [org.clojure/test.check "0.9.0"]
                 [rhizome "0.2.5"]
                 ]

  :repl-options {:port 64545
                 :timeout 6645464645555}
  :profiles {:dev {:dependencies [[cider/piggieback "0.3.6"]
                                  [org.clojure/tools.nrepl "0.2.13"]]
                   :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}} }
  :injections [(require 'clojure.core.typed)
               (clojure.core.typed/install
                 #{:load})]

  ;:global-vars {*warn-on-reflection* true}

  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

  :java-source-paths ["module-check/src/main/java"
                      "module-infer/src/main/java"]
  :source-paths ["module-cljs/src/main/clojure"
                 "module-cljs/src/main/cljs"
                 "module-analyzer-jvm/src/main/clojure"
                 "module-infer/src/main/clojure"
                 "module-infer/src/main/cljs"
                 "module-check/src/main/clojure"
                 "module-check/src/main/cljs"
                 "module-rt/src/main/clojure"
                 "module-rt/src/main/cljs"]
  :test-paths ["module-cljs/src/test/clojure"
               "module-cljs/src/test/cljs"
               "module-analyzer-jvm/src/test/clojure"
               "module-infer/src/test/clojure"
               "module-infer/src/test/cljs"
               "module-check/src/test/clojure"
               "module-check/src/test/cljs"
               "module-check/src/test/resources"
               "module-rt/test/clojure"
               "module-rt/test/cljs"]
	;:resource-paths [~(libyjp-jar-path)]

  :jvm-opts ["-Xss4m" #_~(str "-agentpath:" (libyjp-agent-path))]
  :cljsbuild {:builds {}})


