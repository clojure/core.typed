(ns gen-doc
  (:require [codox.main :as codox]))

(defn -main [& args]
  (codox/generate-docs
    {:source-paths ["src/main/clojure"]
     :output-path "target/doc"}))
