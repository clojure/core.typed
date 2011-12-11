(ns clojure-analyzer.docstrings
  (:require [clojure-analyzer.compiler :as analyze]))

(defn check-docstring [{:keys [op init methods name env]}]
  (when (and (= :def op) 
             (= :fn (:op init)))
    (doseq [[f & rst] (map :statements (:methods init))]
      (and (= :constant (:op f))
           (string? (:form f))
           (println "Found suspicious function" name  ", in" (-> env :ns :name))))))

(defn check-docstring-namespace [ns-sym]
  (analyze/analyze-namespace ns-sym check-docstring))

(doseq [n '#{clojure.set}]
  (check-docstring-namespace n))

;; Output

;Analyzing clojure.set
;Found suspicious function clojure.set/bubble-max-key , in clojure.set
