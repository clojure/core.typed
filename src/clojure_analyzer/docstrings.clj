(ns clojure-analyzer.docstrings
  (:require [clojure-analyzer.compiler :as analyze]))

(defn check-docstring [{:keys [op init methods name env]}]
  (when (and (= :def op) 
             (= :fn (:op init)))
    (doseq [[f & rst] (map :statements (:methods init))]
      (and (= :constant (:op f))
           (string? (:form f))
           (println "Found suspicious function" name  ", in" (-> env :ns :name))))))

(def analyzed
  (map analyze/analyze-namespace '[clojure.test clojure.set clojure.java.io clojure.stacktrace clojure.pprint
                                   clojure.walk clojure.string clojure.repl clojure.core.protocols clojure.template]))

(doseq [ns-ast analyzed
        top-lvl-ast ns-ast]
  (check-docstring top-lvl-ast))

;; Output

;Analyzing clojure.set
;Found suspicious function clojure.set/bubble-max-key , in clojure.set
