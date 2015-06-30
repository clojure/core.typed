(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.examples.privatevars
  (:require [clojure.core.typed.deps.clojure.jvm.tools.analyzer :as analyze]
            [clojure.set :as set]
            [clojure.pprint :as pp]))

(defn- unused-fn [] nil)
(def ^:private unused-var 0)

(defn defs [expr]
  (apply concat
         (when (= :def (:op expr)) [(:var expr)])
         (map defs (analyze/children expr))))

(defn private-defs [expr]
  (filter #(:private (meta %))
          (defs expr)))

(defn var-count [expr]
  (if (= :var (:op expr))
    {(:var expr) 1}
    (apply merge-with +
           (map var-count (analyze/children expr)))))

(defn check-usage-of-private-vars [exprs]
  (let [v-count (apply merge-with + (map var-count exprs))]
    (doseq [pvar (mapcat private-defs exprs)]
      (when-not (get v-count pvar)
        (println "Private variable" pvar "is never used")))))

(comment
(def analyzed
  (doall
    (map analyze/analyze-ns
       '[clojure.test
         clojure.set
         clojure.java.io
         clojure.stacktrace
         clojure.pprint
         clojure.walk
         clojure.string
         clojure.repl
         clojure.core.protocols
         clojure.template
         clojure.core.typed.deps.clojure.jvm.tools.analyzer.examples.privatevars])))

(doseq [exprs analyzed]
  (check-usage-of-private-vars exprs))
  )
