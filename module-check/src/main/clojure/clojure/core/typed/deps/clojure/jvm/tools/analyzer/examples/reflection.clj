(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.examples.reflection
  "Same as *warn-on-reflection*"
  (:require [clojure.core.typed.deps.clojure.jvm.tools.analyzer :as analyze]))

(defn check-new [exp]
  (when (not (:ctor exp))
    (println "WARNING: Unresolved constructor" (:class exp) (-> exp :env :ns :name))))

(defn check-static-method [exp]
  (when (not (:method exp))
    (println "WARNING: Unresolved static method" (:method-name exp) (:class exp) (-> exp :env :ns :name))))

(defn check-instance-method [exp]
  (when (not (:method exp))
    (println "WARNING: Unresolved instance method" (:method-name exp) (:class exp) (-> exp :env :ns :name))))

(defn check-static-field [exp]
  (when (not (:field exp))
    (println "WARNING: Unresolved static field" (:field-name exp) (:class exp) (-> exp :env :ns :name))))

(defn check-instance-field [exp]
  (when (not (:field exp))
    (println "WARNING: Unresolved instance field" (:field-name exp) (:class exp) (-> exp :env :ns :name))))


(defn check-for-reflection [exp]
  (condp = (:op exp)
    :new (check-new exp)
    :static-method (check-static-method exp)
    :instance-method (check-instance-method exp)
    :static-field (check-static-field exp)
    :instance-field (check-instance-field exp)
    nil)

  (doseq [c (analyze/children exp)]
    (check-for-reflection c)))

(comment

(def analyzed
  (doall (map analyze/analyze-ns
              '[clojure.test
                clojure.set
                clojure.java.io
                clojure.stacktrace
                clojure.pprint
                clojure.walk
                clojure.string
                clojure.repl
                clojure.core.protocols
                clojure.template]
              (repeat {:children true}))))

(doseq [exprs analyzed
        exp exprs]
  (check-for-reflection exp))

(analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                     '(Integer. (+ 1 1))
                     {:children true})
(analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                     '(Integer. (+ 1 1))
                     {:children true})
(analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                     '(Integer. (+ 1 (even? 1)))
                     {:children true})
)
