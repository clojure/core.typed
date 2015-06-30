(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.examples.docstring
  "Warns on suspected misplaced docstrings in function definitions.
  Entry point `find-and-check-defs`"
  (:require [clojure.core.typed.deps.clojure.jvm.tools.analyzer :as analyze]))

(defn check-def [exp]
  (when (= :fn-expr (-> exp :init :op))
    (doseq [method (-> exp :init :methods)]
      (let [body (:body method)]
        (when (and (= :do (:op body))
                   (< 1 (count (-> body :exprs))))
          (let [first-exp (-> body :exprs first)]
            (when (= :string (:op first-exp))
              (binding [*out* *err*]
                (println "WARNING: Suspicious string, possibly misplaced docstring," (-> exp :var))))))))))

(defn find-and-check-defs [exp]
  (when (= :def (:op exp))
    (check-def exp))
  (doseq [child-exp (analyze/children exp)]
    (find-and-check-defs child-exp)))

;; Examples

;; Check a good chunk of the core library

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
              (repeat '{:children true}))))

(doseq [exprs analyzed
        exp exprs]
  (find-and-check-defs exp))

;; One form at a time

  (find-and-check-defs
    (analyze/analyze-one {:ns {:name 'clojure.repl} :context :eval}
                         '(defn a []
                            "asdf"
                            (+ 1 1))
                         {:children true}))
  )
