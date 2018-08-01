(ns clojure.core.typed.check.special.tc-ignore
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.analyzer2 :as ana2]
            [clojure.core.typed.type-rep :as r]))

(defn check-tc-ignore [check expr expected]
  (binding [vs/*current-expr* expr]
    (let [expr (-> expr
                   #_(update :ret ana2/run-passes))]
      (assoc expr
             ::t/tc-ignore true
             u/expr-type (below/maybe-check-below
                           (r/ret r/-any)
                           expected)))))

