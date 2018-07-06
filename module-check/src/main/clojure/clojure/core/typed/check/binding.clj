(ns clojure.core.typed.check.binding
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.utils :as u]))

(defn check-binding
  [check {:keys [init] :as expr} expected]
  (let [cinit (binding [vs/*current-expr* init]
                (check init expected))]
    (assoc expr
           :init cinit
           u/expr-type (u/expr-type cinit))))
