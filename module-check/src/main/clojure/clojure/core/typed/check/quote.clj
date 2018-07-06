(ns clojure.core.typed.check.quote
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.check.const :as const]))

(defn check-quote [check constant-type {:keys [expr] :as quote-expr} expected]
  (let [cexpr (const/check-const constant-type true expr expected)]
    (assoc quote-expr
           :expr cexpr
           u/expr-type (u/expr-type cexpr))))
