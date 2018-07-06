(ns ^:skip-wiki clojure.core.typed.check.fn
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.check.fn-methods :as fn-methods]
            [clojure.core.typed.check.utils :as cu]))

;[FnExpr (Option Type) -> Expr]
(defn check-fn 
  "Check a fn to be under expected and annotate the inferred type"
  [{:keys [methods] :as fexpr} expected]
  {:pre [(r/TCResult? expected)
         (#{:fn} (:op fexpr))]
   :post [(-> % u/expr-type r/TCResult?)
          (vector? (::t/cmethods %))]}
  ;(prn "check-fn" methods)
  (let [{:keys [ifn methods cmethods]}
        (fn-methods/check-fn-methods 
          methods
          (r/ret-t expected)
          :self-name (cu/fn-self-name fexpr))]
    (assoc fexpr
           :methods methods
           ::t/cmethods cmethods
           u/expr-type  (r/ret ifn
                               (fo/-FS fl/-top fl/-bot)))))
