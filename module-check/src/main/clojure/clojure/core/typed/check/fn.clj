(ns ^:skip-wiki clojure.core.typed.check.fn
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.check.fn-methods :as fn-methods]
            [clojure.core.typed.check.utils :as cu]))

(alter-meta! *ns* assoc :skip-wiki true)

; Can take a CLJ or CLJS function expression.
;
;[FnExpr (Option Type) -> Expr]
(defn check-fn 
  "Check a fn to be under expected and annotate the inferred type"
  [{:keys [methods] :as fexpr} expected]
  {:pre [(r/TCResult? expected)
         (#{:fn} (:op fexpr))]
   :post [(-> % u/expr-type r/TCResult?)
          (vector? (::t/cmethods %))]}
  ;(prn "check-fn" methods)
  (let [cmethodss (fn-methods/check-fn-methods 
                    methods
                    (r/ret-t expected)
                    :self-name (cu/fn-self-name fexpr))
        ;; if we only check a method once, then it's clear we can just use
        ;; the output of type checking the method as the output :method entry.
        ;; Otherwise, it's unclear what to do, just use the original input :method.
        cmethods (mapv (fn [m cms]
                         (if (== 1 (count cms))
                           (first cms)
                           m))
                       methods
                       cmethodss)]
    (assoc fexpr
           :methods cmethods
           ::t/cmethods cmethods
           u/expr-type  (r/ret (r/ret-t expected)
                               (fo/-FS fl/-top fl/-bot)))))
