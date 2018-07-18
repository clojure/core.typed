(ns clojure.core.typed.check.with-meta
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]))

(defn check-with-meta
  [check {:keys [expr meta] :as with-meta-expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [cexpr (check expr expected)
        cmeta (check meta)]
    (assoc with-meta-expr 
           :expr cexpr
           :meta cmeta
           u/expr-type (u/expr-type cexpr))))
