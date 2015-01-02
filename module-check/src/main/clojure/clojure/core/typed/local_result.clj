(ns clojure.core.typed.local-result
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.check.utils :as cu]))

(defn local-result [expr sym expected]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (let [t (var-env/type-of sym)
          obj (obj/-path nil sym)]
      (prs/with-unparse-ns (cu/expr-ns expr)
        (below/maybe-check-below
          (r/ret t 
                 (if (c/overlap t (c/Un r/-nil r/-false))
                   (fo/-FS (fo/-not-filter-at (c/Un r/-nil r/-false) obj)
                           (fo/-filter-at (c/Un r/-nil r/-false) obj))
                   (fo/-true-filter))
                 obj)
          expected)))))
