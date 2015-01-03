(ns clojure.core.typed.local-result
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.debug :refer [dbg]]
            [clojure.core.typed.path-type :as path-type]))

(defn local-result [expr sym expected]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (let [; see if sym is an alias for an object
          ; if not (-id-path sym) is returned
          obj (lex/lookup-alias sym)
          [alias-path alias-id] (cond
                                  (obj/Path? obj) [(:path obj) (:id obj)]
                                  (obj/EmptyObject? obj) [nil sym]
                                  :else (assert "Internal error: unreachable"))
          _ (assert (pr/path-elems? alias-path))
          _ (assert (fr/name-ref? alias-id))
          t (path-type/path-type (var-env/type-of alias-id) alias-path)]
      (prs/with-unparse-ns (cu/expr-ns expr)
        (below/maybe-check-below
          (r/ret t 
                 (if (c/overlap t (c/Un r/-nil r/-false))
                   (fo/-FS (fo/-not-filter-at (c/Un r/-nil r/-false) obj)
                           (fo/-filter-at (c/Un r/-nil r/-false) obj))
                   (fo/-true-filter))
                 obj)
          expected)))))
