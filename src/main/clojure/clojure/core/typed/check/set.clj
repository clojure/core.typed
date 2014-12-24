(ns clojure.core.typed.check.set
  (:require [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu])
  (:import (clojure.lang PersistentHashSet)))

(defn check-set [check {:keys [items] :as expr} expected]
  (let [cargs (mapv check items)
        ts (map (comp c/fully-resolve-type r/ret-t u/expr-type) cargs)
        res-type (if (every? r/Value? ts)
                   (r/-hset (r/sorted-type-set ts))
                   (impl/impl-case
                     :clojure (c/RClass-of PersistentHashSet [(apply c/Un ts)])
                     :cljs (c/Protocol-of 'cljs.core/ISet [(apply c/Un ts)])))]
    (assoc expr
           :items cargs
           u/expr-type (binding [vs/*current-expr* expr]
                         (below/maybe-check-below
                           (r/ret res-type (fo/-true-filter))
                           expected)))))
