(ns clojure.core.typed.check.vector
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.filter-ops :as fo]))

(defn check-vector [check {:keys [items] :as expr} & [expected]]
  (let [cargs (mapv check items)
        res-type (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                          :filters (mapv (comp r/ret-f u/expr-type) cargs)
                          :objects (mapv (comp r/ret-o u/expr-type) cargs))
        _ (when (and expected (not (sub/subtype? res-type (r/ret-t expected))))
            (cu/expected-error res-type (r/ret-t expected)))]
    (assoc expr
           :items cargs
           u/expr-type (r/ret res-type (fo/-true-filter)))))
