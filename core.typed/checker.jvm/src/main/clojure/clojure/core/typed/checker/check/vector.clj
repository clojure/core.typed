;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.vector
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.filter-ops :as fo]))

(defn check-vector [check {:keys [items] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:items %))]}
  (let [cargs (mapv check items)
        res-type (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                          :filters (mapv (comp r/ret-f u/expr-type) cargs)
                          :objects (mapv (comp r/ret-o u/expr-type) cargs))]
    (assoc expr
           :items cargs
           u/expr-type (below/maybe-check-below
                         (r/ret res-type (fo/-true-filter))
                         expected))))
