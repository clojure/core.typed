(ns clojure.core.typed.check.monitor
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.check-below :as below]))

(defn check-monitor
  "monitor-enter and monitor-exit both take any object and return nil"
  [check {:keys [target] :as expr} expected]
  {:pre [((some-fn nil? r/TCResult?) expected)]}
  (binding [vs/*current-expr* expr]
    (assoc expr
           :target (check target (r/ret (c/RClass-of Object)))
           u/expr-type (below/maybe-check-below (r/ret r/-nil (fo/-false-filter))
                                                expected))))
