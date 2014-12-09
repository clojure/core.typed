(ns clojure.core.typed.check.monitor
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]))

(defn check-monitor
  "monitor-enter and monitor-exit both take any object and return nil"
  [check {:keys [target] :as expr} expected]
  {:pre [(or (nil? expected) (r/TCResult? expected))]}
  ; We're going to return nil
  (let [t (r/ret r/-nil)]
    ; We need to verify that returning nil is OK; e.g. we expect nil.
    (when (r/TCResult? expected)
      (when-not (sub/subtype? t (r/ret-t expected))
        (cu/expected-error t (r/ret-t expected))))

    (assoc expr
           :target (check target (r/ret (c/RClass-of Object)))
           u/expr-type (r/ret r/-nil))))
