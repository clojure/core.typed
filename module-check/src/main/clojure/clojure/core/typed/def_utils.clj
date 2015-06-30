(ns clojure.core.typed.def-utils
  (:refer-clojure :exclude [defrecord defprotocol definterface])
  (:require [clojure.core.typed.deps.clojure.core.contracts.constraints :as contracts]
            [clojure.core :as core]))

(defmacro defrecord [name slots inv-description invariants & etc]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc)))

(defmacro defprotocol [name & args]
  ;only define protocol if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(core/defprotocol ~name ~@args)))
