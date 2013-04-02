(ns clojure.core.typed.datatype-ancestor-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]
            [clojure.set :as set]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DataType Ancestor Env

(defonce DATATYPE-ANCESTOR-ENV (atom {}))
(set-validator! DATATYPE-ANCESTOR-ENV (u/hash-c? (every-pred symbol? #(some #{\.} (str %)))
                                                 (u/set-c? r/Type?)))

(defn add-datatype-ancestors [sym tset]
  (swap! DATATYPE-ANCESTOR-ENV update-in [sym] #(set/union (or % #{}) tset))
  nil)
