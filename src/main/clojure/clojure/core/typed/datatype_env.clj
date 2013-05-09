(ns clojure.core.typed.datatype-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(defonce DATATYPE-ENV (atom {}))
(set-validator! DATATYPE-ENV (u/hash-c? (every-pred symbol? 
                                                    (fn [k] (some #(= \. %) (str k)))) 
                                        r/Type?))

(defn add-datatype [sym t]
  (swap! DATATYPE-ENV assoc sym t)
  nil)

(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  (@DATATYPE-ENV sym))

(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [sym]
  (let [d (get-datatype sym)]
    (assert d (str "Could not resolve DataType: " sym))
    d))
