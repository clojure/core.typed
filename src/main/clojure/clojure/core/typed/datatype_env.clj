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

(defn resolve-datatype [sym]
  (let [d (@DATATYPE-ENV sym)]
    (assert d (str "Could not resolve DataType: " sym))
    d))
