(ns clojure.core.typed.datatype-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]
            [clojure.core.typed :as t :refer [fn>]])
  (:import (clojure.lang IPersistentMap Symbol)))

(t/typed-deps clojure.core.typed.utils
              clojure.core.typed.type-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/def-alias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (IPersistentMap Symbol r/TCType))

(t/ann DATATYPE-ENV (t/Atom1 DataTypeEnv))
(defonce DATATYPE-ENV (atom {}))
(t/tc-ignore
(set-validator! DATATYPE-ENV (u/hash-c? (every-pred symbol? 
                                                    (fn [k] (some #(= \. %) (str k)))) 
                                        r/Type?))
  )

(t/ann add-datatype [Symbol r/TCType -> nil])
(defn add-datatype [sym t]
  (swap! DATATYPE-ENV (fn> [e :- DataTypeEnv]
                        (assoc e sym t)))
  nil)

(t/ann get-datatype [Symbol -> (U nil r/TCType)])
(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  (@DATATYPE-ENV sym))

(t/ann resolve-datatype [Symbol -> r/TCType])
(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [sym]
  (let [d (get-datatype sym)]
    (assert d (str "Could not resolve DataType: " sym))
    d))
