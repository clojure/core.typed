(ns clojure.core.typed.datatype-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t :refer [fn> inst]])
  (:import (clojure.lang IPersistentMap Symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/def-alias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (IPersistentMap Symbol r/TCType))

(t/ann *current-datatype-env* (U nil DataTypeEnv))
(def ^:dynamic *current-datatype-env* nil)

(t/ann assert-datatype-env [-> Any])
(defn assert-datatype-env []
  (assert *current-datatype-env* "No datatype env bound"))

(t/ann ^:no-check datatype-env? [Any -> Any])
(def datatype-env? (u/hash-c? (every-pred symbol? 
                                          (fn [k] (some #(= \. %) (str k)))) 
                              (some-fn r/DataType? r/TypeFn?)))

(t/ann DATATYPE-ENV (t/Atom1 DataTypeEnv))
(defonce DATATYPE-ENV ((inst atom DataTypeEnv) {} :validator datatype-env?))

(t/ann add-datatype [Symbol r/TCType -> nil])
(defn add-datatype [sym t]
  ((inst swap! DataTypeEnv DataTypeEnv)
   DATATYPE-ENV (fn> [e :- DataTypeEnv]
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
