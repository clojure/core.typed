(ns ^:skip-wiki clojure.core.typed.datatype-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t :refer [fn> inst when-let-fail]])
  (:import (clojure.lang IPersistentMap Symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/def-alias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (IPersistentMap Symbol r/TCType))

(t/ann *current-datatype-env* (U nil (t/Atom1 DataTypeEnv)))
(def ^:dynamic *current-datatype-env* nil)

(t/ann assert-datatype-env [-> Any])
(defn ^:private assert-datatype-env []
  (assert *current-datatype-env* "No datatype env bound"))

(t/ann datatype-env? [Any -> Any])
(def ^:private datatype-env? 
  (u/hash-c? (every-pred symbol? 
                         (fn [k] (some #(= \. %) (str k)))) 
             (some-fn r/DataType? r/TypeFn?)))

(t/ann CLJ-DATATYPE-ENV (t/Atom1 DataTypeEnv))
(defonce ^:private CLJ-DATATYPE-ENV ((inst atom DataTypeEnv) {} :validator datatype-env?))

(t/ann CLJS-DATATYPE-ENV (t/Atom1 DataTypeEnv))
(defonce ^:private CLJS-DATATYPE-ENV ((inst atom DataTypeEnv) {} :validator datatype-env?))

(t/ann add-datatype [Symbol r/TCType -> nil])
(defn add-datatype [sym t]
  (assert-datatype-env)
  (when-let-fail [env *current-datatype-env*]
    (let [swap!' (inst swap! DataTypeEnv DataTypeEnv Symbol r/TCType)
          assoc' (inst assoc Symbol r/TCType Any)]
      (swap!' env assoc' sym t)))
  nil)

(t/ann get-datatype [Symbol -> (U nil r/TCType)])
(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  (assert-datatype-env)
  (when-let-fail [env *current-datatype-env*]
    (@env sym)))

(t/ann resolve-datatype [Symbol -> r/TCType])
(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [sym]
  (let [d (get-datatype sym)]
    (assert d (str "Could not resolve DataType: " sym))
    d))

(t/ann reset-datatype-env! [DataTypeEnv -> DataTypeEnv])
(defn reset-datatype-env! [new-env]
  (assert-datatype-env)
  (when-let-fail [a *current-datatype-env*]
    (reset! a new-env)))
