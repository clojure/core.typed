(ns ^:skip-wiki clojure.core.typed.datatype-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t :refer [fn> inst when-let-fail]])
  (:import (clojure.lang IPersistentMap Symbol)))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/def-alias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (IPersistentMap Symbol r/Type))

(t/ann *current-datatype-env* (U nil (t/Atom1 DataTypeEnv)))
(defonce ^:dynamic *current-datatype-env* nil)

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

(t/ann add-datatype [Symbol r/Type -> nil])
(defn add-datatype [sym t]
  (assert-datatype-env)
  (when-let-fail [env *current-datatype-env*]
    (let [swap!' (inst swap! DataTypeEnv DataTypeEnv Symbol r/Type)
          assoc' (inst assoc Symbol r/Type Any)]
      (swap!' env assoc' sym t)))
  nil)

(t/ann get-datatype [Symbol -> (U nil r/Type)])
(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  (assert-datatype-env)
  (when-let-fail [env *current-datatype-env*]
    (@env sym)))

(t/ann resolve-datatype [Symbol -> r/Type])
(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [sym]
  (let [d (get-datatype sym)]
    (when-not d 
      (u/int-error (str "Could not resolve DataType: " sym)))
    d))

(t/ann reset-datatype-env! [DataTypeEnv -> DataTypeEnv])
(defn reset-datatype-env! [new-env]
  (assert-datatype-env)
  (when-let-fail [a *current-datatype-env*]
    (reset! a new-env)))
