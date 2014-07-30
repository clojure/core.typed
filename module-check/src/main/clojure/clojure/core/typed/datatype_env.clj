(ns ^:skip-wiki clojure.core.typed.datatype-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t]))

(t/ann ^:no-check clojure.core.typed.errors/deprecated-warn [String -> nil])
(t/ann ^:no-check clojure.core.typed.errors/int-error [String -> t/Nothing])

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/defalias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (t/Map t/Sym r/Type))

(t/ann *current-datatype-env* (t/U nil (t/Atom1 DataTypeEnv)))
(defonce ^:dynamic *current-datatype-env* nil)

(t/ann assert-datatype-env [-> t/Any])
(defn ^:private assert-datatype-env []
  (assert *current-datatype-env* "No datatype env bound"))

(t/ann ^:no-check datatype-env? [t/Any -> t/Any])
(def ^:private datatype-env? 
  (con/hash-c? (every-pred symbol? 
                           (fn [k] (some #{\.} (str k)))) 
               (some-fn r/DataType? r/TypeFn?)))

(t/ann CLJ-DATATYPE-ENV (t/Atom1 DataTypeEnv))
(defonce ^:private CLJ-DATATYPE-ENV (t/atom :- DataTypeEnv, {} :validator datatype-env?))

(t/ann CLJS-DATATYPE-ENV (t/Atom1 DataTypeEnv))
(defonce ^:private CLJS-DATATYPE-ENV (t/atom :- DataTypeEnv, {} :validator datatype-env?))

(t/ann add-datatype [t/Sym r/Type -> nil])
(defn add-datatype [sym t]
  (assert-datatype-env)
  (t/when-let-fail [env *current-datatype-env*]
    (let [swap!' (t/inst swap! DataTypeEnv DataTypeEnv t/Sym r/Type)
          assoc' (t/inst assoc t/Sym r/Type t/Any)]
      (swap!' env assoc' sym t)))
  nil)

(t/ann get-datatype [t/Sym -> (t/U nil r/Type)])
(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  (assert-datatype-env)
  (t/when-let-fail [env *current-datatype-env*]
    (@env sym)))

(t/ann resolve-datatype [t/Sym -> r/Type])
(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [sym]
  (let [d (get-datatype sym)]
    (when-not d 
      (err/int-error (str "Could not resolve DataType: " sym)))
    d))

(t/ann reset-datatype-env! [DataTypeEnv -> DataTypeEnv])
(defn reset-datatype-env! [new-env]
  (assert-datatype-env)
  (t/when-let-fail [a *current-datatype-env*]
    (reset! a new-env)))
