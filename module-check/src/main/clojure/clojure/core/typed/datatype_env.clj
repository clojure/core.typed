(ns ^:skip-wiki clojure.core.typed.datatype-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t]
            [clojure.core.typed.env :as env]))

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

(def current-datatype-env-kw ::current-datatype-env)

(t/ann ^:no-check datatype-env? [t/Any -> t/Any])
(def datatype-env? 
  (con/hash-c? (every-pred symbol? 
                           (fn [k] (some #{\.} (str k)))) 
               (some-fn r/DataType? r/TypeFn?)))

(defn datatype-env []
  {:post [(map? %)]}
  (get (env/deref-checker) current-datatype-env-kw {}))

(t/ann ^:no-check add-datatype [t/Sym r/Type -> nil])
(defn add-datatype [sym t]
  {:pre [(symbol? sym)
         (r/Type? t)]
   :post [(nil? %)]}
  (env/swap-checker! assoc-in [current-datatype-env-kw sym] t)
  nil)

(t/ann get-datatype [t/Sym -> (t/U nil r/Type)])
(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (get (datatype-env) sym))

(t/ann resolve-datatype [t/Sym -> r/Type])
(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [sym]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (let [d (get-datatype sym)]
    (when-not d 
      (err/int-error (str "Could not resolve DataType: " sym)))
    d))

(t/ann reset-datatype-env! [DataTypeEnv -> DataTypeEnv])
(defn reset-datatype-env! [new-env]
  {:pre [(datatype-env? new-env)]
   :post [(nil? %)]}
  (env/swap-checker! assoc current-datatype-env-kw new-env)
  nil)
