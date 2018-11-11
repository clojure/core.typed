;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.datatype-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t]
            [clojure.core.typed.env :as env]))

(t/ann ^:no-check clojure.core.typed.errors/deprecated-warn [String -> nil])
(t/ann ^:no-check clojure.core.typed.errors/int-error [String -> t/Nothing])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/defalias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (t/Map t/Sym (t/U (t/Delay r/Type) r/Type)))

(defn datatype-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/current-datatype-env-kw {}))

(t/ann ^:no-check add-datatype [t/Sym r/Type -> nil])

(def add-datatype impl/add-datatype)

(t/ann get-datatype [t/Sym -> (t/U nil r/Type)])
(defn get-datatype 
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/DataType? r/TypeFn?) %)]}
  (force (get (datatype-env) sym)))

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
  {:pre [(map? new-env)]
   :post [(nil? %)]}
  (env/swap-checker! assoc impl/current-datatype-env-kw new-env)
  nil)
