;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.name-env
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.datatype-env :as dtenv]
            [clojure.core.typed.checker.jvm.rclass-env :as rcls]
            [clojure.core.typed.checker.protocol-env :as prenv]
            [clojure.core.typed.checker.declared-kind-env :as kinds]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t]
            [clojure.core.typed.env :as env]))

(t/defalias NameEnv
  "Environment mapping names to types. Keyword values are special."
  (t/Map t/Sym (t/U t/Kw r/Type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(t/ann temp-binding t/Kw)
(def temp-binding ::temp-binding)

(t/tc-ignore
(doseq [k [impl/declared-name-type impl/protocol-name-type impl/datatype-name-type]]
  (derive k temp-binding))
  )

(t/ann ^:no-check name-env? [t/Any -> t/Any])
(def name-env? (con/hash-c? (every-pred (some-fn namespace 
                                                 #(some #{\.} (str %)))
                                        symbol?)
                            (some-fn r/Type? #(isa? % temp-binding))))

(t/ann ^:no-check name-env [-> NameEnv])
(defn name-env []
  (get (env/deref-checker) impl/current-name-env-kw {}))

(t/ann ^:no-check update-name-env! [NameEnv -> nil])
(defn update-name-env! [nme-env]
  (env/swap-checker! update impl/current-name-env-kw
                     (fnil merge {}) nme-env)
  nil)

(t/ann ^:no-check reset-name-env! [NameEnv -> nil])
(defn reset-name-env! [nme-env]
  (env/swap-checker! assoc impl/current-name-env-kw nme-env)
  nil)

(defn merge-name-env! [nme-env]
  {:pre [(map? nme-env)]}
  (env/swap-checker! update impl/current-name-env-kw merge nme-env)
  nil)

(t/ann get-type-name [t/Any -> (t/U nil t/Kw r/Type)])
(defn get-type-name 
  "Return the name with var symbol sym.
  Returns nil if not found."
  [sym]
  {:post [(or (nil? %)
              (keyword? %)
              (r/Type? %))]}
  (force (get (name-env) sym)))

(t/ann ^:no-check add-type-name [t/Sym (t/U t/Kw r/Type) -> nil])
(def add-type-name impl/add-tc-type-name)

(t/ann declare-name* [t/Sym -> nil])
(def declare-name* impl/declare-name*)

(t/ann declared-name? [t/Any -> t/Any])
(defn declared-name? [sym]
  (= impl/declared-name-type (get-type-name sym)))

(t/ann declare-protocol* [t/Sym -> nil])
(def declare-protocol* impl/declare-protocol*)

(t/ann declared-protocol? [t/Any -> t/Any])
(defn declared-protocol? [sym]
  (= impl/protocol-name-type (get-type-name sym)))

(t/ann declare-datatype* [t/Sym -> nil])
(def declare-datatype* impl/declare-datatype*)

(t/ann declared-datatype? [t/Any -> t/Any])
(defn declared-datatype? [sym]
  (= impl/datatype-name-type (get-type-name sym)))

(def ^:private get-jsnominal (delay (impl/dynaload 'clojure.core.typed.jsnominal-env/get-jsnominal)))

(t/ann ^:no-check resolve-name* [t/Sym -> r/Type])
(defn resolve-name* [sym]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (let [t (get-type-name sym)
        tfn ((some-fn dtenv/get-datatype 
                      prenv/get-protocol
                      (impl/impl-case :clojure #(or (rcls/get-rclass %)
                                                    (when (class? (resolve %))
                                                      (c/RClass-of-with-unknown-params %)))
                                      :cljs @get-jsnominal)
                      ; during the definition of RClass's that reference
                      ; themselves in their definition, a temporary TFn is
                      ; added to the declared kind env which is enough to determine
                      ; type rank and variance.
                      kinds/declared-kind-or-nil) 
             sym)]
    (if tfn
      tfn
      (cond
        (= impl/protocol-name-type t) (prenv/resolve-protocol sym)
        (= impl/datatype-name-type t) (dtenv/resolve-datatype sym)
        (= impl/declared-name-type t) (throw (IllegalArgumentException. (str "Reference to declared but undefined name " sym)))
        (r/Type? t) (vary-meta t assoc :source-Name sym)
        :else (err/int-error (str "Cannot resolve name " (pr-str sym)
                                  (when t
                                    (str " (Resolved to instance of)" (pr-str (class t))))))))))
