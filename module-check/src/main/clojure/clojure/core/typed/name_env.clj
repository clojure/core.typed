(ns ^:skip-wiki clojure.core.typed.name-env
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.datatype-env :as dtenv]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.core.typed.protocol-env :as prenv]
            [clojure.core.typed.declared-kind-env :as kinds]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :as t]
            [clojure.core.typed.env :as env]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

(t/defalias NameEnv
  "Environment mapping names to types. Keyword values are special."
  (t/Map t/Sym (t/U t/Kw r/Type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(t/ann-many t/Kw 
            declared-name-type 
            protocol-name-type 
            datatype-name-type)

(def declared-name-type ::declared-name)
(def protocol-name-type ::protocol-name)
(def datatype-name-type ::datatype-name)

(t/ann temp-binding t/Kw)
(def temp-binding ::temp-binding)

(t/tc-ignore
(doseq [k [declared-name-type protocol-name-type datatype-name-type]]
  (derive k temp-binding))
  )

(t/ann ^:no-check name-env? [t/Any -> t/Any])
(def name-env? (con/hash-c? (every-pred (some-fn namespace 
                                                 #(some #{\.} (str %)))
                                        symbol?)
                            (some-fn r/Type? #(isa? % temp-binding))))

(def current-name-env-kw ::current-name-env)

(t/ann ^:no-check name-env [-> NameEnv])
(defn name-env []
  (get (env/deref-checker) current-name-env-kw {}))

(t/ann ^:no-check update-name-env! [NameEnv -> nil])
(defn update-name-env! [nme-env]
  (env/swap-checker! update current-name-env-kw
                     (fnil merge {}) nme-env)
  nil)

(t/ann ^:no-check reset-name-env! [NameEnv -> nil])
(defn reset-name-env! [nme-env]
  (env/swap-checker! assoc current-name-env-kw nme-env)
  nil)

(t/ann get-type-name [t/Any -> (t/U nil t/Kw r/Type)])
(defn get-type-name 
  "Return the name with var symbol sym.
  Returns nil if not found."
  [sym]
  (get (name-env) sym))

(t/ann ^:no-check add-type-name [t/Sym (t/U t/Kw r/Type) -> nil])
(defn add-type-name [sym ty]
  (env/swap-checker! assoc-in
                     [current-name-env-kw sym]
                     (if (r/Type? ty)
                       (vary-meta ty assoc :from-name sym)
                       ty))
  nil)

(t/ann declare-name* [t/Sym -> nil])
(defn declare-name* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-type-name sym declared-name-type)
  nil)

(t/ann declared-name? [t/Any -> t/Any])
(defn declared-name? [sym]
  (= declared-name-type (get-type-name sym)))

(t/ann declare-protocol* [t/Sym -> nil])
(defn declare-protocol* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-type-name sym protocol-name-type)
  nil)

(t/ann declared-protocol? [t/Any -> t/Any])
(defn declared-protocol? [sym]
  (= protocol-name-type (get-type-name sym)))

(t/ann declare-datatype* [t/Sym -> nil])
(defn declare-datatype* [sym]
  (add-type-name sym datatype-name-type)
  nil)

(t/ann declared-datatype? [t/Any -> t/Any])
(defn declared-datatype? [sym]
  (= datatype-name-type (get-type-name sym)))

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
                                      :cljs jsnom/get-jsnominal)
                      ; during the definition of RClass's that reference
                      ; themselves in their definition, a temporary TFn is
                      ; added to the declared kind env which is enough to determine
                      ; type rank and variance.
                      kinds/declared-kind-or-nil) 
             sym)]
    (if tfn
      tfn
      (cond
        (= protocol-name-type t) (prenv/resolve-protocol sym)
        (= datatype-name-type t) (dtenv/resolve-datatype sym)
        (= declared-name-type t) (throw (IllegalArgumentException. (str "Reference to declared but undefined name " sym)))
        (r/Type? t) (vary-meta t assoc :source-Name sym)
        :else (err/int-error (str "Cannot resolve name " (pr-str sym)
                                  (when t
                                    (str " (Resolved to instance of)" (pr-str (class t))))))))))
