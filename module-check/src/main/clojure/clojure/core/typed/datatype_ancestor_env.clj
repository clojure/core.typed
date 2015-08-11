(ns ^:skip-wiki clojure.core.typed.datatype-ancestor-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed :as t]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.nilsafe-utils :as nilsafe]
            [clojure.set :as set])
  (:import (clojure.core.typed.type_rep DataType)))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

(t/typed-deps clojure.core.typed.type-ctors
              clojure.core.typed.subst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Aliases

(t/defalias DTAncestorEnv
  "Environment mapping datatype names to sets of ancestor types."
  (t/Map t/Sym (t/Set r/ScopedType)))

(def tset? (con/set-c? (some-fn r/Scope? r/Type?)))
(def dt-ancestor-env? (con/hash-c? symbol? tset?))

(def current-dt-ancestors-kw ::current-dt-ancestors)

(t/ann ^:no-check inst-ancestors [DataType (t/U nil (t/Seqable r/Type)) -> (t/Set r/Type)])
(defn inst-ancestors
  "Given a datatype, return its instantiated ancestors"
  [{poly :poly? :as dt} anctrs]
  {:pre [(r/DataType? dt)]
   :post [((con/set-c? r/Type?) %)]}
  (into #{}
        (map #(c/inst-and-subst % poly))
        anctrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defn all-dt-ancestors []
  {:post [(map? %)]}
  (get (env/deref-checker) current-dt-ancestors-kw {}))

(t/ann ^:no-check get-datatype-ancestors [DataType -> (t/Set r/Type)])
(defn get-datatype-ancestors 
  "Returns the set of overriden ancestors of the given DataType."
  [{:keys [poly? the-class] :as dt}]
  {:pre [(r/DataType? dt)]}
  (inst-ancestors dt (get (all-dt-ancestors) the-class)))

(t/ann ^:no-check add-datatype-ancestors [t/Sym (t/Set r/Type) -> nil])
(defn add-datatype-ancestors 
  "Add a set of ancestor overrides for the datatype named sym."
  [sym tset]
  {:pre [(symbol? sym)
         (tset? tset)]
   :post [(nil? %)]}
  (env/swap-checker! update-in [current-dt-ancestors-kw sym] nilsafe/set-union tset)
  nil)

(t/ann ^:no-check reset-datatype-ancestors! [DTAncestorEnv -> nil])
(defn reset-datatype-ancestors! 
  "Reset the current ancestor map."
  [aenv]
  {:pre [(dt-ancestor-env? aenv)]}
  (env/swap-checker! assoc current-dt-ancestors-kw aenv)
  nil)
