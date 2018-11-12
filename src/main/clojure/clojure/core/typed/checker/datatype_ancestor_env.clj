;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.datatype-ancestor-env
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.subst :as subst]
            [clojure.core.typed :as t]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.checker.nilsafe-utils :as nilsafe]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set])
  (:import (clojure.core.typed.checker.type_rep DataType)))

(t/typed-deps clojure.core.typed.checker.type-ctors
              clojure.core.typed.checker.subst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Aliases

(t/defalias DTAncestorEnv
  "Environment mapping datatype names to sets of ancestor types."
  (t/Map t/Sym (t/Set r/ScopedType)))

(def tmap? (con/hash-c? any? (some-fn delay? r/Scope? r/Type?)))
(def dt-ancestor-env? (con/hash-c? symbol? tmap?))

(t/ann ^:no-check inst-ancestors [DataType (t/U nil (t/Seqable r/Type)) -> (t/Set r/Type)])
(defn inst-ancestors
  "Given a datatype, return its instantiated ancestors"
  [{poly :poly? :as dt} anctrs]
  {:pre [(r/DataType? dt)
         ((some-fn nil? map?) anctrs)]
   :post [((con/set-c? r/Type?) %)]}
  (into #{}
        (map (fn [[_ t]]
               (c/inst-and-subst (force t) poly)))
        anctrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defn all-dt-ancestors []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/current-dt-ancestors-kw {}))

(t/ann ^:no-check get-datatype-ancestors [DataType -> (t/Set r/Type)])
(defn get-datatype-ancestors 
  "Returns the set of overriden ancestors of the given DataType."
  [{:keys [poly? the-class] :as dt}]
  {:pre [(r/DataType? dt)]}
  (let [as (get (all-dt-ancestors) the-class)]
    (inst-ancestors dt as)))

(t/ann ^:no-check add-datatype-ancestors [t/Sym (t/Map t/Any (t/U (t/Delay r/Type) r/Type)) -> nil])
(def add-datatype-ancestors impl/add-datatype-ancestors)

(t/ann ^:no-check reset-datatype-ancestors! [DTAncestorEnv -> nil])
(defn reset-datatype-ancestors! 
  "Reset the current ancestor map."
  [aenv]
  {:pre [(dt-ancestor-env? aenv)]}
  (env/swap-checker! assoc impl/current-dt-ancestors-kw aenv)
  nil)
