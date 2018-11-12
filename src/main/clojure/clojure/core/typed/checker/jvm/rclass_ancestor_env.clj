;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki 
  clojure.core.typed.checker.jvm.rclass-ancestor-env
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.subst :as subst]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.set :as set])
  (:import [clojure.core.typed.checker.type_rep RClass]))

(t/defalias RClassAncestorEnv 
  (t/Map t/Symbol '{:replacements (t/Map t/Symbol r/MaybeScopedType)
                    :ancestors (t/SortedSet r/ScopedType)}))

(t/ann ^:no-check rclass-ancestor-env? (t/Pred RClassAncestorEnv))
(def rclass-ancestor-env? 
  (con/hash-c? symbol? (con/hmap-c? :replacements (con/hash-c? symbol? r/scoped-Type?)
                                    :ancestors (con/sorted-set-c? r/scoped-Type?))))

(t/ann initial-class-ancestors-env RClassAncestorEnv)
(def initial-class-ancestors-env {})

(t/ann ^:no-check RCLASS-ANCESTORS-ENV (t/Atom1 RClassAncestorEnv))
(defonce RCLASS-ANCESTORS-ENV (atom initial-class-ancestors-env
                                    :validator 
                                    (fn [e]
                                      (if (rclass-ancestor-env? e)
                                        true
                                        (assert nil (pr-str e))))))

(defn reset-rclass-ancestors-env! []
  (reset! RCLASS-ANCESTORS-ENV initial-class-ancestors-env))

(t/ann ^:no-check rclass-ancestors [RClass -> (t/SortedSet r/Type)])
(defn rclass-ancestors [{poly :poly?, rsym :the-class, :as rcls}]
  {:pre [(r/RClass? rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  (let [names (repeatedly (count poly) #(gensym "unchecked-ancestor"))
        fs (map r/make-F names)
        abstract-as (get-in @RCLASS-ANCESTORS-ENV [rsym :ancestors])]
    (r/sorted-type-set
      (for [u abstract-as]
        (let [t (c/instantiate-many names u)
              subst (c/make-simple-substitution names poly)]
          (subst/subst-all subst t))))))

(t/ann ^:no-check rclass-replacements [RClass -> (t/Seqable t/Symbol r/Type)])
(defn rclass-replacements [{poly :poly?, rsym :the-class, :as rcls}]
  {:pre [(r/RClass? rcls)]
   :post [((con/hash-c? symbol? r/Type?) %)]}
  (let [abstract-repls (get-in @RCLASS-ANCESTORS-ENV [rsym :replacements])]
    (into {} (for [[k v] abstract-repls]
               [k (c/inst-and-subst v poly)]))))

(t/ann ^:no-check add-rclass-ancestors [RClass (t/Seqable r/Type) -> nil])
(defn add-rclass-ancestors [rsym names as]
  {:pre [(symbol? rsym)]}
  (let [nas (r/sorted-type-set
              (for [u as]
                (c/abstract-many names u)))]
    (swap! RCLASS-ANCESTORS-ENV
           (fn [env]
             (if (contains? env rsym)
               (update-in env [rsym :ancestors]
                          #(set/union nas (or % #{})))
               (assoc env rsym {:ancestors nas
                                :replacements {}}))))
    nil))

(t/ann ^:no-check add-rclass-replacements [RClass (t/Map t/Symbol r/Type) -> nil])
(defn add-rclass-replacements [rsym names as]
  {:pre [(symbol? rsym)]}
  (let [nrp (into {}
                  (for [[k v] as]
                    [k (c/abstract-many names v)]))]
    (swap! RCLASS-ANCESTORS-ENV
           (fn [e]
             (if (contains? e rsym)
               (update-in e [rsym :replacements]
                          merge nrp)
               (assoc e rsym {:ancestors (r/sorted-type-set #{})
                              :replacements nrp}))))
    nil))
