(ns clojure.core.typed.datatype-ancestor-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed :as t :refer [when-let-fail inst]]
            [clojure.set :as set])
  (:import (clojure.lang Symbol)
           (clojure.core.typed.type_rep DataType)))

(t/typed-deps clojure.core.typed.type-ctors
              clojure.core.typed.subst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Aliases

(t/def-alias DTAncestorEnv
  "Environment mapping datatype names to sets of ancestor types."
  (t/Map Symbol (t/Set r/TCType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates

(def ^:no-check ^{:ann '[Any -> Any]}
  dt-ancestor-env? (u/hash-c? symbol? (u/set-c? r/Type?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation specific global state

(t/ann CLJ-DT-ANCESTOR-ENV (t/Atom1 DTAncestorEnv))
(defonce ^:private CLJ-DT-ANCESTOR-ENV ((inst atom DTAncestorEnv) {} :validator dt-ancestor-env?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation agnostic state

(def ^:dynamic ^{:ann '(U nil (t/Atom1 DTAncestorEnv))}
  *current-dt-ancestors* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers

(defn ^:private ^{:ann '[Any -> Any]}
  assert-dt-ancestors []
  (assert *current-dt-ancestors* "No datatype ancestor environment bound"))

(defn ^:private ^{:ann '[DataType (U nil (t/Seqable r/TCType)) -> (t/Set r/TCType)]}
  inst-ancestors
  "Given a datatype, return its instantiated ancestors"
  [{poly :poly? :as dt} anctrs]
  {:pre [(r/DataType? dt)]
   :post [((u/set-c? r/Type?) %)]}
  (let [names (repeatedly (count poly) gensym)
        fs (map r/make-F names)]
    (set (t/for> :- r/TCType
           [u :- Symbol, anctrs]
           (let [t (c/instantiate-many names u)
                 subst (c/make-simple-substitution names poly)]
             (subst/subst-all subst t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defn ^:no-check ^{:ann '[DataType -> (t/Set r/TCType)]}
  get-datatype-ancestors 
  "Returns the set of overriden ancestors of the given DataType."
  [{:keys [poly? the-class] :as dt}]
  {:pre [(r/DataType? dt)]}
  (assert-dt-ancestors)
  (when-let-fail [a *current-dt-ancestors*]
    (inst-ancestors dt (@a the-class))))

(defn ^:no-check ^{:ann '[Symbol (t/Set r/TCType) -> nil]}
  add-datatype-ancestors 
  "Add a set of ancestor overrides for the datatype named sym."
  [sym tset]
  (assert-dt-ancestors)
  (when-let-fail [a *current-dt-ancestors*]
    (swap! a update-in [sym] #(set/union (or % #{}) tset)))
  nil)

(defn ^:no-check ^{:ann '[DTAncestorEnv -> nil]}
  reset-datatype-ancestors! 
  "Reset the current ancestor map."
  [aenv]
  (assert-dt-ancestors)
  (when-let-fail [a *current-dt-ancestors*]
    (reset! a aenv))
  nil)
