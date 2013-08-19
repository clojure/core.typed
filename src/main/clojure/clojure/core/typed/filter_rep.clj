(ns ^:skip-wiki clojure.core.typed.filter-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.filter-protocols :refer [IFilter]]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t])
  (:import (clojure.lang Symbol Seqable IPersistentSet)
           (clojure.core.typed.type_rep TCType)
           (clojure.core.typed.path_rep IPathElem)))

(t/def-alias Filter
  "A filter"
  IFilter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(t/def-alias NameRef
  "A name for a type variable, either a symbol or a number."
  (U Symbol Number))

(t/ann ^:no-check name-ref? (predicate NameRef))
(def name-ref? (some-fn symbol? integer?))

(t/ann ^:no-check Filter? (predicate IFilter))
(defn Filter? [a]
  (satisfies? IFilter a))

(t/ann ^:no-check declare-filter [Class -> Any])
(defn declare-filter [c]
  (extend c IFilter {}))

(u/ann-record BotFilter [])
(u/defrecord BotFilter []
  "Always false proposition"
  [])

(u/ann-record TopFilter [])
(u/defrecord TopFilter []
  "Trivially true proposition"
  [])

(t/ann -top IFilter)
(t/ann -bot IFilter)
(def -top (->TopFilter))
(def -bot (->BotFilter))

(u/ann-record NoFilter [])
(u/defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  [])

(u/ann-record TypeFilter [type :- TCType,
                          path :- (U nil (Seqable IPathElem))
                          id :- NameRef])
(u/defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (not (pr/PathElem? path))
   (name-ref? id)])

(u/ann-record NotTypeFilter [type :- TCType,
                             path :- (Seqable IPathElem)
                             id :- NameRef])
(u/defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (not (pr/PathElem? path))
   (name-ref? id)])

(u/ann-record AndFilter [fs :- (IPersistentSet IFilter)])
(u/defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)])

(u/ann-record OrFilter [fs :- (IPersistentSet IFilter)])
(u/defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)])

(u/ann-record ImpFilter [a :- IFilter
                         c :- IFilter])
(u/defrecord ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)])

(u/ann-record FilterSet [then :- IFilter
                         else :- IFilter])
(u/defrecord FilterSet [then else]
  "A set of filters: those true when the expression is a true value, and 
  those when it is a false value."
  [(and (or (BotFilter? then)
            (and (BotFilter? else)
               (TopFilter? then))
            (Filter? then))
        (or (BotFilter? else)
            (and (BotFilter? then)
                 (TopFilter? else))
            (Filter? else)))])

(declare-filter BotFilter)
(declare-filter TopFilter)
(declare-filter NoFilter)
(declare-filter AndFilter)
(declare-filter OrFilter)
(declare-filter TypeFilter)
(declare-filter NotTypeFilter)
(declare-filter ImpFilter)
(declare-filter FilterSet)
