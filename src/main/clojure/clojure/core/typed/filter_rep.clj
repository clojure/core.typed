(ns clojure.core.typed.filter-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require (clojure.core.typed
             [type-rep :as r]
             [path-rep :as pr]
             [utils :as u])
            [clojure.core.typed :as t])
  (:import (clojure.lang Symbol Seqable IPersistentSet)
           (clojure.core.typed.type_rep TCType)
           (clojure.core.typed.path_rep IPathElem)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(t/def-alias NameRef
  "A name for a type variable, either a symbol or a number."
  (U Symbol Number))

(t/ann ^:nocheck name-ref? (predicate NameRef))
(def name-ref? (some-fn symbol? integer?))

(t/ann-protocol IFilter)
(u/defprotocol IFilter)

(t/ann ^:nocheck Filter? (predicate IFilter))
(defn Filter? [a]
  (satisfies? IFilter a))

(t/ann ^:nocheck declare-filter [Class -> Any])
(defn declare-filter [c]
  (extend c IFilter {}))

(t/ann-record BotFilter [])
(u/defrecord BotFilter []
  "Always false proposition"
  [])

(t/ann-record TopFilter [])
(u/defrecord TopFilter []
  "Trivially true proposition"
  [])

(t/ann -top IFilter)
(t/ann -bot IFilter)
(def -top (->TopFilter))
(def -bot (->BotFilter))

(t/ann-record NoFilter [])
(u/defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  [])

(t/ann-record TypeFilter [type :- TCType,
                          path :- (U nil (Seqable IPathElem))
                          id :- NameRef])
(u/defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (not (pr/PathElem? path))
   (name-ref? id)])

(t/ann-record NotTypeFilter [type :- TCType,
                             path :- (Seqable IPathElem)
                             id :- NameRef])
(u/defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (not (pr/PathElem? path))
   (name-ref? id)])

(t/ann-record AndFilter [fs :- (IPersistentSet IFilter)])
(u/defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)])

(t/ann-record OrFilter [fs :- (IPersistentSet IFilter)])
(u/defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)])

(t/ann-record ImpFilter [a :- IFilter
                         c :- IFilter])
(u/defrecord ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)])

(t/ann-record FilterSet [then :- IFilter
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
