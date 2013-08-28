(ns ^:skip-wiki clojure.core.typed.filter-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t])
  (:import (clojure.lang Symbol Seqable IPersistentSet)
           (clojure.core.typed.path_rep IPathElem)))

(t/def-alias Filter
  "A filter"
  p/IFilter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(t/def-alias NameRef
  "A name for a type variable, either a symbol or a number."
  (U Symbol Number))

(t/ann ^:no-check name-ref? (predicate NameRef))
(def name-ref? (some-fn symbol? integer?))

(t/ann ^:no-check Filter? (predicate Filter))
(defn Filter? [a]
  (p/IFilter? a))

(u/ann-record BotFilter [])
(u/defrecord BotFilter []
  "Always false proposition"
  []
  p/IFilter)

(u/ann-record TopFilter [])
(u/defrecord TopFilter []
  "Trivially true proposition"
  []
  p/IFilter)

(t/ann -top Filter)
(t/ann -bot Filter)
(def -top (->TopFilter))
(def -bot (->BotFilter))

(u/ann-record NoFilter [])
(u/defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  []
  p/IFilter)

(u/ann-record TypeFilter [type :- r/Type,
                          path :- (U nil (Seqable IPathElem))
                          id :- NameRef])
(u/defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (not (pr/PathElem? path))
   (name-ref? id)]
  p/IFilter)

(u/ann-record NotTypeFilter [type :- r/Type,
                             path :- (Seqable IPathElem)
                             id :- NameRef])
(u/defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (not (pr/PathElem? path))
   (name-ref? id)]
  p/IFilter)

(u/ann-record AndFilter [fs :- (IPersistentSet Filter)])
(u/defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)]
  p/IFilter)

(u/ann-record OrFilter [fs :- (IPersistentSet Filter)])
(u/defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)]
  p/IFilter)

(u/ann-record ImpFilter [a :- Filter
                         c :- Filter])
(u/defrecord ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)]
  p/IFilter)

(u/ann-record FilterSet [then :- Filter
                         else :- Filter])
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
            (Filter? else)))]
  p/IFilter
  p/IFilterSet
  (then-filter [_] then)
  (else-filter [_] else))
