(ns clojure.core.typed.filter-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed
             [type-rep :as r]
             [path-rep :as pr]
             [utils :as u]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(def name-ref? (some-fn symbol? integer?))

(defprotocol IFilter)

(defn Filter? [a]
  (satisfies? IFilter a))

(defn declare-filter [c]
  (extend c IFilter {}))

(u/defrecord BotFilter []
  "Always false proposition"
  [])
(u/defrecord TopFilter []
  "Trivially true proposition"
  [])

(def -top (->TopFilter))
(def -bot (->BotFilter))

(u/defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  [])

(u/defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (name-ref? id)])

(u/defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (every? pr/PathElem? path)
   (name-ref? id)])

(u/defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)])

(u/defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)])

(u/defrecord ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)])

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
