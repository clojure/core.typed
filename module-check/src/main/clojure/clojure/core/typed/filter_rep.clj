(ns ^:skip-wiki clojure.core.typed.filter-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.indirect-utils :as ind-u]
            [clojure.core.typed.indirect-ops :as ind]
            [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con])
  (:import (clojure.lang Symbol Seqable IPersistentSet)
           (clojure.core.typed.path_rep IPathElem)))

(t/defalias Filter
  "A filter"
  p/IFilter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(t/defalias NameRef
  "A name for a type variable, either a symbol or a number."
  (t/U Symbol Number))

(t/ann ^:no-check name-ref? (t/Pred NameRef))
(def name-ref? (some-fn symbol? (every-pred integer?
                                            (complement neg?))))

(t/ann ^:no-check Filter? (t/Pred Filter))
(defn Filter? [a]
  (p/IFilter? a))

(u/ann-record BotFilter [])
(u/def-filter BotFilter []
  "Always false proposition"
  []
  :methods
  [p/IFilter])

(u/ann-record TopFilter [])
(u/def-filter TopFilter []
  "Trivially true proposition"
  []
  :methods
  [p/IFilter])

(t/ann -top Filter)
(t/ann -bot Filter)
(def -top (TopFilter-maker))
(def -bot (BotFilter-maker))

(defn -top-fn []
  -top)

(t/tc-ignore
(ind-u/add-indirection ind/-top-fn -top-fn)
)

(u/ann-record NoFilter [])
(u/def-filter NoFilter []
  "Represents no info about filters, used for parsing types"
  []
  :methods
  [p/IFilter])

(t/ann -no-filter Filter)
(def -no-filter (NoFilter-maker))

(u/ann-record TypeFilter [type :- r/Type,
                          path :- (t/U nil (Seqable IPathElem))
                          id :- NameRef])
(u/def-filter TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (pr/path-elems? path)
   (name-ref? id)]
  :methods
  [p/IFilter])

(u/ann-record NotTypeFilter [type :- r/Type,
                             path :- (t/U nil (Seqable IPathElem))
                             id :- NameRef])
(u/def-filter NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (pr/path-elems? path)
   (name-ref? id)]
  :methods
  [p/IFilter])

; id and path should be merged
(defn equal-paths? [f1 f2]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) f1 f2)]
   :post [(con/boolean? %)]}
  (and (= (:id f1) (:id f2))
       (= (:path f1) (:path f2))))

(defn filter-path [f]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) f)]
   :post [(pr/path-elems? %)]}
  (:path f))

(defn filter-id [f]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) f)]
   :post [(name-ref? %)]}
  (:id f))

(u/ann-record AndFilter [fs :- (IPersistentSet Filter)])
(u/def-filter AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)]
  :methods
  [p/IFilter])

(t/ann ^:no-check make-AndFilter [Filter * -> AndFilter])
(defn make-AndFilter [& fs]
  {:pre [(every? Filter? fs)]
   :post [(AndFilter? %)]}
  (AndFilter-maker (set fs)))

(u/ann-record OrFilter [fs :- (IPersistentSet Filter)])
(u/def-filter OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)]
  :methods
  [p/IFilter])

(t/ann make-OrFilter [Filter * -> OrFilter])
(defn make-OrFilter [& fs]
  {:pre [(every? Filter? fs)
         (seq fs)]
   :post [(Filter? %)]}
  (OrFilter-maker (set fs)))

(u/ann-record ImpFilter [a :- Filter
                         c :- Filter])
(u/def-filter ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)]
  :methods
  [p/IFilter])

(u/ann-record FilterSet [then :- Filter
                         else :- Filter])
(u/def-filter FilterSet [then else]
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
  :methods
  [p/IFilter
   p/IFilterSet
   (then-filter [_] then)
   (else-filter [_] else)])
