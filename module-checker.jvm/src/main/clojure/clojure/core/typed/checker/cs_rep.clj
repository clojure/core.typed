;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.cs-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed :as t])
  (:import (clojure.core.typed.checker.type_rep Bounds F)))

(u/ann-record t-subst [type :- r/Type,
                       bnds :- Bounds])
(u/def-type t-subst [type bnds]
  ""
  [(r/Type? type)
   (r/Bounds? bnds)])

(u/ann-record i-subst [types :- (t/U nil (t/Seqable r/Type))])
(u/def-type i-subst [types]
  ""
  [(every? r/Type? types)])

(u/ann-record i-subst-starred [types :- (t/U nil (t/Seqable r/Type)),
                               starred :- r/Type])
(u/def-type i-subst-starred [types starred]
  ""
  [(every? r/Type? types)
   (r/Type? starred)])

(u/ann-record i-subst-dotted [types :- (t/U nil (t/Seqable r/Type)),
                              dty :- r/Type,
                              dbound :- F])
(u/def-type i-subst-dotted [types dty dbound]
  ""
  [(or (nil? types)
       (every? r/Type? types))
   (r/Type? dty)
   (r/F? dbound)])

(t/defalias SubstRHS
  "The substitution records."
  (t/U t-subst i-subst i-subst-starred i-subst-dotted))

(t/defalias SubstMap
  "A substutition map of symbols naming frees to types
  to instantitate them with."
  (t/Map t/Sym SubstRHS))

(t/ann ^:no-check subst-rhs? (t/Pred SubstRHS))
(def subst-rhs? (some-fn t-subst? i-subst? i-subst-starred? i-subst-dotted?))

(t/ann ^:no-check substitution-c? (t/Pred SubstMap))
(def substitution-c? (con/hash-c? symbol? subst-rhs?))

(u/ann-record c [S :- r/Type,
                 X :- clojure.lang.Symbol,
                 T :- r/Type,
                 bnds :- Bounds])
(u/def-type c [S X T bnds]
  "A type constraint on a variable within an upper and lower bound"
  [(r/Type? S)
   (symbol? X)
   (r/Type? T)
   (r/Bounds? bnds)])

;; fixed : Listof[c]
;; rest : option[c]
;; a constraint on an index variable
;; the index variable must be instantiated with |fixed| arguments, each meeting the appropriate constraint
;; and further instantions of the index variable must respect the rest constraint, if it exists
(u/ann-record dcon [fixed :- (t/U nil (t/Seqable c))
                    rest :- (t/U nil c)])
(u/def-type dcon [fixed rest]
  ""
  [(every? c? fixed)
   ((some-fn nil? c?) rest)])

(u/ann-record dcon-exact [fixed :- (t/U nil (t/Seqable c)),
                          rest :- c])
(u/def-type dcon-exact [fixed rest]
  ""
  [(every? c? fixed)
   (c? rest)])

(u/ann-record dcon-dotted [fixed :- (t/U nil (t/Seqable c)),
                           dc :- c,
                           dbound :- F])
(u/def-type dcon-dotted [fixed dc dbound]
  ""
  [(every? c? fixed)
   (c? dc)
   (r/F? dbound)])

; this dcon is used for check prest with drest, because prest will
; have repeat in it, then the dcon must contains it for future check
(u/ann-record dcon-repeat [fixed :- (t/Seqable c)
                           repeat :- (t/NonEmptySeq c)])
(u/def-type dcon-repeat [fixed repeat]
  ""
  [(every? c? fixed)
   (not-empty repeat)
   (every? c? repeat)])

(t/defalias DCon (U dcon dcon-exact dcon-dotted dcon-repeat))

(t/ann ^:no-check dcon-c? (predicate DCon))
(def dcon-c? (some-fn dcon? dcon-exact? dcon-dotted? dcon-repeat?))

;; map : hash mapping index variables to dcons
(u/ann-record dmap [map :- (t/Map t/Sym DCon)])
(u/def-type dmap [map]
  ""
  [((con/hash-c? symbol? dcon-c?) map)])

(t/defalias DelayedCheck
  "A pair of types. The left type must be a subtype
  to the right type at instantiation time."
  '[r/Type r/Type])

(t/defalias CMap
  "Map of free symbols to constraints"
  (t/Map t/Sym c))

(t/defalias DMap
  "Map of dotted free symbols to constraints, wrapped
  in a dmap constructor."
  dmap)

(u/ann-record cset-entry [fixed :- CMap
                          dmap :- DMap])
(u/def-type cset-entry [fixed dmap]
  ""
  [((con/hash-c? symbol? c?) fixed)
   (dmap? dmap)])

(t/ann make-cset-entry (t/IFn [(t/Map t/Sym c) -> cset-entry]
                           [(t/Map t/Sym c) (t/U nil dmap) -> cset-entry]))
(defn make-cset-entry
  ([fixed] (make-cset-entry fixed nil))
  ([fixed dmap] (cset-entry-maker fixed
                              (or dmap (dmap-maker {})))))

;; maps is a list of cset-entries, consisting of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(u/ann-record cset [maps :- (t/U nil (t/Seqable cset-entry))])
(u/def-type cset [maps]
  ""
  [(every? cset-entry? maps)])


;widest constraint possible
(t/ann no-constraint [t/Sym Bounds -> c])
(defn no-constraint [v bnds]
  {:pre [(symbol? v)
         (r/Bounds? bnds)]}
  (c-maker r/-nothing v r/-any bnds))

(t/defalias FreeBnds 
  "A map of free variable names to their bounds."
  (t/Map t/Sym Bounds))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(t/ann ^:no-check empty-cset [FreeBnds FreeBnds -> cset])
(defn empty-cset [X Y]
  {:pre [(every? (con/hash-c? symbol? r/Bounds?) [X Y])]
   :post [(cset? %)]}
  (cset-maker [(cset-entry-maker (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                                 (dmap-maker {}))]))
