(ns ^:skip-wiki clojure.core.typed.cs-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t])
  (:import (clojure.lang IPersistentMap IPersistentSet Symbol Seqable)
           (clojure.core.typed.type_rep Bounds F)))

(u/ann-record t-subst [type :- r/TCType,
                       bnds :- Bounds])
(u/defrecord t-subst [type bnds]
  ""
  [(r/Type? type)
   (r/Bounds? bnds)])

(u/ann-record i-subst [types :- (U nil (Seqable r/TCType))])
(u/defrecord i-subst [types]
  ""
  [(every? r/Type? types)])

(u/ann-record i-subst-starred [types :- (U nil (Seqable r/TCType)),
                               starred :- r/TCType])
(u/defrecord i-subst-starred [types starred]
  ""
  [(every? r/Type? types)
   (r/Type? starred)])

(u/ann-record i-subst-dotted [types :- (U nil (Seqable r/TCType)),
                              dty :- r/TCType,
                              dbound :- F])
(u/defrecord i-subst-dotted [types dty dbound]
  ""
  [(or (nil? types)
       (every? r/Type? types))
   (r/Type? dty)
   (r/F? dbound)])

(t/def-alias SubstRHS
  "The substitution records."
  (U t-subst i-subst i-subst-starred i-subst-dotted))

(t/def-alias SubstMap
  "A substutition map of symbols naming frees to types
  to instantitate them with."
  (IPersistentMap Symbol SubstRHS))

(t/ann ^:no-check subst-rhs? (predicate SubstRHS))
(def subst-rhs? (some-fn t-subst? i-subst? i-subst-starred? i-subst-dotted?))

(t/ann ^:no-check substitution-c? (predicate SubstMap))
(def substitution-c? (u/hash-c? symbol? subst-rhs?))

(u/ann-record c [S :- r/TCType,
                 X :- clojure.lang.Symbol,
                 T :- r/TCType,
                 bnds :- Bounds])
(u/defrecord c [S X T bnds]
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
(u/ann-record dcon [fixed :- (U nil (Seqable c))
                    rest :- (U nil c)])
(u/defrecord dcon [fixed rest]
  ""
  [(every? c? fixed)
   ((some-fn nil? c?) rest)])

(u/ann-record dcon-exact [fixed :- (U nil (Seqable c)),
                          rest :- c])
(u/defrecord dcon-exact [fixed rest]
  ""
  [(every? c? fixed)
   (c? rest)])

(u/ann-record dcon-dotted [fixed :- (U nil (Seqable c)),
                           dc :- c,
                           dbound :- F])
(u/defrecord dcon-dotted [fixed dc dbound]
  ""
  [(every? c? fixed)
   (c? dc)
   (r/F? dbound)])

(t/def-alias DCon (U dcon dcon-exact dcon-dotted))

(t/ann ^:no-check dcon-c? (predicate DCon))
(def dcon-c? (some-fn dcon? dcon-exact? dcon-dotted?))

;; map : hash mapping index variables to dcons
(u/ann-record dmap [map :- (IPersistentMap Symbol DCon)])
(u/defrecord dmap [map]
  ""
  [((u/hash-c? symbol? dcon-c?) map)])

(t/def-alias DelayedCheck
  "A pair of types. The left type must be a subtype
  to the right type at instantiation time."
  '[r/TCType r/TCType])

;  Delayed checks are subtype relationships t1 <: t2 that should be instantiated
;  at the same time as bounds checking. t1 should be a subtype of t2 after instantiating
;  them with the current substitution, otherwise constraint generation should fail.
;  This is useful for types like (I a (Not b)) where it's too hard to use the expression
;  to constrain the type variables.
(u/ann-record cset-entry [fixed :- (IPersistentMap Symbol c),
                          dmap :- dmap,
                          delayed-checks :- (IPersistentSet DelayedCheck)])
(u/defrecord cset-entry [fixed dmap delayed-checks]
  ""
  [((u/hash-c? symbol? c?) fixed)
   (dmap? dmap)
   ((u/set-c? (u/hvector-c? r/Type? r/Type?))
     delayed-checks)])

(t/ann make-cset-entry (Fn [(IPersistentMap Symbol c) -> cset-entry]
                           [(IPersistentMap Symbol c) (U nil dmap) -> cset-entry]
                           [(IPersistentMap Symbol c) (U nil dmap) 
                            (U nil (IPersistentSet DelayedCheck)) -> cset-entry]))
(defn make-cset-entry
  ([fixed] (make-cset-entry fixed nil nil))
  ([fixed dmap] (make-cset-entry fixed dmap nil))
  ([fixed dmap delayed-checks] (->cset-entry fixed
                                             (or dmap (->dmap {}))
                                             (or delayed-checks #{}))))

;; maps is a list of cset-entries, consisting of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(u/ann-record cset [maps :- (U nil (Seqable cset-entry))])
(u/defrecord cset [maps]
  ""
  [(every? cset-entry? maps)])


;widest constraint possible
(t/ann no-constraint [Symbol Bounds -> c])
(defn no-constraint [v bnds]
  {:pre [(symbol? v)
         (r/Bounds? bnds)]}
  (->c (r/Union-maker #{}) v r/-any bnds))

(t/def-alias FreeBnds 
  "A map of free variable names to their bounds."
  (IPersistentMap Symbol Bounds))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(t/ann ^:no-check empty-cset [FreeBnds FreeBnds -> cset])
(defn empty-cset [X Y]
  {:pre [(every? (u/hash-c? symbol? r/Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {})
                         #{})]))
