;(ns clojure.core.typed.alms-infer)
;  (:refer-clojure :exclude [defrecord])
;  (:require [clojure.core.typed :refer :all]
;            [clojure.core.typed.utils :refer [defrecord]]))
;
;#_(defalias DirectedGraph (IPersistentMap Symbol (IPersistentSet Symbol)))
;#_(defalias Substitution (IPersistentMap Symbol Type))
;
;#_(ann subtyping-graph? (predicate DirectedGraph))
;(def subtyping-graph? (hash-c? symbol? (set-c? symbol)))
;
;#_(ann substitution? (predicate Substitution))
;(def substitution? (hash-c? symbol? Type))
;
;;; Subtyping Graph
;
;#_(ann-record SubtypeGraph [tv-graph :- DirectedGraph
;                            tv-bnds (IPersistentMap Symbol Bounds)])
;(defrecord SubtypeGraph [tv-graph tv-bnds]
;  "A constraint set containing subtyping relationships between type variables"
;  [(subtyping-graph? tv-graph)
;   (every? Bounds? tv-bnds)])
;
;#_(ann add-subtype [DirectedGraph Symbol Symbol -> DirectedGraph])
;(defn add-subtype [g t tsuper]
;  {:pre [(subtyping-graph? g)
;         (symbol? t)
;         (symbol? tsuper)]
;   :post [(subtyping-graph? %)]}
;  (update-in g [t] #(if %
;                      (conj % tsuper)
;                      #{tsuper})))
;
;#_(ann supers-of [DirectedGraph Symbol -> (IPersistentSet Symbol)])
;(defn supers-of [g t]
;  {:pre [(subtyping-graph? g)
;         (symbol? t)]
;   :post [((set-c? symbol) %)]}
;  (or (g t)
;      #{}))
;
;
;;; Constraints
;
;#_(ann-protocol IConstraint
;                upper-bound [IConstraint -> Type]
;                lower-bound [IConstraint -> Type])
;(defprotocol IConstraint
;  (upper-bound [c])
;  (lower-bound [c]))
;
;(defrecord SubConstraint [s t]
;  "A subtyping constraint between two types"
;  [(Type? s)
;   (Type? t)]
;
;  IConstraint
;  (upper-bound [_] t)
;  (lower-bound [_] s))
;
;(defrecord TrivialConstraint [cs]
;  "A conjunction of constraints"
;  [(every? IConstraint? cs)]
;
;(defrecord AndConstraint [cs]
;  "A conjunction of constraints"
;  [(every? IConstraint? cs)]
;
;  IConstraint
;  (upper-bound [_] t)
;  (lower-bound [_] s))
;
;#_
;(ann decompose [DirectedGraph IConstraint -> DirectedGraph])
;(defn decompose [g c]
;  (let [ub (fully-resolve-type (upper-bound c))
;        lb (fully-resolve-type (lower-bound c))]
;    ))
;
;
;
;;; An implementation of Alms-style type inference. 
;;; "Practical Programming with Substructural Types" Section 6.2
;
;;; Graph representation
;
;
;;; Alms subtype types
;;;  Relate two types at either subtyping or equality, depending on
;;;  the value of the first parameter (@True@ means equality).
;;;  This eagerly solves as much as possible, adding to the constraint
;;;  only as necessary.
;
;
;(defn solve-subtype-constraints [])
;
;(defrecord GeneralizeResult [subst d]
;  "The results of generalization: a substition subst and
;  constraint d"
;
;#_(ann generalize SubConstraint
;(defn generalize [c env gt]
