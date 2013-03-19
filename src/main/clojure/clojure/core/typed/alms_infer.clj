(ns clojure.core.typed.alms-infer
  (:require [clojure.core.typed]

;(def-alias DirectedGraph (IPersistentMap Symbol (IPersistentSet Symbol)))
;
; Each symbol->#{symbol} refers to supertypes 

;(ann add-subtype [DirectedGraph Symbol Symbol -> DirectedGraph])
(defn add-subtype [g t tsuper]
  {:pre [((hash-c? symbol? (set-c? symbol)) g)
         (symbol? t)
         (symbol? tsuper)]
   :post [((hash-c? symbol? (set-c? symbol)) %)]}
  (update-in g [t] #(if %
                      (conj % tsuper)
                      #{tsuper})))

;(ann supers-of [DirectedGraph Symbol -> (IPersistentSet Symbol)])
(defn supers-of [g t]
  {:pre [((hash-c? symbol? (set-c? symbol)) g)
         (symbol? t)]
   :post [((set-c? symbol) %)]}
  (or (g t)
      #{}))


(defn decompose [

;; An implementation of Alms-style type inference. 
;; "Practical Programming with Substructural Types" Section 6.2

;; Graph representation


;; Alms subtype types
;;  Relate two types at either subtyping or equality, depending on
;;  the value of the first parameter (@True@ means equality).
;;  This eagerly solves as much as possible, adding to the constraint
;;  only as necessary.


