(ns typed-clojure.types
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(comment

;; # Polymorphic Types

;; Declare a protocol/class as polymorphic
;(defpoly clojure.lang.ISeq [t])

;; use `poly` to instantiate a concrete type from a polymorphic type
(poly ISeq Integer)

;(: map (all [a b]
;            ((a -> b) (poly ISeq a)
;             -> (poly ISeq b))))

;; # Propositions

;(: instance? (all [a b]
;                  (isa Class a) ;; class constraint
;                  (a b -> Boolean : (impl-filter {:object b ;; then proposition
;                                                  :isa a}
;                                                 {:object b ;; else proposition
;                                                  :isnota a}))))

  )
