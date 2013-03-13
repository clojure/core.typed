(in-ns 'clojure.core.typed)

;; An implementation of Alms-style type inference. 
;; "Practical Programming with Substructural Types" Section 6.2

;; Graph representation


;; Alms subtype types
;;  Relate two types at either subtyping or equality, depending on
;;  the value of the first parameter (@True@ means equality).
;;  This eagerly solves as much as possible, adding to the constraint
;;  only as necessary.

(defn alms-subtype-types [t1 t2 & [unify?]]
  {:pre [(Type? t1)
         (Type? t2)]}
  (letfn [(check [t1 t2]
            {:pre [(Type? t1)
                   (Type? t2)]}

