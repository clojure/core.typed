(ns clojure.core.typed.fold-rep
  (:require [clojure.core.typed.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Folding

(def fold-rhs-default ::fold-rhs)

(defn derive-default [& kws]
  (doseq [kw kws]
    (derive kw fold-rhs-default)))

;1. fold-rhs calls sends
; a. Type to type-rec
; b. Filter to filter-rec
; c. Object to object-rec

;visit a type nested inside ty. Add methods with a mode deriving ::visit-type-default 
(defmulti fold-rhs (fn [mode options ty]
                     [mode (class ty)]))

; fld-fn has type-rec, filter-rec and object-rec in scope
(defmacro add-fold-case [mode ty fld-fn]
  `(defmethod fold-rhs [~mode ~ty]
     ~(symbol (str "fold-rhs " (name mode) ty))
     [mode# options# ty#]
     (u/p ~(keyword (str "fold-" mode) (str ty))
     (let [~'[type-rec filter-rec object-rec pathelem-rec]
           (map #(or (% options#)
                     (partial fold-rhs mode# options#))
                [:type-rec :filter-rec :object-rec :pathelem-rec])]
       (~fld-fn ty# options#)))))

(defmacro add-default-fold-case [ty fld-fn]
  `(add-fold-case fold-rhs-default ~ty ~fld-fn))

(defn sub-pe [st mode]
  #(fold-rhs fold-rhs-default
             {:type-rec st
              :pathelem-rec (sub-pe st mode)}
             %))

(defn sub-f [st mode]
  #(fold-rhs mode
             {:type-rec st
              :filter-rec (sub-f st mode)
              :pathelem-rec (sub-pe st mode)}
             %))

(defn sub-o [st mode]
  #(fold-rhs mode
             {:type-rec st
              :object-rec (sub-o st mode)
              :pathelem-rec (sub-pe st mode)}
             %))
