(ns typed-clojure.core
  (:require [cljs.compiler :as cljs]))

;; Union Type

(defprotocol ITypePredicate
  (type-predicate [this]))

(deftype Union [types]
  ITypePredicate
  (type-predicate [this] ;TODO
                  ))

(def empty-union (->Union nil))

(defn Un 
  ([] empty-union)
  ([t] t)
  ([t & types]
   (->Union (set (cons t types)))))

(defmacro def-type [name & {:keys [pred]}]
  `(def ~name
     (reify 
       ITypePredicate
       (type-predicate [this] ~pred))))

;; Numeric Types

(def-type Zero 
          :pred #(= 0 %1))
(def-type One 
          :pred #(= 1 %1))

(def-type PositiveInteger
          :pred (comp pos? integer?))

(def-type NegativeInteger
          :pred (comp neg? integer?))

(def -Integer (Un NegativeInteger Zero PositiveInteger))

;; Primitives

(defmacro def-typed [[name _ type] & body]
  `(def ~(with-meta name {::type type})
     ~@body))

(def-typed (a :- -Integer) 
           1)
