(ns typed.test.person
  (:require [typed.core 
             :refer [check-ns cf ann-datatype ann
                     tc-ignore ann-protocol AnyInteger]]))

(ann-protocol Age 
  :methods
  {age [Age -> AnyInteger]})
(tc-ignore
(defprotocol Age
  (age [this]))
)

(ann-datatype Person 
  [(name :- String)
   (age :- AnyInteger)])
(deftype Person [name age]
  Age
  (age [this] age))


(age (Person. "Lucy" 34))

(ann my-apply (All [x y] [[x -> y] x -> y]))
(defn my-apply [f a]
  (f a))

#_(my-apply age nil)
