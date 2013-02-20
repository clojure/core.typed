(ns typed.test.person
  (:require 
    [typed.core :refer [check-ns cf ann-datatype ann
                        ann-protocol AnyInteger defprotocol>]]))

(ann-protocol Age 
  age [Age -> AnyInteger])
(defprotocol> Age
  (age [this]))

(ann-datatype Person 
  [name :- String 
   age :- AnyInteger])
(deftype Person [name age]
  Age
  (age [this] age))

(age (Person. "Lucy" 34))

(ann my-apply (All [x y] [[x -> y] x -> y]))
(defn my-apply [f a]
  (f a))

#_(my-apply age nil)
