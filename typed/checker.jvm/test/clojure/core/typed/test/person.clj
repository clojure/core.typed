(ns clojure.core.typed.test.person
  (:require 
    [clojure.core.typed :refer [check-ns cf ann-datatype ann
                                ann-protocol AnyInteger]
     :as t]))

(t/defprotocol Age
  (age [this] :- Int))

(ann-datatype Person 
  [name :- String 
   age :- AnyInteger])
(deftype Person [name age]
  Age
  (age [this] age))

(age (Person. "Lucy" 34))

(ann my-apply (t/All [x y] [[x -> y] x -> y]))
(defn my-apply [f a]
  (f a))

#_(my-apply age nil)
