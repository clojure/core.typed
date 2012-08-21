(ns typed.test.person
  (:require [typed.core 
             :refer [check-ns ann-datatype
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
