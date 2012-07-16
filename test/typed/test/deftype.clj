(ns typed.test.deftype
  (:require [typed.core :refer [ann-datatype ann]]))

;(ann-protocol MyProtocol
;              :methods
;              {my-fn (Fn [MyProtocol Number Long -> Number])})
;(defprotocol MyProtocol
;  (my-fn [this arg1 arg2]))

(ann-datatype AType 
              [[a :- Number]
               [b :- Long]])
(deftype AType [a b])

(ann make-AType [Number Long -> AType])
(defn make-AType [a b]
  (if (= a 1)
    (AType. a b)
    (->AType a b)))
