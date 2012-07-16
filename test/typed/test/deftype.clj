(ns typed.test.deftype
  (:require [typed.core :refer [ann-datatype ann tc-ignore ann-protocol]]
            [analyze.core :refer [ast]]))

(ann-protocol MyProtocol
              :methods
              {my-fn (Fn [MyProtocol Number Long -> Number])})
(tc-ignore
(defprotocol MyProtocol
  (my-fn [this arg1 arg2]))
)

(ann-datatype AType [[a :- Number]
                     [b :- Long]]
              :ancestors [MyProtocol])
(deftype AType [a b]
  MyProtocol
  (my-fn [this c d]
    (+ a b c d)))

(ann make-AType [Number Long -> AType])
(defn make-AType [a b]
  (if (= a 1)
    (AType. a b)
    (->AType a b)))
