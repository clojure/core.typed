(ns clojure.core.typed.test.protocol
  (:require [clojure.core.typed :refer [ann-protocol ann-datatype defprotocol> check-ns]]))

(ann-protocol AddProtoc
              adder [AddProtoc Number -> Number])
(defprotocol> AddProtoc
  (adder [this amount]))

(ann-datatype Accumulator [t :- Number])
(deftype Accumulator [t]
  AddProtoc
  (adder [_ i] 1))
