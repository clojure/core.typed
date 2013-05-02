(ns clojure.core.typed.test.protocol-fail
  (:require [clojure.core.typed :refer [ann-protocol typed-deps check-ns ann-datatype]]
            [clojure.core.typed.test.protocol
             :refer [AddProtoc]]))

(typed-deps clojure.core.typed.test.protocol)

(ann-datatype Accumulator [t :- Number])
(deftype Accumulator [t]
  AddProtoc
  (adder [_ i] (Accumulator. (+ t i))))
