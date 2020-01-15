(ns clojure.core.typed.test.protocol-fail
  (:require [clojure.core.typed :refer [ann-datatype] :as t]))

(t/defprotocol AddProtoc
  (adder [this amount :- t/Num] :- t/Num))

(ann-datatype Accumulator [t :- Number])
(deftype Accumulator [t]
  AddProtoc
  (adder [_ i] (Accumulator. (+ t i))))
