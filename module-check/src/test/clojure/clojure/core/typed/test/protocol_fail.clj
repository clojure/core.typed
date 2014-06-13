(ns clojure.core.typed.test.protocol-fail
  (:require [clojure.core.typed :refer [ann-protocol typed-deps check-ns ann-datatype] :as t]))

(ann-protocol AddProtoc
              adder [AddProtoc Number -> Number])
(t/defprotocol> AddProtoc
  (adder [this amount]))

(ann-datatype Accumulator [t :- Number])
(deftype Accumulator [t]
  AddProtoc
  (adder [_ i] (Accumulator. (+ t i))))
