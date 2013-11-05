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

(ann-protocol short/ShortNs)

;; polymorphic protocols

(ann-protocol [[x :variance :covariant]]
              IFoo
              bar [IFoo -> Number]
              baz [IFoo -> Any])
(defprotocol> IFoo
  (bar [this])
  (baz [this]))

(comment
 (check-ns 'clojure.core.typed.test.protocol)
  )
