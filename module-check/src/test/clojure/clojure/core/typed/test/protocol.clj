(ns clojure.core.typed.test.protocol
  (:require [clojure.core.typed :as t :refer [ann-protocol ann-datatype defprotocol> check-ns]]))

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
              bar [(IFoo x) -> Number]
              baz [(IFoo x) -> t/Any])
(defprotocol> IFoo
  (bar [this])
  (baz [this]))

;TODO
;(ann-datatype FooPoly []
;              :unchecked-ancestors
;              [(IFoo Any)])
;(deftype FooPoly []
;  IFoo
;  (bar [_] 1))

;#(bar (FooPoly.))
;
;;; extend a polymophic protocol after deftype definition
;
;(ann-datatype FooLatePoly [])
;(deftype FooLatePoly [])
;
;(extend-type FooLatePoly
;  IFoo
;  (bar [this] 1))

(comment
 (check-ns 'clojure.core.typed.test.protocol)
  )
