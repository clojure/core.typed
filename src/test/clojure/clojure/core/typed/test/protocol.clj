(ns clojure.core.typed.test.protocol
  (:require [clojure.core.typed :as t :refer [ann-protocol ann-datatype check-ns]]))

(t/defprotocol AddProtoc
  (adder [this amount :- t/Num] :- t/Num))

(ann-datatype Accumulator [t :- Number])
(deftype Accumulator [t]
  AddProtoc
  (adder [_ i] 1))

(ann-protocol short/ShortNs)

;; polymorphic protocols

(t/defprotocol
  [[x :variance :covariant]]
  IFoo
  (bar [this] :- t/Num)
  (baz [this] :- t/Any))

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

; annotate first position in methods
(t/defalias FunctorLB
  (t/TFn [[x :variance :covariant]]
         t/Nothing))

(t/defprotocol [[F :variance :covariant, 
               :> FunctorLB
               :< Functor]]
  Functor
  ([a b c ...] fmap [fv :- (F a), g :- [a :-> b]] :- (F b)))

(comment
 (check-ns 'clojure.core.typed.test.protocol)
  )
