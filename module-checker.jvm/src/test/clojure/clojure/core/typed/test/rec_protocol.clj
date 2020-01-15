(ns clojure.core.typed.test.rec-protocol
  (:require [clojure.core.typed :as t :refer [ann-protocol ann-datatype check-ns]]))

(t/defprotocol SelfProtocol
  (f1 [this] :- (U nil SelfProtocol)))

;(declare-protocol
;  [[f :variance :covariant]]
;  HOProtocol1)
;
;Foo <: (HOProtocol2 Foo)
;Foo <: (HOProtocol2 Foo)

(ann-protocol [[f :variance :covariant
                :< (HOProtocol2 f)]]
              HOProtocol2)
              ;my-method
;              [(HOProtocol2 Id) -> 
;               (U (HOProtocol1 (TFn [[x :variance :covariant]]
;                                    Integer))
;                  (HOProtocol1 (TFn [[x :variance :covariant]]
;                                    Number)))])

(ann-protocol [[f :variance :covariant
                :< (HOProtocol1 Any)]]
              HOProtocol1)
