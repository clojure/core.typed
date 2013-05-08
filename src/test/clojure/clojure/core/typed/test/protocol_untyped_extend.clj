(ns clojure.core.typed.test.protocol-untyped-extend
  (:require [clojure.core.typed :refer [ann-protocol defprotocol> tc-ignore
                                        ann-datatype ann check-ns]]))

(ann-protocol IFoo)
(defprotocol> IFoo
  (bar [this]))

(deftype Bar [])

(tc-ignore
  (extend-protocol IFoo
    Bar
    (bar [b] 'a))
  )

(ann takes-IFoo [IFoo -> Any])
(defn takes-IFoo [f]
  (bar f))

(takes-IFoo (->Bar))
