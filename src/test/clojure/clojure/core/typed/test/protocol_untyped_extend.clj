(ns clojure.core.typed.test.protocol-untyped-extend
  (:require [clojure.core.typed :refer [ann-protocol defprotocol> tc-ignore
                                        ann-datatype ann check-ns]]))

(ann-protocol IFoo
              bar
              [IFoo -> Any])
(defprotocol> IFoo
  (bar [this]))

(ann-datatype Bar [])
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
