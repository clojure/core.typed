(ns clojure.core.typed.test.protocol-untyped-extend
  (:require [clojure.core.typed :refer [ann-protocol defprotocol> tc-ignore
                                        ann-datatype ann check-ns]]
            [clojure.core.typed.test.protocol-in-another-ns
             :refer [AnotherNs baz]]))

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

;; Annotate protocol in another ns

(ann-protocol clojure.core.typed.test.protocol-in-another-ns/AnotherNs
              baz
              [AnotherNs -> Any])

(tc-ignore
  (extend-protocol AnotherNs
    Bar
    (baz [this] nil))
  )

(ann takes-AnotherNs [AnotherNs -> Any])
(defn takes-AnotherNs [a]
  (baz a))

(takes-AnotherNs (->Bar))
