(ns clojure.core.typed.test.protocol-untyped-extend
  (:require [clojure.core.typed :refer [ann-protocol tc-ignore
                                        ann-datatype ann check-ns]
             :as t]
            [clojure.core.typed.test.protocol-in-another-ns
             :refer [AnotherNs baz]]))

(t/defprotocol IFoo
  (bar [this]))

(ann-datatype Bar [])
(deftype Bar [])

(extend-protocol IFoo
  Bar
  (bar [b] 'a)

  nil
  (bar [c] 'd))

(ann takes-IFoo [IFoo -> t/Any])
(defn takes-IFoo [f]
  (bar f))

; don't eval; see CLJ-979
(fn [] (takes-IFoo (->Bar)))

;; Annotate protocol in another ns

(ann-protocol clojure.core.typed.test.protocol-in-another-ns/AnotherNs
              baz
              [AnotherNs -> t/Any])

(extend-protocol AnotherNs
  Bar
  (baz [this] nil))

(ann takes-AnotherNs [AnotherNs -> t/Any])
(defn takes-AnotherNs [a]
  (baz a))

; don't eval; see CLJ-979
(fn []
  (takes-AnotherNs (->Bar)))

;; A protocol that extends

;(deftype Pair [a]
