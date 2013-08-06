(ns clojure.core.typed.test.unsound-simplify
  (:require [clojure.core.typed :refer [ann-pprotocol ann ann-pdatatype check-ns cf defprotocol>]])
  (:import (clojure.lang Symbol)))

(ann-pprotocol P1 [[a :covariant]]
               get-p1 [(P1 a) -> a])
(defprotocol> P1
  (get-p1 [this]))

(ann-pprotocol P2 [[a :covariant]]
               get-p2 [(P2 a) -> a])
(defprotocol> P2
  (get-p2 [this]))

(ann ^:no-check P1? (predicate (P1 Any)))
(defn P1? [a]
  (satisfies? P1 a))

(ann ^:no-check P2? (predicate (P2 Any)))
(defn P2? [a]
  (satisfies? P2 a))

(ann foo [(U (P1 Number) (P2 Number)) -> (U nil Number)])
(defn foo [p]
  (when (P1? p)
    (get-p1 p)))

(ann-pdatatype T1 [[a :covariant]]
               [p :- a]
               :unchecked-ancestors [(P1 a)])
(deftype T1 [p]
  P1
  (get-p1 [this] p))

(ann-pdatatype T2 [[a :covariant]]
               [p :- a]
               :unchecked-ancestors [(P2 a)])
(deftype T2 [p]
  P2
  (get-p2 [this] p))

(ann-pdatatype T3 [[a :covariant]]
               [p :- a]
               :unchecked-ancestors [(P1 a)
                                     (P2 Symbol)])
(deftype T3 [p]
  P1
  (get-p1 [this] 'a)
  P2
  (get-p2 [this] p))

(foo (->T1 1))
(foo (->T2 2))
(foo (->T3 2))
