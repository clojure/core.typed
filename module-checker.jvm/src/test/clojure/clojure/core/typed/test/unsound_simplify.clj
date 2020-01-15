(ns clojure.core.typed.test.unsound-simplify
  (:require [clojure.core.typed :refer [ann ann-datatype check-ns cf]
             :as t])
  (:import (clojure.lang Symbol)))

(t/defprotocol
  [[a :variance :covariant]]
  P1
  (get-p1 [this] :- a))

(t/defprotocol
  [[a :variance :covariant]]
  P2
  (get-p2 [this] :- a))

(ann ^:no-check P1? (t/Pred (P1 t/Any)))
(defn P1? [a]
  (satisfies? P1 a))

(ann ^:no-check P2? (t/Pred (P2 t/Any)))
(defn P2? [a]
  (satisfies? P2 a))

(ann foo [(t/U (P1 Number) (P2 Number)) -> (t/U nil Number)])
(defn foo [p]
  (when (P1? p)
    (get-p1 p)))

(ann-datatype [[a :variance :covariant]]
              T1 
              [p :- a]
              :unchecked-ancestors [(P1 a)])
(deftype T1 [p]
  P1
  (get-p1 [this] p))

(ann-datatype [[a :variance :covariant]]
              T2 
              [p :- a]
              :unchecked-ancestors [(P2 a)])
(deftype T2 [p]
  P2
  (get-p2 [this] p))

(ann-datatype [[a :variance :covariant]]
              T3 
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
