(ns clojure.core.typed.test.filter-simplify
  (:require [clojure.core.typed :as t]))

(t/fn [a :- (t/Nilable (t/NonEmptyVec Number))
       b :- (t/Nilable (t/NonEmptyVec Number))]
  (let [a (seq a)
        b (seq b)
        fa (first a)
        fb (first b)]
    (t/print-env "before")
    (t/print-filterset 
      "and"
      (and a b))))
