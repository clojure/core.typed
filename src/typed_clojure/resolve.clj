(ns typed-clojure.resolve
  (:require [clojure.core.logic :as l]
            [typed-clojure.types :as t]))

(t/+Fun Integer -> Integer)

(defn check-type []

(comment

(+ 1 1)

(Integer Integer -> _.0)

{a [Number]}
(a a -> a)

(fresh [a]
  (is-of a Number)
  (type-unify [[Integer Integer] Integer] [[a a] a])

(Integer Integer -> _.0)

  ;; Attempt to unify

(inst + Integer Integer)

(t/+Fun Integer -> Integer)

(+T clojure.core/+ :- (Number a) => & [a] -> a)
(+T clojure.core/- :- (Number a) => & [a] -> a)

(+T test-typed-def :- (Number a) => a)
(def test-typed-def
  (let [a (- 2 (+ 1 1))
        b 2]
    (+ a b)))
