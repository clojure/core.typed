(ns clojure.core.typed.test.set-test
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest construct-hset
  (is-tc-e #{1 2 3}
           :expected
           (HSet #{1 2 3}))
  (is-tc-e #{1 nil}
           :expected
           (HSet #{nil 1})))

(deftest upcast-hset
  (is-tc-e #{1 2 3}
           :expected
           (Set Num))
  (is-tc-e (conj #{1 2 3} 1)
           :expected
           (Set Num)))
