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

(deftest set-pred-test
  (is-tc-e (let [foo :- (U false nil ':a ':b), :a]
             (if (#{:a :b false nil} foo)
               (ann-form foo (U ':a ':b))
               (ann-form foo (U false nil)))))
  (is-tc-e (let [foo :- (U false nil ':a ':b), :a]
             (when (#{:a :b nil} foo)
               (ann-form foo (U ':a ':b)))))
  (is-tc-e (let [foo :- (U nil ':a ':b), :a]
             (when (#{:a :b nil} foo)
               (ann-form foo (U ':a ':b))))))
