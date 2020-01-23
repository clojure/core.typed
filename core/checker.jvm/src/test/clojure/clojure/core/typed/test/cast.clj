(ns clojure.core.typed.test.cast
  (:refer-clojure :exclude [update cast])
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [clojure.core.typed :as tc :refer [Int cast]]
    ))

(deftest typed-cast-test
  (is-tc-e (cast Int 1) Int)
  (is-tc-err #(cast Int (inc 'a)))
  (is-tc-e (cast [Int -> Int] identity) [Int -> Int])
  (is-tc-e (cast '{:a Int} {:a 1}) '{:a Int})
  (is-tc-e #(cast Int nil) [:-> Int])
  ;; runtime errors
  (is (thrown? Exception (tc/check-form-info `(tc/cast tc/Int nil))))
  (is (thrown? Exception
               (tc-e (cast '{:a Int} {:a nil}))))
  (is (thrown? Exception
               (tc-e
                 ((:a (cast '{:a [Int :-> Int]} {:a str}))
                  1))))
  )
