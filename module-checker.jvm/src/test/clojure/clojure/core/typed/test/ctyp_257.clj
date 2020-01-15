(ns clojure.core.typed.test.ctyp-257
  (:require [clojure.test :refer :all]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest empty-intersection-test
  (testing "empty intersection should be Top, not Bottom"
    (is (= (c/make-Intersection []) r/-any))
    (is (= (c/In) r/-any)))
  (testing "intersection containing Bottom is Bottom"
    (is (= (c/In r/-nothing r/-any)
           r/-nothing))))
