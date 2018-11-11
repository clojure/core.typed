(ns clojure.core.typed.test.filter-unit-tests
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.checker.filter-ops :refer :all]
            [clojure.core.typed.checker.path-rep :refer :all]
            [clojure.core.typed.checker.type-ctors :refer :all]))

(deftest refine-branch-test
  (is-tc-e (do
             (defalias M (U '{:a Int}
                            '{:a Sym}))
             (fn [a :- M]
               (if (symbol? (:a a))
                 (ann-form (:a a) Sym)
                 (ann-form (:a a) Int))))))
