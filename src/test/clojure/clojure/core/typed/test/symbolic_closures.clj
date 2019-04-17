(ns clojure.core.typed.test.symbolic-closures
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.core.typed.checker.type-rep :as r]
    [clojure.test :refer :all]))

(when r/enable-symbolic-closures?
  (deftest symbolic-closure-test
    (tc-e (let [f (fn* [x] x)]
            (f 1)))
    (tc-e (let [f (fn* [x] x)]
            (f 1))
          Int)
    (tc-err (let [f (fn* [x] x)]
              (f 1))
            Bool)
    #_
    (tc-e (let [comp (fn* [f g] (fn* [x] (f (g x))))
                f (fn* [x] x)
                g (fn* [y] y)]
            ((comp f g) 1))
          )
    ))
