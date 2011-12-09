(ns clojure-analyzer.test
  (:require [typed-clojure.types :as t])
  (:use [typed-clojure.core :only [T]]))

(T clojure.core/+ :- (t/+fn [t/+number :-> t/+number]))

(T a :- t/+integer)
(defn a [b c]
  (+ b c))

(defn blah2 [a] a)

(defmacro testfn [c]
  `(blah2 ~c))
