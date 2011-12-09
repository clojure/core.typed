(ns clojure-analyzer.test
  (:import (clojure.lang RT))
  (:require [typed-clojure.types :as t])
  (:use [typed-clojure.core :only [T]]))

;(T clojure.core/+ :- (t/+fn [t/+number :-> t/+number]))
;
;(T a :- t/+integer)
(defn testfn [b c]
  (+ b c))

(defn blah2 [a] a)
