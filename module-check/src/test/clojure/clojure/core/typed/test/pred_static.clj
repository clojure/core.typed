(ns clojure.core.typed.test.pred-static
  (:require [clojure.core.typed :as t]))

(let [a (t/ann-form {:a 1} Any)
      _ (assert ((t/pred '{:a Number}) a))]
  (+ 1 (:a a)))
