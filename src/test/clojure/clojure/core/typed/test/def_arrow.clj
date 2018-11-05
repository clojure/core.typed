(ns clojure.core.typed.test.def-arrow
  (:require [clojure.core.typed :as t]))

(t/def a :- Number, 1)

(assert (= (+ a 1) 2))
