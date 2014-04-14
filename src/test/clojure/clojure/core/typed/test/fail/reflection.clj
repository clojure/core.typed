(ns clojure.core.typed.test.fail.reflection
  (:require [clojure.core.typed :as t]))

(t/fn> [a :- java.io.File]
  (.setReadOnly a 1))

(t/fn> [a :- java.io.File]
  (.getName a))

(fn [a]
  (java.io.File. a))
