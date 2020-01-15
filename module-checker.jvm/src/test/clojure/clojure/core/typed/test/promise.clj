(ns clojure.core.typed.test.promise
  (:require [clojure.core.typed :as t]))

(t/ann p (t/Promise Number))
(def p (promise))

(fn []
  (p 1)

  (inc @p))
