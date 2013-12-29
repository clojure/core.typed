(ns clojure.core.typed.test.swap-bang
  (:require [clojure.core.typed :as t]))

(t/ann foo (t/Atom1 '{:a Number}))
(def foo (atom {:a 1}))

(fn []
  (swap! foo assoc :a 3))

;
;(swap! foo (fn [a] a))
;(swap! foo (fn [a] a))
;(swap! foo (fn [a b] a) 2)
;(swap! foo (fn [a b] a) 2)
