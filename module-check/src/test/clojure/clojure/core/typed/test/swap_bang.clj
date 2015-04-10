(ns clojure.core.typed.test.swap-bang
  (:require [clojure.core.typed :as t]))

(t/ann foo (t/Atom1 (t/Map ':a Number)))
(def foo (atom {:a 1}))

(fn []
  (swap! foo (t/inst assoc (t/Map ':a Number) ':a Number) :a 3))

;
;(swap! foo (fn [a] a))
;(swap! foo (fn [a] a))
;(swap! foo (fn [a b] a) 2)
;(swap! foo (fn [a b] a) 2)
