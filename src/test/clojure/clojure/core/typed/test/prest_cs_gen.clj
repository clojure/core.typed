(ns clojure.core.typed.test.prest-cs-gen
  (:require [clojure.core.typed :as t]))

(t/ann foo (All [k v r] [[k v -> r] k v -> r]))
(defn foo [f k v] (f k v))

(t/ann foo1 (All [k v r] [[k v k v -> r] k v k v -> r]))
(defn foo1 [f k1 v1 k2 v2] (f k1 v1 k2 v2))

(t/ann bar (t/Map Number String))
(def bar (foo (t/inst hash-map Number String) 1 "a"))

(def bar (foo1 (t/inst hash-map Number String) 1 "a" 2 "b"))
