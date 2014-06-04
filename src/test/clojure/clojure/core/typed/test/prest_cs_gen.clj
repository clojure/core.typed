(ns clojure.core.typed.test.prest-cs-gen
  (:require [clojure.core.typed :as t]))

(t/ann foo (All [k v r] [[k v -> r] k v -> r]))
(defn foo [f k v] (f k v))

(t/ann foo1 (All [k v r] [[k v k v -> r] k v k v -> r]))
(defn foo1 [f k1 v1 k2 v2] (f k1 v1 k2 v2))

(t/ann bar (t/Map Number String))
(def bar (foo (t/inst hash-map Number String) 1 "a"))

(def bar (foo1 (t/inst hash-map Number String) 1 "a" 2 "b"))

; FIXME remove no-check
; this map1 must accept at least 2 list, this is correspond to our easy mode in cs-gen-Function
(t/ann ^:no-check map1 (All [a b r c ...]
                         [[a b c ... c -> r] (t/Seqable a) (t/Seqable b) (t/Seqable c) ... c -> (t/Seqable r)]))
(defn map1 [f a b & rst]
  (apply map f a b rst))

(t/ann bar1 (t/Seqable (t/Map Number String)))
(def bar1 (map1 (t/inst hash-map Number String) [1 2 3] ["a" "b" "c"]))
(def bar1 (map1 (t/inst hash-map Number String) [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))
