(ns clojure.core.typed.test.prest-cs-gen
  (:require [clojure.core.typed :as t]))

(t/ann foo (t/All [k v r] [[k v -> r] k v -> r]))
(defn foo [f k v] (f k v))

(t/ann foo1 (t/All [k v r] [[k v k v -> r] k v k v -> r]))
(defn foo1 [f k1 v1 k2 v2] (f k1 v1 k2 v2))

(t/ann bar (t/Map t/Num t/Str))
(def bar (foo (t/inst hash-map t/Num t/Str) 1 "a"))

(def bar (foo1 (t/inst hash-map t/Num t/Str) 1 "a" 2 "b"))

; this map1 must accept at least 2 list, this is correspond one case in our hard mode in cs-gen-Function
(t/ann ^:no-check map1 (t/All [a b r c ...]
                         [[a b c ... c -> r] (t/Seqable a) (t/Seqable b) (t/Seqable c) ... c -> (t/Seqable r)]))
(defn map1 [f a b & rst]
  (apply map f a b rst))

(t/ann bar1 (t/Seqable (t/Map t/Num t/Str)))
(def bar1 (map1 (t/inst hash-map t/Num t/Str) [1 2 3] ["a" "b" "c"]))
(def bar1 (map1 (t/inst hash-map t/Num t/Str) [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))

(def bar1 (map (t/inst hash-map t/Num t/Str) [1 2 3] ["a b c"]))
(def bar1 (map (t/inst hash-map t/Num t/Str) [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))

; this hash-map1 accept two dummy arguments, this is correspond to our easy mode in cs-gen-Function
(t/ann hash-map1 [t/Any t/Any (t/HSequential [t/Num t/Str] :repeat true) <* -> (t/Map t/Num t/Str)])
(defn hash-map1 [_ _ & rst] (apply (t/inst hash-map t/Num t/Str) rst))

(def bar1 (map hash-map1 [1 "a" \a] [1 "c" \a] [1 2 3] ["a b c"]))
(def bar1 (map hash-map1 [1 "a" \a] [1 "c" \a] [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))

(t/ann hash-map2 [(t/HSequential [t/Num t/Str] :repeat true) <* -> (t/Map t/Num t/Str)])
(defn hash-map2 [& rst] (apply (t/inst hash-map t/Num t/Str) rst))

(def bar2 (map hash-map2 [1] ["a"] [2] ["b"]))

; test prest <: rest, `t/All` just makes function a Poly function, poly function
; and FnIntersection treated differently in Typed Clojure
(t/ann higher-level-func (t/All [x y]
                           [[(t/HSequential [t/Num t/Num] :repeat true) <* -> t/Num] -> t/Num]))
(defn higher-level-func [f]
  (f 1 2))

(t/ann higher-level-func1 [[(t/HSequential [t/Num t/Num] :repeat true) <* -> t/Num] -> t/Num])
(defn higher-level-func1 [f]
  (f 1 2))

(t/ann number t/Num)
(def number (higher-level-func +))
(def number (higher-level-func1 +))

(t/ann foo2 [(t/HSequential [t/Num t/Str] :repeat true) <* -> (t/U nil (t/Map t/Num t/Str))])
(defn foo2 [& rst] (when-not (nil? rst) (apply (t/inst hash-map t/Num t/Str) rst)))

(t/ann nsmap (t/U nil (t/Map t/Num t/Str)))
(def nsmap (apply foo2 1 "2" [3 "4"]))
(def nsmap (apply foo2 1 "2" 3 ["4"]))

; check Poly func with prest in apply
(t/ann foo3 (t/All [x] [(t/HSequential [t/Num t/Str] :repeat true) <* -> (t/U nil (t/Map t/Num t/Str))]))
(defn foo3 [& rst] (when-not (nil? rst) (apply (t/inst hash-map t/Num t/Str) rst)))
(def nsmap (apply foo3 1 "2" [3 "4"]))
