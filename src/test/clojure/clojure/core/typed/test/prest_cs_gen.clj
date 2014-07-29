(ns clojure.core.typed.test.prest-cs-gen
  (:require [clojure.core.typed :as t]))

(t/ann foo (All [k v r] [[k v -> r] k v -> r]))
(defn foo [f k v] (f k v))

(t/ann foo1 (All [k v r] [[k v k v -> r] k v k v -> r]))
(defn foo1 [f k1 v1 k2 v2] (f k1 v1 k2 v2))

(t/ann bar (t/Map Number String))
(def bar (foo (t/inst hash-map Number String) 1 "a"))

(def bar (foo1 (t/inst hash-map Number String) 1 "a" 2 "b"))

; this map1 must accept at least 2 list, this is correspond one case in our hard mode in cs-gen-Function
(t/ann ^:no-check map1 (All [a b r c ...]
                         [[a b c ... c -> r] (t/Seqable a) (t/Seqable b) (t/Seqable c) ... c -> (t/Seqable r)]))
(defn map1 [f a b & rst]
  (apply map f a b rst))

(t/ann bar1 (t/Seqable (t/Map Number String)))
(def bar1 (map1 (t/inst hash-map Number String) [1 2 3] ["a" "b" "c"]))
(def bar1 (map1 (t/inst hash-map Number String) [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))

(def bar1 (map (t/inst hash-map Number String) [1 2 3] ["a b c"]))
(def bar1 (map (t/inst hash-map Number String) [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))

; this hash-map1 accept two dummy arguments, this is correspond to our easy mode in cs-gen-Function
(t/ann hash-map1 [Any Any (HSequential [Number String] :repeat true) <* -> (t/Map Number String)])
(defn hash-map1 [_ _ & rst] (apply (t/inst hash-map Number String) rst))

(def bar1 (map hash-map1 [1 "a" \a] [1 "c" \a] [1 2 3] ["a b c"]))
(def bar1 (map hash-map1 [1 "a" \a] [1 "c" \a] [1 2 3] ["a" "b" "c"] [4 5 6] ["d" "e" "f"]))

; test prest <: rest, `All` just makes function a Poly function, poly function
; and FnIntersection treated differently in Typed Clojure
(t/ann higher-level-func (All [x y]
                           [[(HSequential [Number Number] :repeat true) <* -> Number] -> Number]))
(defn higher-level-func [f]
  (f 1 2))

(t/ann higher-level-func1 [[(HSequential [Number Number] :repeat true) <* -> Number] -> Number])
(defn higher-level-func1 [f]
  (f 1 2))

(t/ann number Number)
(def number (higher-level-func +))
(def number (higher-level-func1 +))

(t/ann foo2 [(HSequential [Number String] :repeat true) <* -> (U nil (t/Map Number String))])
(defn foo2 [& rst] (when-not (nil? rst) (apply (t/inst hash-map Number String) rst)))

(t/ann nsmap (U nil (t/Map Number String)))
(def nsmap (apply foo2 1 "2" [3 "4"]))
(def nsmap (apply foo2 1 "2" 3 ["4"]))

; check Poly func with prest in apply
(t/ann foo3 (All [x] [(HSequential [Number String] :repeat true) <* -> (U nil (t/Map Number String))]))
(defn foo3 [& rst] (when-not (nil? rst) (apply (t/inst hash-map Number String) rst)))
(def nsmap (apply foo3 1 "2" [3 "4"]))
