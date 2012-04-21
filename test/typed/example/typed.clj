(ns typed.example.typed
  (:require [typed.core :refer [+T require-typed check-namespace]]
            [typed.example.typed2 :refer [add-twice]]))

(require-typed typed.example.typed2)

(+T double-num [Number -> Number])
(defn double-num [n]
  (add-twice n))

(+T my-map (Mapof Number Number))
(def my-map {1 2 3 4})

(+T a1 (Fun [Number -> Number]))
(defn a1 [n]
    n)

; Shorthand for single arity functions: 
(+T a2 [Number -> Number])
(defn a2 [n]
  n)

; A function that has 1 fixed parameter (Number) and any number
; of Number parameters, and returns a Number.
(+T a3 [Number & Number * -> Number])
(defn a3 [n & ns]
  (+ n (count ns)))

; Multiple arity:
(+T a4 (Fun [Number Boolean -> Number]
            [Boolean -> Double]))
(defn a4
  ([n b] (+ n (if b 1 2)))
  ([b] (if b 1.1 2.2)))

(+T some-vec-test [(Vectorof Number) -> Boolean])
(defn some-vec-test [vs]
  (boolean (first vs)))

(check-namespace 'typed.example.typed)
