(ns typed.example.typed
  (:require [typed.core :refer [+T typed-ns]]
            [typed.types :refer [Vector Map]]
            [typed.example.typed2 :refer [add-twice MyNumber]]))

(+T double-num [MyNumber -> MyNumber])
(defn double-num [n]
  (add-twice n))

(+T my-map (Map MyNumber MyNumber))
(def my-map {1 2 3 4})

(+T a1 (Fun [MyNumber -> MyNumber]))
(defn a1 [n]
  n)

; Shorthand for single arity functions: 
(+T a2 [MyNumber -> MyNumber])
(defn a2 [n]
  n)

; A function that has 1 fixed parameter (Number) and any MyNumber
; of Number parameters, and returns a Number.
(+T a3 [MyNumber & MyNumber * -> MyNumber])
(defn a3 [n & ns]
  (+ n (count ns)))

; Multiple arity:
(+T a4 (Fun [MyNumber Boolean -> MyNumber]
            [Boolean -> pfloat]))
(defn a4
  ([n b] (+ n (if b 1 2)))
  ([b] (if b 1.1 2.2)))

(+T some-vec-test [(Vector MyNumber) -> pboolean])
(defn some-vec-test [vs]
  (boolean (first vs)))

#_(check-namespace 'typed.example.typed)
