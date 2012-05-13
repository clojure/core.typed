(ns typed.example.typed
  (:require [typed.core :refer [+T typed-ns]]
            [typed.example.typed2 :refer [add-twice]]))

(typed-ns typed.example.typed
  ;default: (:require [typed.base :refer-type :all])
  ;(:refer-base :exclude [float ....])
  (:require [typed.example.typed2 :refer-type [my-number] :as t2]))

(+T double-num [my-number -> my-number])
(defn double-num [n]
  (add-twice n))

(+T my-map (map number number))
(def my-map {1 2 3 4})

(+T a1 (Fun [number -> number]))
(defn a1 [n]
  n)

; Shorthand for single arity functions: 
(+T a2 [number -> number])
(defn a2 [n]
  n)

; A function that has 1 fixed parameter (Number) and any number
; of Number parameters, and returns a Number.
(+T a3 [number & number * -> number])
(defn a3 [n & ns]
  (+ n (count ns)))

; Multiple arity:
(+T a4 (Fun [number boolean -> number]
            [boolean -> float]))
(defn a4
  ([n b] (+ n (if b 1 2)))
  ([b] (if b 1.1 2.2)))

(+T some-vec-test [(vector number) -> boolean])
(defn some-vec-test [vs]
  (boolean (first vs)))

#_(check-namespace 'typed.example.typed)
