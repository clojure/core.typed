(ns clojure.core.typed.test.core-match
  (:refer-clojure :exclude [macroexpand])
  (:import (clojure.lang Keyword))
  (:require [clojure.core.typed :refer [check-ns ann ann-form typed-deps]]
            [clojure.core.typed.hole :refer [noisy-hole]]
            ;[clojure.jvm.tools.analyzer.hygienic :refer [macroexpand]]
            [clojure.pprint :refer [pprint]]))
(comment
(require '[clojure.core.match :refer [match]])

(typed-deps clojure.core.typed.hole)

(let [x true
      y true
      z true]
  (match [x y z]
         [_ false true] 1
         [false true _ ] 2
         [_ _ false] 3
         [_ _ true] 4))

(->
  (let [x 1 y 2 z 4]
    (match [x y z]
           [1 2 b] [:a0 b]
           [a 2 4] [:a1 a]))
  (ann-form (U nil '[Keyword Number])))

(->
  (match [['my-sym]]
         [['my-sym]] :success)  ;; Branch is chosen because (= 'my-sym 'my-sym)
  (ann-form (U nil ':success)))

(->
  (let [a (+ 1 2)]
    (match [[3]]
           [[a]] a))  ;; 3 matches against the value of `a`, local binding is preserved
  ;; => 3
  (ann-form (U nil '3)))

(->
  (match [['my-sym]]
         [[a]] a) ;; a is a wildcard, here bound to 'my-sym on the right of the pattern row
  ;; => 'my-sym
  (ann-form (U nil 'my-sym)))

(->
  (let [v [1 2 3]]
    (match [v]
           [[1 1 1]] :a0
           [[1 2 1]] :a1
           [[1 2 _]] :a2))
  ;; => :a2
  (ann-form (U nil ':a0 ':a1 ':a2)))

(->
  (let [v [3 2 3]]
    (match [v]
           [[1 1 3]] :a0
           [[2 & r]] :a1
           [[3 & r]] :a2))
  ;; => :a2
  ; :a0 is unreachable
  (ann-form (U nil ':a1 ':a2)))

#_(->
  '(let [x '(1 2 3)]
    (match [x]
           [[1 (:or 3 4) 3]] :a0
           [[1 (:or 2 3) 3]] :a1))
  macroexpand
  pprint)

(->
  (let [x '(1 2 3)]
    (match [x]
           [[1 (:or 3 4) 3]] :a0
           [[1 (:or 2 3) 3]] :a1))
  ;; => :a1
  (ann-form (U nil ':a0 ':a1)))

(->
'(let [x {:a 3}]
  (match [x]
    [{:a (:or 1 2)}] :a0
    [{:a (:or 3 4)}] :a1))
  macroexpand
  pprint)
;; => :a1
)
