(ns typed.test.collatz
  (:require [typed.core :refer [check-ns ann]]))

(ann collatz [Number -> Number])
(defn collatz [n]
  (cond
    (= 1 n) 
     1
    (and (integer? n) 
         (even? n)) 
     (collatz (/ n 2))
    :else 
     (collatz (inc (* 3 n)))))
