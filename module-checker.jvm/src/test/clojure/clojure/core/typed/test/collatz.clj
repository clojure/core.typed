(ns clojure.core.typed.test.collatz
  (:require [clojure.core.typed :refer [ann] :as t]))

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

(collatz 10)
