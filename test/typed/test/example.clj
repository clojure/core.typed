(ns typed.test.example
  (:import (clojure.lang Seqable))
  (:require [typed.core :refer [ann inst cf fn> pfn> check-ns]]
            [clojure.repl :refer [pst]]
            [analyze.core :refer [ast]]))

(ann test1 (All [x y] [x y -> x]))
(defn test1 [a b]
  a)

(test1 1 2)

(ann test2 (All [y] 
                [(Seqable y) -> (Seqable Number)]))
(defn test2 [a]
  (map + [1 2]))

(ann use-map [(HMap {:a Number}) -> Number])
(defn use-map [a]
  (get a :a))

(use-map {:a 1})

;(ann rest-arg1 [Number * -> Number])
;(defn rest-arg1 [& ns]
;  (+ (first ns)))
