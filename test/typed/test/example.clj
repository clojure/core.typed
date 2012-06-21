(ns typed.test.example
  (:import (clojure.lang Seqable))
  (:require [typed.core :refer [ann inst]]))

(ann test1 (All [x y] [x y -> x]))
(defn test1 [a b]
  a)

(ann test2 (All [y] 
                [(Seqable Number) -> (Seqable (U true false))]))
(defn test2 [a]
  ((inst map (U true false) Any) number? a))

(ann use-map [(Map* :mandatory
                    {:a Number})
              -> Number])
(defn use-map [a]
  (get a :a))

(use-map {:a 1})
