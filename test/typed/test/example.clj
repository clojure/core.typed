(ns typed.test.example
  (:require [typed.core :refer [ann]]))

(ann test1 (All [x] [x -> x]))
(defn test1 [a]
  a)
