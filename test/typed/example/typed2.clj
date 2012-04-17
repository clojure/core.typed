(ns typed.example.typed2
  (:require [typed.attempt2 :refer [+T]]))

(+T add-twice [Number -> Number])
(defn add-twice [n]
  (+ n n))
