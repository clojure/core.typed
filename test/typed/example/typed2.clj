(ns typed.example.typed2
  (:require [typed.core :refer [+T]]))

(+T add-twice [Number -> Number])
(defn add-twice [n]
  (+ n n))
