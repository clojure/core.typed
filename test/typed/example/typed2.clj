(ns typed.example.typed2
  (:require [typed.core :refer [+T def-type-alias]]))

(def-type-alias my-number number)

(+T add-twice [my-number -> my-number])
(defn add-twice [n]
  (+ n n))
