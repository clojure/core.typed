(ns typed.example.typed2
  (:require [typed.core :refer [+T def-type-alias]]
            [typed.types :refer [AnyNumber]]))

(def-type-alias MyNumber AnyNumber)

(+T add-twice [MyNumber -> MyNumber])
(defn add-twice [n]
  (+ n n))
