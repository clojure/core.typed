(ns typed-clojure.example.typed2
  (:require [typed-clojure.attempt2 :refer [+T]]))

(+T add-twice [Number -> Number])
(defn add-twice [n]
  (+ n n))
