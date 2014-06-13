(ns clojure.core.typed.test.datatype-recur
  (:require [clojure.core.typed :as t]))

(t/ann-protocol PBar
                rec [PBar Number -> Number])
(t/defprotocol> PBar
  (rec [this bar]))

(t/ann-datatype Foo [])
(deftype Foo []
  PBar
  (rec [this bar]
    (if (even? (rand-int 10))
      (recur bar)
      bar)))
