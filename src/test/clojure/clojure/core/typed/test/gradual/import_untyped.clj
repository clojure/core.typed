(ns clojure.core.typed.test.gradual.import-untyped
  "Docstring"
  {:lang :core.typed}
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.test.gradual.untyped :as u]))

(t/untyped-var u/a t/Int)
(t/untyped-var u/b t/Int)

(t/ann good [:-> t/Int])
(defn good []
  u/a)

(t/ann bad [:-> t/Int])
(defn bad []
  u/b)

;#(inc 'a)
