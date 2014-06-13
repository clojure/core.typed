(ns clojure.core.typed.test.var-usage
  (:require [clojure.core.typed :refer [ann check-ns]]))

(ann foo [Number -> Number])
(declare foo)

(ann bar [Number -> Number])
(defn bar [b]
  (+ 2 (foo b)))
