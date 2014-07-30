(ns clojure.core.typed.test.nocheck
  (:require [clojure.core.typed :refer [ann check-ns]]))

(ann ^:no-check foo [Number -> Number])
(defn foo [a]
  'a)

(ann bar [Number -> Number])
(defn bar [b]
  (+ 2 (foo b)))
