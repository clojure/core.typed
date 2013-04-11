(ns clojure.core.typed.test.heterogeneous-restarg
  (:require [clojure.core.typed :refer [ann check-ns ann-form]]))

(ann het-args (Fn [Number -> Number]
                  [Number Number -> Number]))
(defn het-args [a & [b]]
  (cond b (+ a b)
        :else a))
