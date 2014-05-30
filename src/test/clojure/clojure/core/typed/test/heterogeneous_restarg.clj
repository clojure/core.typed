(ns clojure.core.typed.test.heterogeneous-restarg
  (:require [clojure.core.typed :refer [ann check-ns ann-form]
             :as t]))

(ann het-args (t/IFn
                [Number -> Number]
                [Number Number -> Number]))
(defn het-args [a & [b]]
  (cond b (+ a b)
        :else a))
