(ns clojure.core.typed.test.dyn-propagate
  (:require [clojure.core.typed :as t]))

(t/tc-ignore
  (def a {:a 1}))

(defn b [local]
  (let [i a
        l (:a a)
        id (identity a)
        ]
    (t/print-env "there")
    (inc l)))
