(ns cljs.core.typed.test.identity
  (:require [cljs.core.typed :as t])
  (:require-macros [cljs.core.typed :as t]))

(t/ann my-identity (All [x] [x -> (U x t/Number)]))
(defn my-identity [x]
  (if (number? x)
    (inc x)
    x))
