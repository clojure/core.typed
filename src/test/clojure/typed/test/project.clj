(ns typed.test.project
  (:import [clojure.lang Atom])
  (:require [typed.core :refer [ann check-ns]]))

(ann my-atom (Atom (HMap {:a Number}) (HMap {:a Number})))
(def my-atom (atom {:a 1}))

(ann my-fn (All [x a ...] [Any x a ... a -> Any]))
(defn my-fn [a b & c]
  {:a 2})

(swap! my-atom my-fn :a 2)
