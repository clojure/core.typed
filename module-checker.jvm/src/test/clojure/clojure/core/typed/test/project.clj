(ns clojure.core.typed.test.project
  (:require [clojure.core.typed :refer [ann check-ns] :as t]))

(ann my-atom (t/Atom1 (t/HMap :mandatory {:a t/Num})))
(def my-atom (atom {:a 1}))

(ann my-fn (t/All [x a ...] [t/Any x a ... a -> t/Any]))
(defn my-fn [a b & c]
  {:a 2})

(swap! my-atom my-fn :a 2)
