(ns clojure.core.typed.annotator.test.rt-infer.loop
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}}
  (:require [clojure.core.typed :as t]))

(defn b [coll]
  (loop [c coll, out []]
    (if (seq c)
      (recur (next c) (conj out (inc (first c))))
      out)))

(b [1 2 3 4 5])

;(defn c [coll]
;  (loop [[:as c] coll
;         out []
;         {:as nothing} {}]
;    (if (seq c)
;      (recur (next c) (conj out (inc (first c))) nil)
;      out)))
;
;(c [1 2 3 4 5])

(doall
  (for
    [a [1 2]
     b [2 3]]
    [a b])
  )
