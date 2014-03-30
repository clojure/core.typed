(ns clojure.core.typed.test.trampoline
  (:require [clojure.core.typed :as t]))

(declare funb)

(t/ann-many [Number -> (Rec [f]
                         (U Number [-> (U Number f)]))]
            funa funb)
(defn funa [n]
  (if (= n 0)
    0
    #(funb (dec n))))

(defn funb [n]
  (if (= n 0)
    0
    #(funa (dec n))))

(trampoline funa 100)
