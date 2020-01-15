(ns clojure.core.typed.test.hsequential
  (:require [clojure.core.typed :as t]))

; currently we can't test if (HSequential [z t/Any *]) is subtype of
; (HSequential [x y ... y]) or not, because we can't compare free variable
(t/ann first-number [(t/HSequential [Number t/Any *]) -> Number])
(defn first-number [l] (first l))

; NOTE don't write code like this, we assume one argument corresponding to one type
(t/ann test-target (t/All [y ...] [Number y ... y -> (t/U nil Number)]))
(defn test-target [& y] (when-not (empty? y) (first-number y)))

#_(t/defn [y ...] test-target2 
  [& y :- y ... y] :- (t/U nil Number)
  (when-not (empty? y) (first-number y)))
