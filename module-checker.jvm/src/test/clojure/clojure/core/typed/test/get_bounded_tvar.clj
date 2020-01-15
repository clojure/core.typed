(ns clojure.core.typed.test.get-bounded-tvar
  (:require [clojure.core.typed :as t]))

(t/defalias QuilSketch Number)

(t/ann-record InkwellSketch [running? :- (t/Atom1 Boolean)])
(defrecord InkwellSketch [running?])

; free variables bounded by a record
(t/ann stop (t/All [[x :< InkwellSketch]] [x -> x]))
(defn stop [sketch]
  (reset! (:running? sketch) false)
  sketch)

; free variables bounded by a HMap
(t/ann f1 (t/All [[x :< '{:a (t/Atom1 Boolean)}]] [x -> x]))
(defn f1 [h]
  (reset! (:a h) false)
  h)
