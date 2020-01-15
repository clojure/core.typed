(ns clojure.core.typed.test.destructure
  (:import (clojure.lang APersistentVector APersistentMap))
  (:require [clojure.core.typed :as t :refer [ann-form check-ns cf]]))

;; map destructuring
(let [{:keys [b] :or {b 3}} {}]
  (ann-form b Number))

(let* [map__65083 {} 
       map__65083 (if (seq? map__65083) 
                    (clojure.lang.PersistentHashMap/create (clojure.core/seq map__65083)) 
                    map__65083) 
       b (get map__65083 :b 3)] 
  (ann-form b Number))

(let [{:as c} {}]
  (ann-form c '{}))

(let [{:as c} nil]
  (ann-form c nil))

;FIXME
;(let [{:strs [str] :syms [symb]} (ann-form {} (Extends [(APersistentMap Any String)] :without [(clojure.lang.ISeq Any)]))]
;  (ann-form symb (U nil String))
;  (ann-form str (U nil String)))

;; vector destructuring
(let [[a b & c :as d] (ann-form [] (APersistentVector Number))]
  (ann-form a (t/U nil Number))
  (ann-form b (t/U nil Number))
  (ann-form c (t/U nil (t/Seqable Number)))
  (ann-form d (t/U nil (APersistentVector Number))))

(let [[[x1 y1]
       [x2 y2]] [[1 2] [3 4]]]
  (ann-form [x1 y1 x2 y2]
            (t/Seqable Number)))

(let [[a b & c :as str] "asdjhhfdas"]
  ;could do a bit better there
  (ann-form [a b] (t/Seqable (t/U nil Character)))
  (ann-form c (t/U nil (t/Seqable Character)))
  (ann-form str String))

;vectors
(let [[a b c & d :as e] [1 2 3 4 5 6 7]]
  (ann-form a Number)
  (ann-form b Number)
  (ann-form c Number)
  (ann-form [a b c] (t/Seqable Number))
  (ann-form d (t/U nil (t/Seqable Number)))
  (ann-form e (t/Seqable Number)))

;lists
(let [[a b c & d :as e] '(1 2 3 4 5 6 7)]
  ; FIXME stopped working with 1.9.0
  ; to fix, start adding HList to `AnySequential?` predicate
  ;(ann-form [a b c] (t/Seqable Number))
  (ann-form d (t/U nil (t/Seqable Number)))
  (ann-form e (t/Seqable Number)))

;flexible map destructing
(let [{:keys [a]} (ann-form '() (t/U nil (t/Seq t/Any)))]
  a)
