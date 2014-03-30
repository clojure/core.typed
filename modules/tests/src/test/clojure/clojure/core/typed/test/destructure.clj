(ns clojure.core.typed.test.destructure
  (:import (clojure.lang APersistentVector Seqable APersistentMap))
  (:require [clojure.core.typed :refer [ann-form check-ns cf]]))

;; map destructuring
(let [{:keys [b] :or {b 3}} {}]
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
  (ann-form a (U nil Number))
  (ann-form b (U nil Number))
  (ann-form c (U nil (Seqable Number)))
  (ann-form d (U nil (APersistentVector Number))))

(let [[[x1 y1]
       [x2 y2]] [[1 2] [3 4]]]
  (ann-form [x1 y1 x2 y2]
            (Seqable Number)))

(let [[a b & c :as str] "asdjhhfdas"]
  ;could do a bit better there
  (ann-form [a b] (Seqable (U nil Character)))
  (ann-form c (U nil (Seqable Character)))
  (ann-form str String))

;vectors
(let [[a b c & d :as e] [1 2 3 4 5 6 7]]
  (ann-form [a b c] (Seqable Number))
  (ann-form d (U nil (Seqable Number)))
  (ann-form e (Seqable Number)))

;lists
(let [[a b c & d :as e] '(1 2 3 4 5 6 7)]
  (ann-form [a b c] (Seqable Number))
  (ann-form d (U nil (Seqable Number)))
  (ann-form e (Seqable Number)))
