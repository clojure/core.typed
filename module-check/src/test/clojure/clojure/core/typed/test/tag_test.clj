(ns clojure.core.typed.test.tag-test
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]))

;(deftest tag-test
;  (is-tc-e (tag 1 long))
;  (is-tc-err (tag 1 Long))
;  (is-tc-e (tag
;             (let [a (inc 1)
;                   b (* 2 10)]
;               (+ a b))
;             long)))
;
;(deftest unboxed-arith-test
;  (is-tc-e (do
;             ;from http://stackoverflow.com/questions/11824815/fast-complex-number-arithmetic-in-clojure
;             (ann-datatype complex [real :- Double
;                                    imag :- Double])
;             (deftype complex [^double real
;                               ^double imag])
;
;             (ann plus [complex complex -> complex])
;             (defn plus [^complex z1 ^complex z2]
;               (let [x1 (double (.real z1))
;                     y1 (double (.imag z1))
;                     x2 (double (.real z2))
;                     y2 (double (.imag z2))]
;                 (tag
;                   (complex. (ann-form 
;                               (tag 
;                                 (+ (tag x1 double) 
;                                    (tag x2 double))
;                                 double)
;                               Double)
;                             (+ y1 y2))
;                   complex))))))
;
