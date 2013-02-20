(ns typed.test.example
  (:refer-clojure :exclude [< not=])
  (:import (clojure.lang Seqable PersistentHashSet Symbol)
           (java.io File))
  (:require [typed.core :refer [ann inst cf fn> pfn> check-ns ann-form]]
            [clojure.repl :refer [pst]]
            [analyze.core :refer [ast]]))

(ann test1 (All [x y] [x y -> x]))
(defn test1 [a b]
  a)

(test1 1 2)

(ann test2 (All [y] 
                [(Seqable y) -> (Seqable Number)]))
;(defn test2 [a]
;  (map + [1 2]))

(ann use-map [(HMap {:a Number}) -> Number])
(defn use-map [a]
  (get a :a))

(use-map {:a 1})

;(ann rest-arg1 [Number * -> Number])
;(defn rest-arg1 [& ns]
;  (+ (first ns)))

;#lang typed-scheme
;(: collatz (Number → Number)) (define (collatz n)
;                                  (cond
;                                    [(= 1 n) 1] [(even? n)
;                                                 (collatz (/ n 2))]
;                                    [else (collatz (add1 (∗ 3 n)))]))
;(collatz 17)

(comment
(ns typed.test.collatz
  (:require [typed.core :refer [ann]]))

(ann collatz [Number -> Number])
(defn collatz [n]
  (cond
    (= 1 n) 1
    (even? n) (collatz (/ n 2))
    :else (collatz (inc (* 3 n)))))
  )

(ann to-set (All [x]
                 [(U nil (Seqable x)) -> (PersistentHashSet x)]))
(defn to-set [a]
  (set a))

(ann config
     (HMap {:file String
            :ns Symbol}))
(def config
  {:file "clojure/core.clj"
   :ns 'clojure.core})

(comment
(ann add-or-zero [(U nil Number) * -> Number])
(defn add-or-zero [& nzs]
  (reduce (fn> [[acc :- Number]
                [n :- (U nil Number)]]
            (+ acc (if n
                     n
                     0)))
          0 nzs))

(add-or-zero 1 2 3 nil)
)

(ann num-vec2 [(U nil Number) (U nil Number) -> (Vector* Number Number)])
(defn num-vec2 [a b]
  [(if a a 0) (if b b 0)])

(ann < (Fn [Number -> boolean]
           [Number Number -> boolean]
           [Number Number Number * -> boolean]))
#_(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  ([x] true)
  ([x y] (. clojure.lang.Numbers (lt x y)))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(ann not= (Fn [Any -> boolean]
              [Any Any -> boolean]
              [Any Any Any * -> boolean]))
(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))
