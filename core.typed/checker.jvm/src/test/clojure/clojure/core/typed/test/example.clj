(ns clojure.core.typed.test.example
  (:refer-clojure :exclude [< not=])
  (:import (java.io File))
  (:require [clojure.core.typed :refer [ann inst cf check-ns ann-form]
             :as t]
            [clojure.repl :refer [pst]]))

(ann test1 (t/All [x y] [x y -> x]))
(defn test1 [a b]
  a)

(test1 1 2)

(ann test2 (t/All [y] 
                [(t/Seqable y) -> (t/Seqable Number)]))
;(defn test2 [a]
;  (map + [1 2]))

(ann use-map [(t/HMap :mandatory {:a Number}) -> Number])
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
(ns clojure.core.typed.test.collatz
  (:require [clojure.core.typed :refer [ann]]))

(ann collatz [Number -> Number])
(defn collatz [n]
  (cond
    (= 1 n) 1
    (even? n) (collatz (/ n 2))
    :else (collatz (inc (* 3 n)))))
  )

(ann to-set (t/All [x]
                 [(t/U nil (t/Seqable x)) -> (t/Set x)]))
(defn to-set [a]
  (set a))

(ann config
     (t/HMap :mandatory {:file String
                       :ns t/Sym}))
(def config
  {:file "clojure/core.clj"
   :ns 'clojure.core})

(comment
(ann add-or-zero [(t/U nil Number) * -> Number])
(defn add-or-zero [& nzs]
  (reduce (t/fn [acc :- Number
                 n :- (t/U nil Number)]
            (+ acc (if n
                     n
                     0)))
          0 nzs))

(add-or-zero 1 2 3 nil)
)

(ann num-vec2 [(t/U nil Number) (t/U nil Number) -> '[Number Number]])
(defn num-vec2 [a b]
  [(if a a 0) (if b b 0)])

(ann < (t/IFn 
         [Number -> boolean]
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

(ann not= (t/IFn 
            [t/Any -> boolean]
            [t/Any t/Any -> boolean]
            [t/Any t/Any t/Any * -> boolean]))
(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))
