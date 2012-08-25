(ns typed.test.poly
  (:require [typed.core :refer [ann AnyInteger check-ns cf]]
            [clojure.repl :refer [pst]])
  (:import [clojure.lang Seqable]))

(ann repeatedly'
     (All [x]
       (Fn [[-> x] -> (Seqable x)]
           [AnyInteger [-> x] -> (Seqable x)])))
(defn repeatedly'
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  ([f] (lazy-seq (cons (f) (repeatedly' f))))
  ([n f] (take n (repeatedly' f))))
