(ns clojure.core.typed.test.poly
  (:require [clojure.core.typed :as t]))

(t/ann repeatedly'
     (t/All [x]
       (t/IFn 
         [[-> x] -> (t/Seqable x)]
         [t/Int [-> x] -> (t/Seqable x)])))
(defn repeatedly'
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  ([f] (lazy-seq (cons (f) (repeatedly' f))))
  ([n f] (take n (repeatedly' f))))
