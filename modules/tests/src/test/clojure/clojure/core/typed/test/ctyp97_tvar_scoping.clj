(ns clojure.core.typed.test.ctyp97-tvar-scoping
  (:import (clojure.lang ASeq LazySeq))
  (:require [clojure.core.typed :as t
             :refer [ann Seqable fn>]]))

(ann reduce_ (All [a b] (Fn [[a b
                              -> b]
                             b
                             (Seqable a)
                             -> b]
                            [[a (Seqable (U a b))
                              -> (Seqable (U a b))]
                             (Seqable (U a b))
                             (Seqable a)
                             -> (Seqable (U a b))])))
(defn reduce_
  [f zero coll]
  (if-let [s (seq coll)]
    (f (first s)
       (reduce_ f zero (rest s)))
    zero))

(ann append_ (All [a b] [(Seqable a) (Seqable b) -> (U (Seqable (U a b))
                                                       (ASeq (U a b)))]))
(defn append_
  [coll1 coll2]
  (reduce_ cons coll2 coll1))


; -------- main

; if you comment out this function, you will get :ok
(ann map-1 (All [a b] [[a -> b] (Seqable a)
                       -> (LazySeq b)]))
(defn map-1 [f coll]
  (lazy-seq
   (if-let [s (seq coll)]
     (cons (f (first s))
           (map-1 f (rest s))))))

(ann map-2 (All [a b] [[a -> b] (Seqable a) -> (LazySeq b)]))
(defn map-2
  [f coll]
  (lazy-seq
   (reduce_ (fn> [x :- a ; ERROR!! -- Cannot resolve type: a
                  y :- (Seqable b)]
              (append_ [(f x)] y))
            []
            coll)))
