(ns cljs.core.typed.test.ympbyc.test-base-env
  (:require-macros [cljs.core.typed :refer [ann] :as ct])
  (:require [cljs.core.typed :refer [All U IFn Option I Any Seqable HSequential NonEmptyASeq NonEmptySeqable]]
            [cljs.core :refer [IVector ISeq ASeq]]))

;;seq
(ann seq-vec (NonEmptySeqable number))
(def seq-vec (seq [1 2 3]))

(ann seq-empty (Option (NonEmptyASeq nil)))
(def seq-empty (seq []))

;;fst

(ann vec-fst number)
(def vec-fst (first [8]))

(ann seq-fst number)
(def seq-fst (first (seq [1 2 3])))

(ann fst-nil nil)
(def fst-nil (first nil))


;;rest

(ann vec-rest (ASeq number))
(def vec-rest (rest [1 2 3]))

(ann seq-rest (ASeq number))
(def seq-rest (rest (seq [1 2 3])))

(ann rest-empty (ASeq nil))
(def rest-empty (rest []))


;;last

(ann vec-last number)
(def vec-last (last [1 2 3]))

(ann seq-last number)
(def seq-last (last (seq [1 2 3])))

(ann last-nil (Option number))
(def last-nil (last []))


;;butlast

(ann vec-butlast (ASeq number))
(def vec-butlast (butlast [1 2 3]))

(ann seq-butlast (ASeq number))
(def vec-butlast (butlast (seq [1 2 3])))

(ann butlast-empty (ASeq nil))
(def butlast-empty (butlast []))


;;utest if NonEmptySeqable is Seqable
(ann nonemp (All [x] [(NonEmptySeqable x) -> number]))
(defn foo [xs] 1)

(foo (seq [1 2 3]))



(ann second-vec number)
(def second-vec (second [1 2 3]))

(ann second-empty nil)
(def second-empty (second []))

(ann second-nil nil)
(def second-nil (second nil))

(ann second-seq (Option number))
(def second-seq (second (seq [1 2 3])))



(ann clj-to-jsjs Any)
(def clj-to-js (clj->js {:a 1}))

;use of js-obj triggers "js-op missing" in check_cljs
;(ann js-to-clj Any)
;(def js-to-clj (js->clj (js-obj "a" 1 "b" 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test vars in base-env-common

(ann take-vec (ASeq int))
(def take-vec (take 2 [1 2 3 4]))


(ann drop-vec (ASeq int))
(def drop-vec (drop 2 [1 2 3 4]))



(ann get-set (Option int))
(def get-set (get #{1 2 3} 2))
