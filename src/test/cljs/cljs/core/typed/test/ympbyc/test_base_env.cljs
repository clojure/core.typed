(ns cljs.core.typed.test.ympbyc.test-base-env
  (:require-macros [cljs.core.typed :refer [ann] :as ct])
  (:require [cljs.core.typed :refer [All U IFn]]
            [cljs.core :refer [IVector ISeq]]))

;;fst

(ann vec-fst (U nil number))
(def vec-fst (first [8]))

(ann seq-fst (U nil number))
(def seq-fst (first (seq [1 2 3])))

(ann fst-nil (U nil number))
(def fst-nil (first []))


;;rest

(ann vec-rest (ISeq number))
(def vec-rest (rest [1 2 3]))

(ann seq-rest (ISeq number))
(def seq-rest (rest (seq [1 2 3])))

(ann rest-empty (ISeq nil))
(def rest-empty (rest []))


;;last

(ann vec-last (U nil number))
(def vec-last (last [1 2 3]))

(ann seq-last (U nil number))
(def seq-last (last (seq [1 2 3])))

(ann last-nil (U nil number))
(def last-nil (last []))


;;butlast

(ann vec-butlast (ISeq number))
(def vec-butlast (butlast [1 2 3]))

(ann seq-butlast (ISeq number))
(def vec-butlast (butlast (seq [1 2 3])))

(ann butlast-empty (ISeq nil))
(def butlast-empty (butlast []))
