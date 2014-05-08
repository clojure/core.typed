(ns clojure.core.typed.test.pred-hmap
  (:require [clojure.core.typed :as t :refer [ann ann-form def-alias declare-names]]))
 
(def-alias Cow '{:type ':cow})
(def-alias Pig '{:type ':pig})
(def-alias Animal (U Cow Pig))
 
(t/ann in-the-barn '[Animal Animal *])
(def in-the-barn [{:type :cow} {:type :pig}])
 
;; Could cow? have automatic annotations, as it comes from core.typed?
(t/ann cow? (predicate Cow))
(def cow? (t/pred Cow))
 
(t/ann cow-moo [Cow -> nil])
(defn cow-moo [cow])
 
(t/ann pig-oink [Pig -> nil])
(defn pig-oink [pig])
 
(let [a (first in-the-barn)]
  (if (cow? a)
    (cow-moo a)
    (pig-oink a)))
