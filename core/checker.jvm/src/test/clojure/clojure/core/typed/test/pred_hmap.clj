(ns clojure.core.typed.test.pred-hmap
  (:require [clojure.core.typed :as t]))
 
(t/defalias Cow '{:type ':cow})
(t/defalias Pig '{:type ':pig})
(t/defalias Animal (t/U Cow Pig))
 
(t/ann in-the-barn '[Animal Animal *])
(def in-the-barn [{:type :cow} {:type :pig}])
 
;; Could cow? have automatic annotations, as it comes from core.typed?
(t/ann cow? (t/Pred Cow))
(def cow? (t/pred Cow))
 
(t/ann cow-moo [Cow -> nil])
(defn cow-moo [cow])
 
(t/ann pig-oink [Pig -> nil])
(defn pig-oink [pig])
 
(let [a (first in-the-barn)]
  (if (cow? a)
    (cow-moo a)
    (pig-oink a)))
