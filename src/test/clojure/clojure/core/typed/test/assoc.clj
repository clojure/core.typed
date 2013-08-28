(ns clojure.core.typed.test.assoc
  (:require [clojure.core.typed :refer [ann-form def-alias check-ns] :as t])
  (:import (clojure.lang Symbol)))

(ann-form (assoc {:a 1} :b 2)
          '{:a Number :b Number})

#(let [a 1]
   (assoc a :b 2))

(t/ann-record FooRec [a :- Number
                      b :- Symbol])
(defrecord FooRec [a b])

#(assoc (->FooRec 1 'a) :a 'b)

#(assoc (->FooRec 1 'a) :a 4)

#(update-in (->FooRec 1 'a) [:a] (constantly 2))
