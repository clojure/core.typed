(ns clojure.core.typed.test.assoc
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.core.typed :refer [ann-form check-ns] :as t]
    [clojure.test :refer :all])
  (:import (clojure.lang Symbol)))

(deftest assoc-test
  (is-tc-e (assoc {:a 1} :b 2)
           '{:a Number :b Number})

  (is-tc-err #(let [a 1]
                (assoc a :b 2)))
  (is-tc-err
    (do (t/ann-record FooRec [a :- t/Num
                              b :- t/Symbol])
        (defrecord FooRec [a b])
        (assoc (->FooRec 1 'a) :a 'b)))

  (is-tc-e
    (do (t/ann-record FooRec [a :- t/Num
                              b :- t/Symbol])
        (defrecord FooRec [a b])
        (assoc (->FooRec 1 'a) :a 4)))

  ;intersections
  (is-tc-e (t/fn [m :- (t/I (t/HMap :mandatory {:foo Num})
                            (t/HMap :mandatory {:bar Num}))]
             :- '{:foo Num, :bar Num, :baz Num}
             (assoc m :baz 2))))
