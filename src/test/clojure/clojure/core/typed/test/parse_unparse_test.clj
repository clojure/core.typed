(ns clojure.core.typed.test.parse-unparse-test
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [clojure.core.typed.parse-unparse :refer :all]
    [clojure.core.typed :as t]))

(deftest unparse-free-scoping-test
  (is-clj (= (second
               (unparse-type 
                 (parse-type 
                   `(t/All ~'[a b] [t/Any t/Any :-> t/Any]))))
             '[a b]))
  (is-clj (= (rest (unparse-type (parse-type `(t/TFn ~'[[a :variance :covariant]] ~'a))))
             '([[a :variance :covariant]] a)))
  (is-clj (= (do
               '([a b] [a b -> [a b -> nil :filters {:then ff :else tt}]
                              :filters {:then tt :else ff}]))
             (->
               (tc-e
                 (fn :forall [a b]
                   [f :- a, coll :- b]
                   (fn
                     [x :- a
                      y :- b])))
               :t
               unparse-type
               rest))))
