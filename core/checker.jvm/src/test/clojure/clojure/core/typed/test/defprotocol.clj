(ns clojure.core.typed.test.defprotocol
  (:refer-clojure :exclude [defprotocol fn])
  (:require [clojure.core.typed :as t :refer [defprotocol fn]]))

(defprotocol Foo
  (is-foo [this, a :- t/Num] :- t/Num)
  (is-bar [this, a :- t/Int] :- t/Int
          [this, b :- t/Num] :- t/Num)
  (is-baz [this, a :- t/Int] :- t/Int
          [this, a :- t/Int, b :- t/Int] :- t/Int
          [this, a :- t/Int, b :- t/Int, c :- t/Int] :- t/Int))

(defprotocol DocD
  "This is a docstring"
  (docd [this, a :- t/Num] :- t/Num
        "trailing docstring"))

(fn [a :- Foo]
  (is-foo a 1))

(fn [a :- Foo] :- t/Int
  (is-bar a 1))

(fn [a :- Foo] :- t/Num
  (is-bar a 1.1))

(fn [a :- Foo] :- t/Int
  (is-baz a 1))

(fn [a :- Foo] :- t/Int
  (is-baz a 1 1))

(fn [a :- Foo] :- t/Int
  (is-baz a 1 1 1))
