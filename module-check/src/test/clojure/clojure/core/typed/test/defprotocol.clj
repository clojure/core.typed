(ns clojure.core.typed.test.defprotocol
  (:refer-clojure :exclude [defprotocol fn])
  (:require [clojure.core.typed :as t :refer [defprotocol fn Int Num]]))

(defprotocol Foo
  (is-foo [this, a :- Num] :- Num)
  (is-bar [this, a :- Int] :- Int
          [this, b :- Num] :- Num)
  (is-baz [this, a :- Int] :- Int
          [this, a :- Int, b :- Int] :- Int
          [this, a :- Int, b :- Int, c :- Int] :- Int))

(defprotocol DocD
  "This is a docstring"
  (docd [this, a :- Num] :- Num
        "trailing docstring"))

(fn [a :- Foo]
  (is-foo a 1))

(fn [a :- Foo] :- Int
  (is-bar a 1))

(fn [a :- Foo] :- Num
  (is-bar a 1.1))

(fn [a :- Foo] :- Int
  (is-baz a 1))

(fn [a :- Foo] :- Int
  (is-baz a 1 1))

(fn [a :- Foo] :- Int
  (is-baz a 1 1 1))
