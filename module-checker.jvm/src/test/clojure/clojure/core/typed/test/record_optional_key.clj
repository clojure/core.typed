(ns clojure.core.typed.test.record-optional-key
  (:require [clojure.core.typed :as t]))

(t/ann-record Foo [a :- (t/U nil Number)])
(defrecord Foo [a])

(map->Foo {})

(t/ann-record FooP [a :- (t/U nil Number)])
(defrecord FooP [a])

(map->FooP {})
