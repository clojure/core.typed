(ns clojure.core.typed.test.record-optional-key
  (:require [clojure.core.typed :as t]))

(t/ann-record Foo [a :- (U nil Number)])
(defrecord Foo [a])

(map->Foo {})
