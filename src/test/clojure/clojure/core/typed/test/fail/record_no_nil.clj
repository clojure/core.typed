(ns clojure.core.typed.test.fail.record-no-nil
  (:require [clojure.core.typed :as t]))

(t/ann-record Foo [a :- Number])
(defrecord Foo [a])

(map->Foo {})
