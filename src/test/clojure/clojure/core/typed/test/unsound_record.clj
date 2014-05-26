(ns clojure.core.typed.test.unsound-record
  (:require [clojure.core.typed :as t]))

(t/ann-record Foo [a :- Number])
(defrecord Foo [a])

(t/ann unsound [(t/Map t/Any t/Any) -> Number])
(defn unsound [r]
  (let [r (assoc r :a nil)]
    (assert (instance? Foo r))
    (inc (:a r))))

(fn []
  (unsound (->Foo 1)))
