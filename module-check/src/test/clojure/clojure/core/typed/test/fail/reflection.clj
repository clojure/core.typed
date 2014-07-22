(ns clojure.core.typed.test.fail.reflection
  (:require [clojure.core.typed :as t]))

(t/fn [a :- java.io.File]
  (.setReadOnly a 1))

(t/fn [a :- java.io.File]
  (.getName a))

(fn [a]
  (java.io.File. a))

(t/ann write-lines [java.io.Writer (t/Option (t/Coll String)) -> nil])
(defn write-lines [writer lines]
  (t/doseq [l :- String lines]
    (.write writer "testing")
    (println l)))
