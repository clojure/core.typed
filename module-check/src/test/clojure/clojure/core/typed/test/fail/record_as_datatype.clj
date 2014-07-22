(ns clojure.core.typed.test.fail.record-as-datatype
  (:require [clojure.core.typed :as t]))

;Should throw a top level type error
(t/ann-datatype IncorrectRec [])
(defrecord IncorrectRec [])
