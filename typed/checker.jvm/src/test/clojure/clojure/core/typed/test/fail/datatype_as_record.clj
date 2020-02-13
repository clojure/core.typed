(ns clojure.core.typed.test.fail.datatype-as-record
  (:require [clojure.core.typed :as t]))

;Should throw a top level type error
(clojure.core.typed/ann-record IncorrectDt [])
(deftype IncorrectDt [])
