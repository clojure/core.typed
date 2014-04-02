(ns clojure.core.typed.test.fail.unannotated-record
  (:require [clojure.core.typed :as t]))

;Should throw a top level type error
(defrecord Unannotated [])
