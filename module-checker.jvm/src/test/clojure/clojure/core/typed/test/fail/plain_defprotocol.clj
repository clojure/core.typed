(ns clojure.core.typed.test.fail.plain-defprotocol
  (:require [clojure.core.typed :as t]))

; should throw top level type error
(defprotocol Foo (bar [this]))
