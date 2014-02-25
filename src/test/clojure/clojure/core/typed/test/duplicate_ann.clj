(ns clojure.core.typed.test.duplicate-ann
  (:require [clojure.core.typed :as t]))

(t/ann foo Number)

(t/ann foo Integer)
