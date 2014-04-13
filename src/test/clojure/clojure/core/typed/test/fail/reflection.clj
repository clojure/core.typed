(ns clojure.core.typed.test.fail.reflection
  (:require [clojure.core.typed :as t]))

(fn [a]
  (.missing a))
