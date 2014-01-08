(ns clojure.core.typed.test.collect-on-eval
  (:require [clojure.core.typed :as t]))

(t/ann foo-bar Number)
(t/cf (def foo-bar 1))
(t/cf foo-bar)
