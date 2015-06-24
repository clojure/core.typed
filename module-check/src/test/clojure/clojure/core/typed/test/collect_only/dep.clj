(ns clojure.core.typed.test.collect-only.dep
  {:core.typed {:collect-only true}}
  (:require [clojure.core.typed :as t]))

(t/defalias Num t/Num)
