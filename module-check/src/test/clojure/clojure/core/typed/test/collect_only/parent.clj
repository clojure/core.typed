(ns clojure.core.typed.test.collect-only.parent
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.test.collect-only.dep :as dep]))

(t/ann-form 1 dep/Num)
