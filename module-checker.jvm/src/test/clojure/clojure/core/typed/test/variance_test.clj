(ns clojure.core.typed.test.variance-test
  (:require [clojure.core.typed :as t]))


(t/ann-datatype [[w :variance :contravariant]
                 [r :variance :covariant]]
                FooT
                [])

(deftype FooT [])
