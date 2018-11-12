(ns clojure.core.typed.test.check-tc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [check-ns]]))

#_(deftest check-tc
  ;fails in hudson
  (is (check-ns '[clojure.core.typed.checker.utils
                  clojure.core.typed.checker.type-rep
                  clojure.core.typed.checker.cs-rep
                  clojure.core.typed.checker.name-env
                  clojure.core.typed.checker.type-ctors])))
