(ns clojure.core.typed.test.check-tc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [check-ns]]))

(deftest check-tc
  (is (check-ns '[clojure.core.typed.chk.common.utils
                  clojure.core.typed.chk.common.type-rep
                  clojure.core.typed.chk.common.cs-rep
                  clojure.core.typed.chk.common.name-env
                  clojure.core.typed.chk.common.type-ctors])))
