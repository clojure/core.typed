(ns clojure.core.typed.test.ctyp-255
  (:require [clojure.test :refer :all]
            [clojure.core.typed.parse-unparse :refer :all]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed :as t]
            [clojure.core.typed.init :as init]
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest ctyp-255-test
  (testing "unknown implementation of unparse uses clojure.core.typed
           for special types"
    (is (= (unparse-type r/-any)
           'clojure.core.typed/Any)))
  (testing "can print a tc-e result with :unknown"
    (is (do (prn (tc-e 1))
            true))))
