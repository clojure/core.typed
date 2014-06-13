(ns clojure.core.typed.test.mapv-test
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t :refer [ann-form print-env fn]]))

(deftest mapv-test
  (testing "when we know the input sequence has values"
    (is-tc-e
     (fn [coll :- (NonEmptySeqable String)]
       (let [result (mapv str coll)]
         (ann-form result (CountRange 1)))))))
