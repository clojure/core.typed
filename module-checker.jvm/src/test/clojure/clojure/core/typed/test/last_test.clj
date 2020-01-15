(ns clojure.core.typed.test.last-test
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t :refer [ann-form print-env fn]]))

(deftest last-test
  (testing "when we know there is at least one value"
    (is-tc-e
     (fn [coll :- (NonEmptySeqable Long)]
       (let [value (last coll)]
         (ann-form value Long))))))
