(ns clojure.core.typed.test.ctyp-285
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.test.cljc-fail]))

(deftest fail-on-cljc-type-check-test
  (is (caught-top-level-errors (constantly true)
                               (t/check-ns 'clojure.core.typed.test.cljc-fail))))


