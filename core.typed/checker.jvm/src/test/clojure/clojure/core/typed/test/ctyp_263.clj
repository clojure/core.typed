(ns clojure.core.typed.test.ctyp-263
  (:require [clojure.test :refer :all]
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest catch-type-hint-test
  (is-tc-e (try :anything (catch Exception e (.getCause e)))))
