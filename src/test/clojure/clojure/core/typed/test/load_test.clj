(ns clojure.core.typed.test.load-test
  (:require [clojure.core.typed.load :as load]
            [clojure.test :refer :all]))

;; ensures evaluation occurs
(deftest evaluation-test
  (is (thrown? clojure.lang.ExceptionInfo
               (load/typed-load1 "clojure/core/typed/test/typed_load/eval"))))
