(ns clojure.core.typed.test.load-test
  (:require [clojure.core.typed.load :as load]
            [clojure.test :refer :all]))

;; ensures evaluation occurs
(deftest evaluation-test
  (is (try (load/typed-load1 "clojure/core/typed/test/typed_load/eval")
           nil
           (catch clojure.lang.ExceptionInfo e
             (-> e ex-data :blame :file #{"clojure/core/typed/test/typed_load/eval.clj"})))))
