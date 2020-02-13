(ns clojure.core.typed.test.error-msg
  (:require
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [clojure.core.typed :as t :refer [check-ns
                                      check-ns-info]]))

(deftest invoke-line-number
  (is (=
        {:line 4 :column 2}
        (-> (check-ns-info 'clojure.core.typed.test.line-number)
            :delayed-errors
            first
            ex-data
            :env
            (select-keys [:line :column])))))
