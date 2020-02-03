(ns ^:skip-wiki clojure.core.typed.ext-test.clojure.core
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.typed :as t]))

(deftest ns-test
  ; type checked
  (is
    (let [form `(ns ~'foo)
          expected nil]
      (-> (binding [*ns* *ns*]
            (t/check-form-info form
                               :expected expected
                               :type-provided? true))
          :delayed-errors
          empty?)))
  ; type error
  (is
    (let [form `(ns ~'foo)
          expected `t/Str]
      (-> (binding [*ns* *ns*]
            (t/check-form-info form
                               :expected expected
                               :type-provided? true))
          :delayed-errors
          first
          ex-data
          :form
          #{form})))
  ; eval
  (is
    (binding [*ns* *ns*]
      (let [form `(ns ~'foo)
            expected nil
            res (t/check-form-info form
                                   :expected expected
                                   :type-provided? true)
            _ (is (= *ns* (find-ns 'foo)))]
        (-> res
            (find :result)
            #{[:result nil]})))))
