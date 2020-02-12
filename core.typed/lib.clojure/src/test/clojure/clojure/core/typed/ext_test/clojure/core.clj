(ns ^:skip-wiki clojure.core.typed.ext-test.clojure.core
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.typed :as t]))

(defn eval-in-ns [form]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (refer-clojure)
    (form)))

(deftest ns-test
  ; type checked
  (let [form `(ns ~'foo)
        expected nil
        res (binding [*ns* *ns*]
              (t/check-form-info form
                                 :expected expected
                                 :type-provided? true))]
    (is (-> res :delayed-errors empty?))
    (is (not (:ex res))))
  ; type error
  (is
    (let [form `(ns ~'foo)
          expected `t/Str
          res (binding [*ns* *ns*]
                (t/check-form-info form
                                   :expected expected
                                   :type-provided? true))
          err ((some-fn (comp first
                              :errors
                              ex-data
                              :ex)
                        (comp first
                              :delayed-errors))
               res)]
      (is (-> err
              ex-data
              :form
              #{form}))))
  ; eval
  (is
    (binding [*ns* *ns*]
      (let [form `(ns ~'foo)
            res (t/check-form-info form)
            _ (is (= *ns* (find-ns 'foo)))]
        (-> res
            (find :result)
            #{[:result nil]})))))
