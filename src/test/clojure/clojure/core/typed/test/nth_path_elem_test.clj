(ns clojure.core.typed.test.nth-path-elem-test
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t :refer [ann-form defalias print-env fn]]))

(deftest nth-path-elem-test-first
  (is-tc-e
   (do
     (defalias StatementA '[':params String])
     (defalias StatementB '[':no-params])
     (defalias Statement (U StatementA StatementB))
     (fn [stmt :- Statement] :- Any
       (if (= :params (first stmt))
         (let [param (nth stmt 1)]
           (ann-form param String)))))))

(deftest nth-path-elem-test-second
  (is-tc-e
   (do
     (defalias StatementA '[Number ':params String])
     (defalias StatementB '[Number ':no-params])
     (defalias Statement (U StatementA StatementB))
     (fn [stmt :- Statement] :- Any
       (if (= :params (second stmt))
         (let [param (nth stmt 2)]
           (ann-form param String))))))

  (testing "we actually do typechecking at the innermost form"
    ;; If `second` incorrectly uses an index of 0, this test fails
    (is-tc-err
     (do
       (defalias StatementA '[Number ':params String])
       (defalias StatementB '[Number ':no-params])
       (defalias Statement (U StatementA StatementB))
       (fn [stmt :- Statement] :- Any
         (if (= :params (second stmt))
           (let [param (nth stmt 2)]
             (ann-form param Number))))))))

(deftest nth-path-elem-test-types
  (testing "HVec"
    (is-tc-e
     (do
       (defalias StatementA (HVec [':params String]))
       (defalias StatementB (HVec [':no-params]))
       (defalias Statement (U StatementA StatementB))
       (fn [stmt :- Statement] :- Any
         (if (= :params (first stmt))
           (ann-form stmt StatementA))))))

  (testing "HSeq"
    (is-tc-e
     (do
       (defalias StatementA (HSeq [':params String]))
       (defalias StatementB (HSeq [':no-params]))
       (defalias Statement (U StatementA StatementB))
       (fn [stmt :- Statement] :- Any
         (if (= :params (first stmt))
           (ann-form stmt StatementA))))))

  (testing "HSequential"
    (is-tc-e
     (do
       (defalias StatementA (HSequential [':params String]))
       (defalias StatementB (HSequential [':no-params]))
       (defalias Statement (U StatementA StatementB))
       (fn [stmt :- Statement] :- Any
         (if (= :params (first stmt))
           (ann-form stmt StatementA))))))

  (testing "a mixture of types"
    (is-tc-e
     (do
       (defalias StatementA (HSeq [':params String]))
       (defalias StatementB (HVec [':no-params]))
       (defalias Statement (U StatementA StatementB))
       (fn [stmt :- Statement] :- Any
         (if (= :params (first stmt))
           (ann-form stmt StatementA)))))))
