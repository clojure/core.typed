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
