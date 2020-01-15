(ns clojure.core.typed.test.nthnext-test
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t :refer [ann-form print-env fn]]))

(deftest nthnext-test
  (is-tc-e
   (fn [stmt :- '[Any Long]]
     (let [body (nthnext stmt 1)]
       (ann-form body (HSeq [Long])))))

  (is-tc-e
   (ann-form (nthnext nil 1) nil)))

(deftest nthnext-test-input-types
  (testing "HVec"
    (is-tc-e
     (fn [stmt :- (t/HVec [Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (HSeq [Long]))))))

  (testing "HSeq"
    (is-tc-e
     (fn [stmt :- (t/HSeq [Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (HSeq [Long]))))))

  (testing "HSequential"
    (is-tc-e
     (fn [stmt :- (t/HSequential [Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (HSeq [Long])))))))

(deftest next-test-input-types
  (testing "HVec"
    (is-tc-e
     (fn [stmt :- (t/HVec [Any Long])]
       (let [body (next stmt)]
         (ann-form body (HSeq [Long]))))))

  (testing "HSeq"
    (is-tc-e
     (fn [stmt :- (t/HSeq [Any Long])]
       (let [body (next stmt)]
         (ann-form body (HSeq [Long]))))))

  (testing "HSequential"
    (is-tc-e
     (fn [stmt :- (t/HSequential [Any Long])]
       (let [body (next stmt)]
         (ann-form body (HSeq [Long])))))))

(deftest seq-test-input-types
  (testing "HVec"
    (is-tc-e
     (fn [stmt :- (t/HVec [Any Long])]
       (let [body (seq stmt)]
         (ann-form body (HSeq [Any Long]))))))

  (testing "HSeq"
    (is-tc-e
     (fn [stmt :- (t/HSeq [Any Long])]
       (let [body (seq stmt)]
         (ann-form body (HSeq [Any Long]))))))

  (testing "HSequential"
    (is-tc-e
     (fn [stmt :- (t/HSequential [Any Long])]
       (let [body (seq stmt)]
         (ann-form body (HSeq [Any Long])))))))

(deftest nthnext-test-fixed-types
  (testing "skipping past all the fixed types"
    (is-tc-e
     (fn [stmt :- '[Any Long]]
       (let [body (nthnext stmt 100)]
         (ann-form body nil))))
    (is-tc-err
     (fn [stmt :- '[Any Long]]
       (let [body (nthnext stmt 100)]
         (ann-form body (HSeq [])))))))

(deftest nthnext-test-fixed-types-and-rest
  (testing "skipping past all the fixed types with a rest type"
    (is-tc-e
     (fn [stmt :- '[Any Long *]]
       (let [body (nthnext stmt 100)]
         (ann-form body (t/Option (HSeq [Long *]))))))
    (is-tc-err
     (fn [stmt :- '[Any Long *]]
       (let [body (nthnext stmt 100)]
         (ann-form body (HSeq [Long *])))))
    (is-tc-err
     (fn [stmt :- '[Any Long *]]
       (let [body (nthnext stmt 100)]
         (ann-form body nil))))))

(deftest nthnext-test-union
  (testing "a union of types is also refined"
    (is-tc-e
     (fn [stmt :- (U '[Any String]
                     '[Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (U (HSeq [String])
                           (HSeq [Long]))))))))

(deftest nthnext-test-destructuring
  (testing "implicitly used via destructuring"
    (is-tc-e
     (fn [stmt :- '[Any Long]]
       (let [[_ & body] stmt]
         (ann-form body (HSeq [Long]))))))

  (testing "destructuring past fixed types"
    (is-tc-e
     (fn [stmt :- '[Any]]
       (let [[_ & body] stmt]
         (ann-form body nil)))))

  (testing "destructuring past fixed types with a rest type"
    (is-tc-e
     (fn [stmt :- '[Any Long *]]
       (let [[_ & body] stmt]
         (ann-form body (t/Option (HSeq [Long *]))))))))
