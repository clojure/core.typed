(ns clojure.core.typed.test-contract
  (:require [clojure.core.typed.contract :as con
             :refer :all]
            [clojure.test :refer :all]))

(defmacro thrown-blame? [& e]
  `(try (do ~@e)
        false
        (catch clojure.lang.ExceptionInfo e#
          (boolean (-> e# ex-data :blame)))))

(deftest int-c-test
  (is (= (contract int-c 1) 1))
  (is (thrown-blame? (contract int-c nil))))

(deftest ifn-test
  (is (= ((contract (ifn-c [int-c] int-c) (fn [x] x)) 1)
         1))
  (is (thrown-blame?
               ((contract (ifn-c [int-c] int-c) (fn [x] x)) nil))))

(deftest Object-c-test
  (is (= (contract Object-c 1) 1))
  (is (thrown-blame?
               (= (contract Object-c nil) 1))))

(deftest val-c-test
  (is (= (contract nil-c nil) nil))
  (is (thrown-blame? (contract nil-c 1))))

;TODO
#_(deftest seqable-c-test
  (is (= (contract (seqable-c int-c) (list 1 2 3)) (list 1 2 3)))
  (is (thrown-blame? (doall (contract (seqable-c int-c) (list nil 2 3))))))
