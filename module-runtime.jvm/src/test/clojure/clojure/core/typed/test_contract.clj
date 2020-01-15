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
  (is (= (contract int-c 1)
         1))
  (is (thrown-blame? (contract int-c nil))))

(deftest ifn-test
  (is (= ((contract (ifn-c [int-c] int-c) (fn [x] x)) 1)
         1))
  (testing "violate input"
    (is (thrown-blame?
          ((contract (ifn-c [int-c] int-c) (fn [x] x)) nil))))
  (testing "violate output"
    (is (thrown-blame?
          ((contract (ifn-c [int-c] int-c) str) 1)))))

(deftest Object-c-test
  (is (= (contract Object-c 1) 
         1))
  (is (thrown-blame?
               (= (contract Object-c nil) 1))))

(deftest val-c-test
  (is (= (contract nil-c nil) 
         nil))
  (is (thrown-blame? (contract nil-c 1))))

;TODO
#_(deftest seqable-c-test
  (is (= (contract (seqable-c int-c) (list 1 2 3)) (list 1 2 3)))
  (is (thrown-blame? (doall (contract (seqable-c int-c) (list nil 2 3))))))

(deftest hmap-c-test
  (testing "no options"
    (is (= (contract (hmap-c) 
                     {})
           {}))
    (is (= (contract (hmap-c) 
                     {:a 1})
           {:a 1}))
    (is (= (contract (hmap-c)
                     {:a 1 :b 2})
           {:a 1 :b 2})))
  (testing "mandatory keys, partial map, no absent"
    (testing "flat '{:a Int}"
      (let [c (hmap-c :mandatory {:a int-c})]
        (testing "good value"
          (is (= (contract c {:a 1})
                 {:a 1})))
        (testing "extra entry ok"
          (is (= (contract c {:a 1 :b 2})
                 {:a 1 :b 2})))
        (testing "bad value"
          (is (thrown-blame?
                (contract c {:a nil}))))))
    (testing "higher-order '{:a [Int -> Int]}"
      (let [c (hmap-c :mandatory {:a (ifn-c [int-c] int-c)})]
        (testing "good value"
          (is (= ((:a (contract c
                                {:a inc}))
                  1)
                 2)))
        (testing "extra entry ok"
          (is (= ((:a (contract c
                                {:a inc
                                 :b 2}))
                  1)
                 2)))
        (testing "bad value"
          (testing "violate input"
            (is (thrown-blame?
                  ((:a (contract (hmap-c :mandatory {:a (ifn-c [int-c] int-c)})
                                 {:a inc}))
                   nil))))
          (testing "violate output"
            (is (thrown-blame?
                  ((:a (contract (hmap-c :mandatory {:a (ifn-c [int-c] int-c)})
                                 {:a str}))
                   1)))))))
    (testing "missing entry"
      (testing "flat"
        (is (thrown-blame? 
              (contract (hmap-c :mandatory {:a int-c})
                        {}))))
      (testing "higher-order"
        (is (thrown-blame? 
              (contract (hmap-c :mandatory {:a (ifn-c [int-c] int-c)})
                        {}))))))

  (testing "mandatory keys, partial map, no absent"
    (testing "flat (HMap :optional {:a Int})"
      (let [c (hmap-c :optional {:a int-c})]
        (testing "good value"
          (is (= (contract c {:a 1})
                 {:a 1}))
          (testing "extra entry ok"
            (is (= (contract c {:a 1 :b 2})
                   {:a 1 :b 2}))))
        (testing "bad value"
          (is (thrown-blame? 
                (contract c {:a nil}))))))
    (testing "higher-order (HMap :optional {:a [Int -> Int]})"
      (let [c (hmap-c :optional {:a (ifn-c [int-c] int-c)})]
        (testing "good value"
          (is (= ((:a (contract c
                                {:a inc}))
                  1)
                 2)))
        (testing "extra entry ok"
          (is (= ((:a (contract c
                                {:a inc
                                 :b 1}))
                  1)
                 2)))
        (testing "bad value"
          (testing "violate input"
            (is (thrown-blame?
                  ((:a (contract c
                                 {:a inc}))
                   nil))))
          (testing "violate output"
            (is (thrown-blame?
                  ((:a (contract c
                                 {:a str}))
                   1)))))))
    (testing "missing entry"
      (testing "flat"
        (is (= (contract (hmap-c :optional {:a int-c})
                         {}))))
      (testing "higher-order"
        (is (= (contract (hmap-c :optional {:a (ifn-c [int-c] int-c)})
                         {})
               {})))))
  (testing "mandatory keys, complete map"
    (testing "flat (HMap :complete? true, :mandatory {:a Int})"
      (let [c (hmap-c 
                :complete? true
                :mandatory {:a int-c})]
        (testing "extra entry bad"
          (is (thrown-blame?
                (contract c {:a 1 :b 2}))))))
    (testing "higher-order (HMap :complete? true :mandatory {:a [Int -> Int]})"
      (let [c (hmap-c 
                :complete? true
                :mandatory {:a (ifn-c [int-c] int-c)})]
        (testing "extra entry bad"
          (is (thrown-blame?
                (contract c {:a inc :b 2})))))))
  (testing "optional keys, complete map"
    (testing "flat (HMap :complete? true, :mandatory {:a Int})"
      (let [c (hmap-c 
                :complete? true
                :optional {:a int-c})]
        (testing "missing optional ok"
          (is (= (contract c {})
                 {})))
        (testing "extra entry bad"
          (is (thrown-blame?
                (contract c {:a 1 :b 2})))
          (is (thrown-blame?
                (contract c {:b 2}))))))
    (testing "higher-order (HMap :optional {:a [Int -> Int]})"
      (let [c (hmap-c 
                :complete? true
                :mandatory {:a (ifn-c [int-c] int-c)})]
        (testing "extra entry bad"
          (is (thrown-blame?
                (contract c {:a inc :b 2})))))))
  (testing "absent keys"
    (testing "flat (HMap :absent-keys #{:a} :mandatory {:b Int})"
      (let [c (hmap-c 
                :mandatory {:b int-c}
                :absent-keys #{:a})]
        (testing "absent ok"
          (is (= (contract c {:b 1})
                 {:b 1})))
        (testing "present bad"
          (is (thrown-blame?
                (contract c {:a 1 :b 2}))))
        (testing "missing mandatory"
          (is (thrown-blame?
                (contract c {}))))))
    (testing "higher-order (HMap :optional {:a [Int -> Int]})"
      (let [c (hmap-c 
                :absent-keys #{:a}
                :mandatory {:b (ifn-c [int-c] int-c)})]
        (testing "absent ok"
          (is ((:b (contract c {:b inc}))
               1)))
        (testing "present bad"
          (is (thrown-blame?
                (contract c {:a 1 :b inc}))))
        (testing "missing mandatory"
          (is (thrown-blame?
                (contract c {}))))))
    (testing "with complete"
      (let [c (hmap-c 
                :complete? true
                :absent-keys #{:a}
                :mandatory {:b int-c})]
        (testing "fine"
          (is (= (contract c {:b 1})
                 {:b 1})))
        (testing "missing mandatory"
          (is (thrown-blame?
                (contract c {}))))
        (testing "bad absent"
          (is (thrown-blame?
                (contract c {:a 1}))))
        (testing "bad complete"
          (is (thrown-blame?
                (contract c {:c 1}))))))))
