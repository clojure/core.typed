(ns clojure.core.typed.test.subtype
  (:require [clojure.core.typed :as tc :refer []]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.type-ctors :refer :all]
            [clojure.core.typed.type-rep :refer :all]
            [clojure.core.typed.parse-unparse :refer [parse-type]]
            [clojure.test :refer :all])
  (:import (clojure.lang Seqable)))

(deftest subtype-test
  (is-clj (sub? Integer Integer))
  (is-clj (sub? Integer Object))
  (is-clj (not (sub? Object Integer)))
  (is-clj (sub? Object Object))
  (is-clj (sub? Integer Number))
  (is-clj (sub? (clojure.lang.Seqable Integer)
                (clojure.lang.Seqable Integer)))
  (is-clj (sub? (clojure.lang.Seqable Integer)
                (clojure.lang.Seqable Number)))
  (is-clj (not
            (sub? (clojure.lang.Seqable Number)
                  (clojure.lang.Seqable Integer))))
  (is-clj (sub? (clojure.lang.Cons Integer)
                (clojure.lang.Cons Number)))
  (is-clj (sub? (clojure.lang.Cons Integer)
                (clojure.lang.Seqable Number))))

(deftest subtype-java-exceptions-test
  (is-clj (subtype? (RClass-of IndexOutOfBoundsException nil)
                    (RClass-of Exception nil))))

(deftest subtype-intersection
  (is-clj (not (subtype? (RClass-of Seqable [-any])
                     (In (RClass-of Seqable [-any])
                         (make-CountRange 1))))))

(deftest subtype-Object
  (is-clj (subtype? (RClass-of clojure.lang.IPersistentList [-any]) (RClass-of Object nil))))

(deftest subtype-hmap
  (is (sub? (HMap :mandatory {:a '1} :complete? true)
            (HMap :mandatory {:a Number} :complete? true)))
  (is (sub? (HMap :mandatory {:a '1} :complete? true)
            (HMap :mandatory {} :complete? false)))
  (is (sub? (HMap :mandatory {:a '1 :b '2 :c '3} :complete? true)
            (HMap :mandatory {:a '1 :b '2} :complete? false)))
  (is (not (sub? '{:a nil}
                 '{:a '1})))
  (is (not (sub? (HMap :mandatory {:a '1} :complete? true)
                 (HMap :mandatory {} :complete? true))))
  (is (not (sub? (HMap :mandatory {:a '1 :b '2} :complete? true)
                 (HMap :mandatory {:a '1 :b '2 :c '3} :complete? false)))))

(deftest subtype-poly
  (is-clj (subtype? (parse-type '(clojure.core.typed/All [x] (clojure.lang.ASeq x)))
                    (parse-type '(clojure.core.typed/All [y] (clojure.lang.Seqable y))))))

(deftest subtype-rec
  (is-clj (subtype? (parse-type 'Integer)
                    (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is-clj (subtype? (parse-type '(clojure.lang.Seqable (clojure.lang.Seqable Integer)))
                    (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is-clj (not (subtype? (parse-type 'Number)
                         (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x)))))))
  (is-clj (sub? (HMap :mandatory {:op (Value :if)
                  :test (HMap :mandatory {:op (Value :var)
                               :var (clojure.lang.Var Nothing clojure.core.typed/Any)})
                  :then (HMap :mandatory {:op (Value :nil)})
                  :else (HMap :mandatory {:op (Value :false)})})
            (Rec [x] 
                 (U (HMap :mandatory {:op (Value :if)
                           :test x
                           :then x
                           :else x})
                    (HMap :mandatory {:op (Value :var)
                           :var (clojure.lang.Var Nothing clojure.core.typed/Any)})
                    (HMap :mandatory {:op (Value :nil)})
                    (HMap :mandatory {:op (Value :false)})))))

  (is-clj (sub? (Rec [x] (U Integer (clojure.lang.ILookup x x)))
            (Rec [x] (U Number (clojure.lang.ILookup x x))))))

(deftest count-subtype-test
  (is-clj (subtype? (make-CountRange 1)
                (make-CountRange 1)))
  (is-clj (not (subtype? (make-CountRange 1)
                     (make-ExactCountRange 1))))
  (is-clj (subtype? (make-ExactCountRange 1)
                (make-CountRange 1)))
  (is-clj (subtype? (make-ExactCountRange 4)
                (make-CountRange 1)))
  (is-clj (subtype? (make-ExactCountRange 4)
                (make-CountRange 0)))
  (is-clj (subtype? (make-CountRange 2)
                (make-CountRange 1)))
  )

(deftest array-subtype-test
  (is-clj (sub? (Array int) (Array int)))
  (is-clj (sub? (Array int) (ReadOnlyArray int)))
  (is-clj (sub? (Array Long) (clojure.lang.Seqable Long)))
  ;FIXME
  ;(is-clj (not (sub? (Array Object) (clojure.lang.Seqable Long))))
  (is-clj (not (sub? (ReadOnlyArray int) (Array int)))))

(deftest top-function-subtype-test
  (is-clj (subtype? (parse-type '[clojure.core.typed/Any -> clojure.core.typed/Any])
                (parse-type 'AnyFunction))))

(deftest complete-hash-subtype-test
  (is-clj (sub? (HMap :optional {} :complete? true)
            (clojure.lang.IPersistentMap Integer Long))))

(deftest latent-filter-subtype-test 
  (is-clj (not (subtype? (parse-type '(Fn [clojure.core.typed/Any -> clojure.core.typed/Any :filters {:then (is Number 0)}]))
                         (parse-type '(Fn [clojure.core.typed/Any -> clojure.core.typed/Any :filters {:then (is Nothing 0)}]))))))

(deftest subtype-tfn-test
  (is-clj (sub? (TFn [[x :variance :covariant]] Number)
            (TFn [[x :variance :covariant]] clojure.core.typed/Any)))
  (is-clj (not (sub? (TFn [[x :variance :covariant]] clojure.core.typed/Any)
                 (TFn [[x :variance :covariant]] Number))))
  (is-clj (sub? (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)
            ((TFn [[x :variance :covariant]] (clojure.lang.IPersistentMap clojure.core.typed/Any clojure.core.typed/Any)) clojure.core.typed/Any))))
