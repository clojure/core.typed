(ns clojure.core.typed.test.overlap
  (:require [clojure.test :refer :all]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.typed.checker.type-ctors :refer :all]
            [clojure.core.typed.checker.type-rep :refer :all]
            [clojure.core.typed.checker.jvm.parse-unparse :refer [parse-type]]))

(defmacro overlap-prs [s1 s2]
  `(clj
     (overlap (parse-type ~s1) (parse-type ~s2))))

(deftest overlap-test
  (is-clj (not (overlap -false -true)))
  (is-clj (not (overlap (-val :a) (-val :b))))
  (is-clj (overlap (RClass-of Number) (RClass-of Integer)))
  (is-clj (not (overlap (RClass-of Number) (RClass-of clojure.lang.Symbol))))
  (is-clj (not (overlap (RClass-of Number) (RClass-of String))))
  (is-clj (overlap (RClass-of clojure.lang.Seqable [-any]) (RClass-of clojure.lang.IMeta)))
  (is-clj (overlap (RClass-of clojure.lang.Seqable [-any]) (RClass-of clojure.lang.PersistentVector [-any]))))

(deftest hmap-overlap-test
  (is-clj
    (not (overlap-prs `t/Int `t/Kw)))
  (is-clj
    (not
      (overlap-prs
        `(t/HMap :mandatory {:a t/Int})
        `(t/HMap :mandatory {:a t/Kw}))))
  (is-clj
    (overlap-prs
      `(t/HMap :optional {:a t/Int})
      `(t/HMap :optional {:a t/Kw})))
  (is-clj
    (overlap-prs
      `(t/HMap :complete? true :optional {:a t/Int})
      `(t/HMap :complete? true :optional {:a t/Kw}))))

(deftest hvec-overlap-test
  (testing "without rest types"
    (testing "when the fixed types match"
      (is-clj
       (overlap-prs
        `(t/HVec [t/Num])
        `(t/HVec [t/Num]))))

    (testing "when the fixed types differ"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num])
         `(t/HVec [t/Str])))))

    (testing "with a differing number of fixed types"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num])
         `(t/HVec [t/Num t/Str]))))))

  (testing "with one rest type"
    (testing "when fixed types match"
      (is-clj
       (overlap-prs
        `(t/HVec [t/Num])
        `(t/HVec [t/Num t/Str ~'*]))))

    (testing "when fixed types differ"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num])
         `(t/HVec [t/Str t/Str ~'*])))))

    (testing "when the extra fixed types match the rest type"
      (is-clj
       (overlap-prs
        `(t/HVec [t/Num ~'*])
        `(t/HVec [t/Num]))))

    (testing "when the extra fixed types differ from the rest type"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num ~'*])
         `(t/HVec [t/Str])))))

    (testing "when the extra fixed types come from type with the rest type"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Str t/Str t/Str ~'*])
         `(t/HVec [t/Str]))))))

  (testing "with two rest types"
    (testing "when the rest types match"
      (is-clj
       (overlap-prs
        `(t/HVec [t/Num ~'*])
        `(t/HVec [t/Num ~'*]))))

    (testing "when the rest types differ"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num ~'*])
         `(t/HVec [t/Str ~'*])))))

    (testing "when the extra fixed types match the rest type of shorter"
      (is-clj
       (overlap-prs
        `(t/HVec [t/Num ~'*])
        `(t/HVec [t/Num t/Num ~'*]))))

    (testing "when the extra fixed types differ from the rest type of shorter"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num ~'*])
         `(t/HVec [t/Str t/Num ~'*])))))

    (testing "when the fixed types match"
      (is-clj
       (overlap-prs
        `(t/HVec [t/Num t/Str ~'*])
        `(t/HVec [t/Num t/Str ~'*]))))

    (testing "when the fixed types differ"
      (is-clj
       (not
        (overlap-prs
         `(t/HVec [t/Num t/Str ~'*])
         `(t/HVec [t/Str t/Str ~'*])))))))

(deftest hvec-complex-overlap
  (is-clj (overlap-prs `(t/HVec [t/Int t/Num])
                       `(t/HVec [t/Num t/Int]))))

(deftest overlap-free-test
  (is-clj (overlap (make-F 'a)
                   (-val 'a)))
  (is-clj (overlap (-val 'a)
                   (make-F 'a)))
  (is-clj (overlap (make-F 'b)
                   (make-F 'a))))
