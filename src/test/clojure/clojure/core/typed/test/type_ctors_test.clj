(ns clojure.core.typed.test.type-ctors-test
  (:require [clojure.test :refer :all]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.type-ctors :refer :all]
            [clojure.core.typed.type-rep :refer :all]
            [clojure.core.typed.parse-unparse :refer [parse-type]]))

(defmacro overlap-prs [s1 s2]
  `(clj
     (overlap (parse-type '~s1) (parse-type '~s2))))

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
    (not (overlap-prs Integer clojure.lang.Keyword)))
  (is-clj
    (not
      (overlap-prs
        (HMap :mandatory {:a Integer})
        (HMap :mandatory {:a clojure.lang.Keyword}))))
  (is-clj
    (overlap-prs
      (HMap :optional {:a Integer})
      (HMap :optional {:a clojure.lang.Keyword})))
  (is-clj
    (overlap-prs
      (HMap :complete? true :optional {:a Integer})
      (HMap :complete? true :optional {:a clojure.lang.Keyword}))))
