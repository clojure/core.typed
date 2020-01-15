(ns clojure.core.typed.test.difference
  (:require [clojure.core.typed :as tc :refer []]
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.checker.type-rep :refer :all]
            [clojure.core.typed.checker.type-ctors :refer :all]
            [clojure.core.typed.checker.cs-gen :refer :all]
            [clojure.core.typed.checker.cs-rep :refer :all]
            [clojure.core.typed.checker.subst :refer [subst-all]]
            [clojure.test :refer :all]))

;; When Difference is ready, uncomment tests at the bottom

(deftest not-type-subtype
  (is-clj (not (subtype? (NotType-maker -nil) -nil)))
  (is-clj (subtype? (NotType-maker -nil) (NotType-maker -nil)))
  (is-clj (not (subtype? (NotType-maker -nil) (NotType-maker -false))))
  (is-clj (subtype? (NotType-maker (Un -false -nil)) (NotType-maker -false))))

(deftest Not-combine-test
  (is-clj (= (In -nil (NotType-maker -nil))
             (Un)))
  (is-clj (clj
            (= (In (RClass-of Integer) (NotType-maker (RClass-of Number)))
               (Un))))

  (is-clj (not (overlap (RClass-of Integer) (NotType-maker (RClass-of Number)))))
  (is-clj (clj (overlap (RClass-of Number) (NotType-maker -nil))))

  (is-clj (overlap (NotType-maker (RClass-of Number))
                   (NotType-maker (RClass-of Integer))))

  (is-clj (not (overlap (NotType-maker (RClass-of Number)) 
                        (RClass-of Integer))))

  (is-clj (= (In (RClass-of Number) (NotType-maker -nil))
             (RClass-of Number)))

  (is-clj (= (In (Un (RClass-of Number) -nil) -nil)
             -nil))
  (is-clj (= (In (Un (RClass-of Number) -nil) (NotType-maker -nil))
             (RClass-of Number)))
  (is-clj (= (In (RClass-of Number) (NotType-maker -nil))
             (RClass-of Number)))

  (is-clj (clj (= (In (Un -nil (RClass-of Number)) (NotType-maker (Un -false -nil)))
                  (RClass-of Number))))

  (is-clj (overlap (Un -nil (RClass-of Number)) (NotType-maker (Un -false -nil))))

  (is-clj (= (In (Un -nil (RClass-of Number)) (NotType-maker -nil))
             (RClass-of Number)))
  (is-clj (= (Un (In -nil (NotType-maker -nil))
                 (In (RClass-of Number) (NotType-maker -nil)))
             (RClass-of Number)))

  (is-clj (= (In (NotType-maker (RClass-of Number))
                 (NotType-maker (RClass-of Integer)))
             (NotType-maker (RClass-of Number))))

  (is-clj (subtype? (RClass-of Number) (NotType-maker -nil)))
  (is-clj (subtype? (RClass-of Number) (NotType-maker (RClass-of Integer))))
  (is-clj (not (subtype? (RClass-of Integer) (NotType-maker (RClass-of Number)))))
  (is-clj (not (subtype? (NotType-maker -nil) (RClass-of Number))))

  (is-clj (= (let [i (subst-all {'x (t-subst-maker (Un (RClass-of Number) -nil) no-bounds) 
                                 'y (t-subst-maker -nil no-bounds)} 
                                (In (make-F 'x) (NotType-maker (make-F 'y))))
                   _ (assert (Intersection? i))]
               (apply In (:types i)))
             (RClass-of Number)))

  (is-clj (overlap (make-F 'x)
                   (NotType-maker (make-F 'y))))
  (is-clj (overlap (B-maker 0)
                   (NotType-maker (B-maker 1))))
  (is-clj (not (subtype? (B-maker 0)
                         (NotType-maker (B-maker 1)))))
  (is-clj (not= (In (make-F 'x)
                    (NotType-maker (make-F 'y)))
                (Un))))

;(deftest difference-type-subtype
;  (is-clj (not (sub? (Difference Any nil) nil)))
;  (is-clj (sub? (Difference Any nil) (Difference Any nil)))
;  (is-clj (overlap -any (NotType-maker -nil)))
;  (is-clj (not (sub? (Difference Any nil) (Difference Any false))))
;  (is-clj (sub? (Difference Any (U false nil)) (Difference Any false)))
;  (is-clj (sub? (Difference Any false nil) (Difference Any false)))
;  (is-clj (sub? (Difference (U Long Float) Float) Long)))
;
;(deftest Difference-combine-test
;  (is-clj (= (-difference -nil (NotType-maker -nil))
;             (Un)))
;  (is-clj (clj
;            (= (-difference (RClass-of Integer) (RClass-of Number))
;               (Un))))
;
;  (is-clj (not (overlap (RClass-of Integer) (-difference -any (RClass-of Number)))))
;  (is-clj (clj (overlap (RClass-of Number) (-difference -any -nil))))
;
;  (is-clj (overlap (-difference -any (RClass-of Number))
;                   (-difference -any (RClass-of Integer))))
;
;  (is-clj (not (overlap (-difference -any (RClass-of Number))
;                        (RClass-of Integer))))
;
;  (is-clj (= (-difference (RClass-of Number) -nil)
;             (RClass-of Number)))
;
;  (is-clj (= (-difference (Un (RClass-of Number) -nil) -nil)
;             (RClass-of Number)))
;  (is-clj (= (-difference (RClass-of Number) -nil)
;             (RClass-of Number)))
;
;  (is-clj (clj (= (-difference (Un -nil (RClass-of Number)) (Un -false -nil))
;                  (RClass-of Number))))
;
;  (is-clj (overlap (Un -nil (RClass-of Number)) 
;                   (-difference -any (Un -false -nil))))
;
;  (is-clj (= (-difference (Un -nil (RClass-of Number)) -nil)
;             (RClass-of Number)))
;  (is-clj (= (Un (-difference -nil -nil)
;                 (-difference (RClass-of Number) -nil))
;             (RClass-of Number)))
;
;  (is-clj (= (In (-difference -any (RClass-of Number))
;                 (-difference -any (RClass-of Integer)))
;             (-difference -any (RClass-of Number))))
;
;  (is-clj (subtype? (RClass-of Number) (-difference -any -nil)))
;  (is-clj (subtype? (RClass-of Number) (-difference -any (RClass-of Integer))))
;  (is-clj (not (subtype? (RClass-of Integer) (-difference -any (RClass-of Number)))))
;  (is-clj (not (subtype? (-difference -any -nil) (RClass-of Number))))
;
;  (is-clj (= (let [i (subst-all {'x (->t-subst (Un (RClass-of Number) -nil) no-bounds) 
;                                 'y (->t-subst -nil no-bounds)} 
;                                (-difference (make-F 'x) (make-F 'y)))
;                   _ (assert (Intersection? i))]
;               (apply In (:types i)))
;             (RClass-of Number)))
;
;  (is-clj (overlap (make-F 'x)
;                   (-difference -any (make-F 'y))))
;  (is-clj (overlap (B-maker 0)
;                   (-difference -any (B-maker 1))))
;  (is-clj (not (subtype? (B-maker 0)
;                         (-difference -any (B-maker 1)))))
;  (is-clj (not= (-difference (make-F 'x)
;                             (make-F 'y))
;                (Un))))
;
;(deftest negative-filter-test
;  (is-tc-e ;need to instantiate negative types for now
;         (fn [a] 
;           ((inst a (U nil Number) nil)
;            (inst identity (U nil Number)) 
;            [1 nil]))
;         :expected
;         [(All [x y]
;               [[x -> Any :filters {:then (! y 0)}]
;                (Seqable x) -> (Seq (Difference x y))])
;          -> (Seqable Number)]))
  #_(is-tc-e (let [filter (ann-form filter
                                  (All [x y]
                                       [[x -> Any :filters {:then (! y 0)}] 
                                        (U nil (Seqable x)) -> (Seq (Difference x y))]))]
             (filter (inst identity (U nil Number)) [1 nil]))
           :expected (Seqable Number))
