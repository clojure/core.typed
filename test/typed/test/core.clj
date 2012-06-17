(ns typed.test.core
  (:refer-clojure :exclude [defrecord])
  (:import (clojure.lang Seqable ISeq ASeq))
  (:require [clojure.test :refer :all]
            [typed.core :refer :all]))

(deftest add-scopes-test
  (is (let [body (make-F 'a)]
        (= (add-scopes 0 body)
           body)))
  (is (let [body (make-F 'a)]
        (= (add-scopes 1 body)
           (->Scope body))))
  (is (let [body (make-F 'a)]
        (= (add-scopes 3 body)
           (-> body ->Scope ->Scope ->Scope)))))

(deftest remove-scopes-test
  (is (let [scope (->Scope (make-F 'a))]
        (= (remove-scopes 0 scope)
           scope)))
  (is (let [body (make-F 'a)]
        (= (remove-scopes 1 (->Scope body))
           body))))

(deftest name-to-test
  (is (let [body (make-F 'a)]
        (= (name-to body 'a 0)
           (->B 0 (->Top) (Bottom))))))

(deftest parse-type-test
  (is (= (Poly-body* '(x) (parse-type '(All [x] x)))
         (make-F 'x)))
  (is (= (Poly-body* '(x y) (parse-type '(All [x y] x)))
         (make-F 'x)))
  (is (= (Poly-body* '(x y) (parse-type '(All [x y] y)))
         (make-F 'y)))
  (is (= (Poly-body* '(a b c d e f g h i) (parse-type '(All [a b c d e f g h i] e)))
         (make-F 'e))))

(deftest subtype-test
  (is (subtype (parse-type 'Integer)
               (parse-type 'Integer)))
  (is (subtype (parse-type 'Integer)
               (parse-type 'Number)))
  (is (subtype (parse-type '(clojure.lang.Seqable Integer))
               (parse-type '(clojure.lang.Seqable Integer))))
  (is (subtype (parse-type '(clojure.lang.Seqable Integer))
               (parse-type '(clojure.lang.Seqable Number))))
  (is (thrown? Exception
               (subtype (parse-type '(clojure.lang.Seqable Number))
                        (parse-type '(clojure.lang.Seqable Integer)))))
  (is (subtype (parse-type '(clojure.lang.Cons Integer))
               (parse-type '(clojure.lang.Cons Number))))
  (is (subtype (parse-type '(clojure.lang.Cons Integer))
               (parse-type '(clojure.lang.Seqable Number)))))

(deftest subtype-rec
  (is (subtype? (parse-type 'Integer)
                (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is (subtype? (parse-type '(clojure.lang.Seqable (clojure.lang.Seqable Integer)))
                (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))
  (is (not (subtype? (parse-type 'Number)
                     (parse-type '(Rec [x] (U Integer (clojure.lang.Seqable x))))))))

; expanding dotted pretypes
(deftest trans-dots-test
  (is (= (manual-inst (parse-type '(All [x b ...]
                                        [x ... b -> x]))
                      (map parse-type '(Integer Double Float)))
         (parse-type '[Integer Integer -> Integer])))
  (is (= (manual-inst (parse-type '(All [x b ...]
                                        [b ... b -> x]))
                      (map parse-type '(Integer Double Float)))
         (parse-type '[Double Float -> Integer])))
  ;map type
  (is (= (manual-inst (parse-type '(All [c a b ...]
                                        [[a b ... b -> c] (clojure.lang.Seqable a) (clojure.lang.Seqable b) ... b -> (clojure.lang.Seqable c)]))
                      (map parse-type '(Integer Double Float)))
         (parse-type '[[Double Float -> Integer] (clojure.lang.Seqable Double) (clojure.lang.Seqable Float) -> (clojure.lang.Seqable Integer)]))))

(deftest tc-invoke-fn-test
  (is (subtype? (tc-t 
                  ((typed.core/fn> [[a :- Number] [b :- Number]] b)
                     1 2))
                (parse-type 'Number)))
  (is (subtype? (tc-t 
                  ((typed.core/fn> [[a :- (clojure.lang.Seqable Number)] [b :- Number]] 
                                  ((typed.core/inst seq Number) a))
                     [1 2 1.2] 1))
                (parse-type '(U nil (clojure.lang.ASeq Number)))))
  (is (subtype? (tc-t 
                  ((typed.core/fn> [[a :- (clojure.lang.IPersistentMap Any Number)] [b :- Number]] 
                                  ((typed.core/inst get Number) a b))
                     {:a 1} 1))
                (parse-type '(U nil Number)))))
