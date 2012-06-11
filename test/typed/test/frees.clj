(ns typed.test.frees
  (:refer-clojure :exclude [defrecord])
  (:import (clojure.lang Seqable ISeq ASeq))
  (:use [clojure.test]
        [typed.new]))

(deftest add-scopes-test
  (is (let [body (->F 'a nil nil nil)]
        (= (add-scopes 0 body)
           body)))
  (is (let [body (->F 'a nil nil nil)]
        (= (add-scopes 1 body)
           (->Scope body))))
  (is (let [body (->F 'a nil nil nil)]
        (= (add-scopes 3 body)
           (-> body ->Scope ->Scope ->Scope)))))

(deftest remove-scopes-test
  (is (let [scope (->Scope (->F 'a nil nil nil))]
        (= (remove-scopes 0 scope)
           scope)))
  (is (let [body (->F 'a nil nil nil)]
        (= (remove-scopes 1 (->Scope body))
           body))))

(deftest name-to-test
  (is (let [body (->F 'a nil nil nil)]
        (= (name-to body 'a 0)
           (->B 0 nil nil nil)))))

(deftest parse-type-test
  (is (= (Poly-body* '(x) (parse-type '(All [x] x)))
         (->F 'x nil nil nil)))
  (is (= (Poly-body* '(x y) (parse-type '(All [x y] x)))
         (->F 'x nil nil nil)))
  (is (= (Poly-body* '(x y) (parse-type '(All [x y] y)))
         (->F 'y nil nil nil)))
  (is (= (Poly-body* '(a b c d e f g h i) (parse-type '(All [a b c d e f g h i] e)))
         (->F 'e nil nil nil))))

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

(deftest tc-invoke-fn-test
  (is (subtype? (-> (tc 
                      ((typed.new/fn> [[a :- Number] [b :- Number]] b)
                         1 2)) 
                  expr-type)
                (parse-type 'Number)))
  (is (subtype? (-> (tc 
                      ((typed.new/fn> [[a (clojure.lang.Seqable Number)] [b Number]] ((typed.new/inst seq Number) a))
                         [1 2 1.2] 1))
                  expr-type)
                (parse-type '(U nil (clojure.lang.ASeq Number)))))
  (is (subtype? (-> (tc 
                      ((typed.new/fn> [[a (clojure.lang.IPersistentMap Any Number)] [b Number]] ((typed.new/inst get Number) a b))
                         {:a 1} 1))
                  expr-type)
                (parse-type '(U nil Number)))))
