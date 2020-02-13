(ns clojure.core.typed.test.namespaced-specials
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]))

;backwards compatibility tests for type syntax

(deftest value-test
  (is-tc-e (ann-form :a ':a))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form :a (Value :a)))
  (is-tc-e (ann-form :a (Value :a)))
  (is-tc-e (ann-form :a (t/Value :a)))
  ; runtime parsing
  (is-tc-e (defalias Foo (t/Value :a)))
  (is-tc-e (defalias Foo (Value :a)))
  ; old syntax
  (is-cf (clojure.core.typed/defalias Foo (Value :a))))

(deftest quote-test
  ; bare quote should always resolve to clojure.core/quote
  (is-tc-e (do (defalias quote Any)
               (defalias TAlias (quote :a))
               (ann-form :a TAlias)))
  (is-tc-e (do (defalias quote Any)
               (defalias TAlias (quote {:a Num}))
               (ann-form {:a 1} TAlias))))

(deftest hmap-test
  (is-tc-e (ann-form {:a 1} (HMap :mandatory {:a Num})))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form {:a 1} (HMap :mandatory {:a clojure.core.typed/Num})))
  ;new syntax
  (is-tc-e (ann-form {:a 1} (t/HMap :mandatory {:a Num}))))

(deftest hvec-test
  (is-tc-e (ann-form [1] (HVec [Num])))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form 
           [1]
           (HVec [clojure.core.typed/Num])))
  ;new syntax
  (is-tc-e (ann-form 
             [1]
             (t/HVec [Num]))))

(deftest Any-test
  (is-tc-e (ann-form {:a 1} Any))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form {:a 1} Any))
  ; new syntax
  (is-tc-e (ann-form {:a 1} t/Any)))

(deftest Nothing-test
  (is-tc-e (ann-form (fn [] (throw (Exception.))) 
                     [-> Nothing]))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form
           (fn [] (throw (Exception.))) 
           [-> Nothing]))
  ; new syntax
  (is-tc-e (ann-form 
             (fn [] (throw (Exception.))) 
             [-> t/Nothing])))
