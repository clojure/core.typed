(ns typed-clojure.test
  (:use [typed-clojure.core]))

(T test-typed-def-fn :- (Fn (IntegerT -> IntegerT)))
(def test-typed-def-fn
  (fn-T ([[a :- IntegerT]] 1)))

(T asdf :- (Fn (IntegerT -> IntegerT)
               (IntegerT IntegerT -> IntegerT)))
(defn asdf
  ([a] 1)
  ([a b] 3))

(T a :- IntegerT)
(def a 1)

(T test-typed-def-fn :- (Fn (IntegerT -> IntegerT)))
(def test-typed-def-fn
  (fn-T 
    ([[a :- IntegerT]] 1)))

(T clojure.core/inc :- (Fn (NumberT -> NumberT)))
(T clojure.core/dec :- (Fn (NumberT -> NumberT)))

(T test-inc-dec :- (Fn ((Fn (NumberT -> NumberT))
                        (Fn (NumberT -> NumberT))
                        NumberT
                        -> NumberT)))
(defn test-inc-dec [fn1 fn2 n]
  (fn1 (fn2 n)))

;(type-check-form
;  (test-inc-dec inc dec 2))

(T call-other-fn :- IntegerT)
(def call-other-fn
  (test-typed-def-fn 1))
