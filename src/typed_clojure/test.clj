(ns typed-clojure.test
  (:use [typed-clojure.core]
        [typed-clojure.types]))

;(T test-typed-def-fn :- (+fn [+integer -> +integer]))
;(def test-typed-def-fn
;  (fn-T ([[a :- +integer]] 1)))

(T test-typed-def :- +integer)
(def test-typed-def
  1)

;(T ^{:type (Fn [IntegerT :-> IntegerT]
;               [IntegerT IntegerT :-> IntegerT])} 'asdf)
;(defn asdf
;  ([a] 1)
;  ([a b] 3))
;
;(T ^{:type IntegerT} 'a)
;(def a 1)
;
;(T ^{:type (Fn [IntegerT :-> IntegerT])} 'test-typed-def-fn)
;(def test-typed-def-fn
;  (fn-T 
;    ([[a :- IntegerT]] 1)))
;
;(T ^{:type (Fn [NumberT :-> NumberT])} 'clojure.core/inc)
;(T ^{:type (Fn [NumberT :-> NumberT])} 'clojure.core/dec)
;
;;(T test-inc-dec :- (Fn [(Fn [NumberT :-> NumberT])
;;                        (Fn [NumberT :-> NumberT])
;;                        NumberT
;;                        :-> NumberT]))
;;(defn test-inc-dec [fn1 fn2 n]
;;  (fn1 (fn2 n)))
;
;;(type-check-form
;;  (test-inc-dec inc dec 2))
;
;(T ^{:type IntegerT} 'call-other-fn)
;(def call-other-fn
;  (test-typed-def-fn 1))
