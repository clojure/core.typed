(ns typed-clojure.test
  (:use [typed-clojure.core]
        [typed-clojure.types]))

;(type-check-namespace 'typed-clojure.test)

;(T test-typed-def-fn :- (+Fun [+integer -> +integer]))
;(def test-typed-def-fn
;  (fn-T ([[a :- +integer]] 1)))

(+T clojure.core/+ :- (+Fun [+number +number -> +number]))
(+T clojure.core/- :- (+Fun [+number +number -> +number]))

(+T test-typed-def :- +number)
(+def test-typed-def
  (+let [[a :- +number] (- 2 (+ 1 1))
         [[b :- +integer]] [2]]
    (+ a b)))

        

(comment
  (+T test-wrong-def :- +integer)
  (+def test-typed-def
    1.1)
  )

(+let [[a :- +integer] 1]
  a)

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
