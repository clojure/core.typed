(ns typed-clojure.test
  (:use [typed-clojure.core]
        [typed-clojure.types]))

;(type-check-namespace 'typed-clojure.test)

;(T test-typed-def-fn :- (+Fun [+integer -> +integer]))
;(def test-typed-def-fn
;  (fn-T ([[a :- +integer]] 1)))

(+T clojure.core/list :- (IPersistentList b) => a * -> b)

(+T clojure.core/cons :- (ISeq c) => a nil -> c)
(+T clojure.core/cons :- (ISeq b) (ISeq c) => a b -> c)
(+T clojure.core/cons :- (Seqable b) (ISeq c) => a b -> c)
(+T clojure.core/cons :- (Iterable b) (ISeq c) => a b -> c)
(+T clojure.core/cons :- (PrimArray b) (ISeq c) => a b -> c)
(+T clojure.core/cons :- (CharSequence b) (ISeq c) => a b -> c)
(+T clojure.core/cons :- (Map b) (ISeq c) => a b -> c)

(+T clojure.core/first :- (ISeq a) => a -> b)
(+T clojure.core/first :- nil -> nil)
(+T clojure.core/first :- (Seqable a) => a -> b)
(+T clojure.core/first :- (Iterable a) => a -> b)
(+T clojure.core/first :- (PrimArray a) => a -> b)
(+T clojure.core/first :- (CharSequence a) => a -> b)
(+T clojure.core/first :- (Map a) => a -> b)

(+T clojure.core/next :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/next :- (ISeq a) => a -> nil)
(+T clojure.core/next :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/next :- (Seqable a) => a -> nil)
(+T clojure.core/next :- nil -> nil)
(+T clojure.core/next :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/next :- (Iterable a) => a -> nil)
(+T clojure.core/next :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/next :- (PrimArray a) => a -> nil)
(+T clojure.core/next :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/next :- (CharSequence a) => a -> nil)
(+T clojure.core/next :- (Map a) (ISeq b) => a -> b)
(+T clojure.core/next :- (Map a) => a -> nil)

(+T clojure.core/rest :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/rest :- (ISeq a) => nil -> a)
(+T clojure.core/rest :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/rest :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/rest :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/rest :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/rest :- (Map a) (ISeq b) => a -> b)

(+T clojure.core/conj :- (IPersistentCollection a) => a b b * -> a)
(+T clojure.core/conj :- (IPersistentCollection a) => nil b b * -> a)

(+T clojure.core/second :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/second :- (ISeq a) => a -> nil)
(+T clojure.core/second :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/second :- (Seqable a) => a -> nil)
(+T clojure.core/second :- nil -> nil)
(+T clojure.core/second :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/second :- (Iterable a) => a -> nil)
(+T clojure.core/second :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/second :- (PrimArray a) => a -> nil)
(+T clojure.core/second :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/second :- (CharSequence a) => a -> nil)
(+T clojure.core/second :- (Map a) (ISeq b) => a -> b)
(+T clojure.core/second :- (Map a) => a -> nil)

(+T clojure.core/ffirst :- (ISeq a) => a -> b)
(+T clojure.core/ffirst :- nil -> nil)
(+T clojure.core/ffirst :- (Seqable a) => a -> b)
(+T clojure.core/ffirst :- (Iterable a) => a -> b)
(+T clojure.core/ffirst :- (PrimArray a) => a -> b)
(+T clojure.core/ffirst :- (CharSequence a) => a -> b)
(+T clojure.core/ffirst :- (Map a) => a -> b)

(+T clojure.core/nfirst :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/nfirst :- (ISeq a) => a -> nil)
(+T clojure.core/nfirst :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/nfirst :- (Seqable a) => a -> nil)
(+T clojure.core/nfirst :- nil -> nil)
(+T clojure.core/nfirst :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/nfirst :- (Iterable a) => a -> nil)
(+T clojure.core/nfirst :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/nfirst :- (PrimArray a) => a -> nil)
(+T clojure.core/nfirst :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/nfirst :- (CharSequence a) => a -> nil)
(+T clojure.core/nfirst :- (Map a) (ISeq b) => a -> b)
(+T clojure.core/nfirst :- (Map a) => a -> nil)

(+T clojure.core/fnext :- (ISeq a) => a -> b)
(+T clojure.core/fnext :- nil -> nil)
(+T clojure.core/fnext :- (Seqable a) => a -> b)
(+T clojure.core/fnext :- (Iterable a) => a -> b)
(+T clojure.core/fnext :- (PrimArray a) => a -> b)
(+T clojure.core/fnext :- (CharSequence a) => a -> b)
(+T clojure.core/fnext :- (Map a) => a -> b)

(+T clojure.core/nnext :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/nnext :- (ISeq a) => a -> nil)
(+T clojure.core/nnext :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/nnext :- (Seqable a) => a -> nil)
(+T clojure.core/nnext :- nil -> nil)
(+T clojure.core/nnext :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/nnext :- (Iterable a) => a -> nil)
(+T clojure.core/nnext :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/nnext :- (PrimArray a) => a -> nil)
(+T clojure.core/nnext :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/nnext :- (CharSequence a) => a -> nil)
(+T clojure.core/nnext :- (Map a) (ISeq b) => a -> b)
(+T clojure.core/nnext :- (Map a) => a -> nil)

(+T clojure.core/seq :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/seq :- (ISeq a) => a -> nil)
(+T clojure.core/seq :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/seq :- (Seqable a) => a -> nil)
(+T clojure.core/seq :- nil -> nil)
(+T clojure.core/seq :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/seq :- (Iterable a) => a -> nil)
(+T clojure.core/seq :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/seq :- (PrimArray a) => a -> nil)
(+T clojure.core/seq :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/seq :- (CharSequence a) => a -> nil)
(+T clojure.core/seq :- (Map a) (ISeq b) => a -> b)
(+T clojure.core/seq :- (Map a) => a -> nil)

(+T clojure.core/instance? :- Class a -> Boolean)
(+T clojure.core/seq? :- (ISeq a) => a -> true)
(+T clojure.core/seq? :- (not ISeq a) => a -> false)
(+T clojure.core/char? :- Character -> true)
(+T clojure.core/char? :- (not Character) -> false)
(+T clojure.core/string? :- a -> Boolean)

(comment
  (+T test-wrong-def :- +integer)
  (+def test-typed-def
    1.1)
  )

(comment
(+T clojure.core/+ :- (+Fun [+number +number -> +number]))
(+T clojure.core/- :- (+Fun [+number +number -> +number]))

(+T test-typed-def :- +number)
(+def test-typed-def
  (+let [[a :- +number] (- 2 (+ 1 1))
         [[b :- +integer]] [2]]
    (+ a b)))
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
