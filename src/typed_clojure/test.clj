(ns typed-clojure.test
  (:use [typed-clojure.core]
        [typed-clojure.types]
        [clojure.core.logic :only [lvar]]))

;(type-check-namespace 'typed-clojure.test)

;(T test-typed-def-fn :- (+Fun [+integer -> +integer]))
;(def test-typed-def-fn
;  (fn-T ([[a :- +integer]] 1)))
(comment

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
(+T clojure.core/string? :- String -> true)
(+T clojure.core/string? :- (not String) -> false)
(+T clojure.core/map? :- (IPersistentMap a) => a -> true)
(+T clojure.core/map? :- (not IPersistentMap a) => a -> false)
(+T clojure.core/vector? :- (IPersistentVector a) => a -> true)
(+T clojure.core/vector? :- (not IPersistentVector a) => a -> false)

(+T clojure.core/assoc :- (Associative e) => a b c d * -> e)

(+T clojure.core/meta :- a -> b)

(+T clojure.core/with-meta :- (IObj a) (IObj b) => a -> b)

(+T clojure.core/last :- (ISeq a) => a -> b)
(+T clojure.core/last :- nil -> nil)
(+T clojure.core/last :- (Seqable a) => a -> b)
(+T clojure.core/last :- (Iterable a) => a -> b)
(+T clojure.core/last :- (PrimArray a) => a -> b)
(+T clojure.core/last :- (CharSequence a) => a -> b)
(+T clojure.core/last :- (Map a) => a -> b)

(+T clojure.core/butlast :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/butlast :- (ISeq a) => a -> nil)
(+T clojure.core/butlast :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/butlast :- (Seqable a) => a -> nil)
(+T clojure.core/butlast :- nil -> nil)
(+T clojure.core/butlast :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/butlast :- (Iterable a) => a -> nil)
(+T clojure.core/butlast :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/butlast :- (PrimArray a) => a -> nil)
(+T clojure.core/butlast :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/butlast :- (CharSequence a) => a -> nil)
(+T clojure.core/butlast :- (Map a) (ISeq b) => a -> b)
(+T clojure.core/butlast :- (Map a) => a -> nil)

(+T clojure.core/cast :- Class a -> b)

(+T clojure.core/to-array :- nil -> PrimObjectArray)
(+T clojure.core/to-array :- PrimIntArray -> PrimObjectArray) ;; TODO Other arrays
(+T clojure.core/to-array :- PrimStringArray -> PrimObjectArray)
(+T clojure.core/to-array :- String -> PrimObjectArray)
(+T clojure.core/to-array :- (Collection a) => a -> PrimObjectArray)
(+T clojure.core/to-array :- (Map a) => a -> PrimObjectArray)

(+T clojure.core/vector :- (IPersistentVector b) => a * -> b)

(+T clojure.core/vec :- (Collection a) (IPersistentVector b) => a -> b)
(+T clojure.core/vec :- (IPersistentVector b) => PrimObjectArray -> b)
(+T clojure.core/vec :- (ISeq a) (IPersistentVector b) => a -> b)
(+T clojure.core/vec :- (Seqable a) (IPersistentVector b) => a -> b)
(+T clojure.core/vec :- (Iterable a) (IPersistentVector b) => a -> b)
(+T clojure.core/vec :- (PrimArray a) (IPersistentVector b) => a -> b)
(+T clojure.core/vec :- (CharSequence a) (IPersistentVector b) => a -> b)
(+T clojure.core/vec :- (Map a) (IPersistentVector b) => a -> b)

(+T clojure.core/hash-map :- (IPersistentMap b) => a * -> b)

(+T clojure.core/hash-set :- (IPersistentSet b) => a * -> b)

(+T clojure.core/sorted-map :- (IPersistentMap b) (Sorted b) => a * -> b)

(+T clojure.core/sorted-map-by :- (Comparator a) (IPersistentMap c) (Sorted c) => a b * -> c)

(+T clojure.core/sorted-set-by :- (Comparator a) (IPersistentSet c) (Sorted c) => a b * -> c)

(+T clojure.core/nil? :- nil -> true)
(+T clojure.core/nil? :- (not nil) -> false)

;; TODO defmacro
;; TODO when
;; TODO when-not

(+T clojure.core/false? :- false -> true)
(+T clojure.core/false? :- (not false) -> false)

(+T clojure.core/true? :- true -> true)
(+T clojure.core/true? :- (not true) -> false)

(+T clojure.core/not :- (not nil false) -> false)
(+T clojure.core/not :- nil -> true)
(+T clojure.core/not :- false -> true)

(+T clojure.core/str :- a * -> String)

(+T clojure.core/symbol? :- Symbol -> true)
(+T clojure.core/symbol? :- (not Symbol) -> false)

(+T clojure.core/keyword? :- Keyword -> true)
(+T clojure.core/keyword? :- (not Keyword) -> false)

(+T clojure.core/symbol :- String -> Symbol)
(+T clojure.core/symbol :- String String -> Symbol)

;; TODO cond

(+T clojure.core/gensym :- -> Symbol)
(+T clojure.core/gensym :- a -> Symbol)

(+T clojure.core/keyword :- Keyword -> Keyword)
(+T clojure.core/keyword :- Symbol -> Keyword)
(+T clojure.core/keyword :- String -> Keyword)
(+T clojure.core/keyword :- String String -> Keyword)

(+T clojure.core/find-keyword :- Keyword -> Keyword)
(+T clojure.core/find-keyword :- Symbol -> Keyword)
(+T clojure.core/find-keyword :- String -> Keyword)
(+T clojure.core/find-keyword :- String String -> Keyword)

(+T clojure.core/list* (ISeq c) => a * b -> c)

(+T clojure.core/apply :- (IFn a) => a b * c -> d)

(+T clojure.core/vary-meta :- (IObj a) (IFn b) (IObj d) => a b c * -> d)

(+T clojure.core/lazy-seq :- a * -> LazySeq) ;; TODO a* returns an ISeq or nil

(+T clojure.core/chunk-buffer :- Integer/TYPE -> ChunkBuffer) ;; TODO a* returns an ISeq or nil
                                                     ;; TODO int or Long or ...

(+T clojure.core/chunk-append :- ChunkBuffer a -> nil)

(+T clojure.core/chunk :- (IChunk a) => ChunkBuffer -> a)

(+T clojure.core/chunk-first :- (IChunkedSeq a) (IChunk b) => a -> b)

(+T clojure.core/chunk-rest :- (IChunkedSeq a) (ISeq b) => a -> b)

(+T clojure.core/chunk-next :- (IChunkedSeq a) (ISeq b) => a -> b)

(+T clojure.core/chunk-cons :- (IChunk a) => a b -> c)

(+T clojure.core/concat :- a * -> LazySeq)

(+T clojure.core/delay :- a * -> Delay)

(+T clojure.core/delay? :- Delay -> true)
(+T clojure.core/delay? :- (not Delay) -> false)

(+T clojure.core/force :- a -> b)

;; TODO if-not

(+T clojure.core/identical? :- a b -> Boolean)

(+T clojure.core/= :- a -> true)
(+T clojure.core/= :- a * -> Boolean)

(+T clojure.core/not= :- a -> false)
(+T clojure.core/not= :- a * -> Boolean)

(+T clojure.core/compare :- (Comparable a) => a b -> Integer)

(+T clojure.core/and :- -> true)
(+T clojure.core/and :- a -> a)
(+T clojure.core/and :- a * -> b) ;; TODO hmm, what does a * represent?

(+T clojure.core/or :- -> nil)
(+T clojure.core/or :- a -> a)
(+T clojure.core/or :- a * -> b)

(+T clojure.core/zero? :- 0 -> true)
(+T clojure.core/zero? :- (not 0) -> false)

(+T clojure.core/count :- (Counted a) => a -> Integer)
(+T clojure.core/count :- nil -> 0)
(+T clojure.core/count :- (IPersistentCollection a) => a -> Integer)
(+T clojure.core/count :- (CharSequence a) => a -> Integer)
(+T clojure.core/count :- (Collection a) => a -> Integer)
(+T clojure.core/count :- (Map a) => a -> Integer)
(+T clojure.core/count :- PrimArray -> Integer)

(+T clojure.core/int :- Integer -> int)
(+T clojure.core/int :- Long -> int)
(+T clojure.core/int :- BigInt -> int)
(+T clojure.core/int :- BigInteger -> int)
(+T clojure.core/int :- Byte -> int)
(+T clojure.core/int :- Short -> int)
(+T clojure.core/int :- Ratio -> int)
(+T clojure.core/int :- Character -> int)
(+T clojure.core/int :- char -> int)
(+T clojure.core/int :- byte -> int)
(+T clojure.core/int :- int -> int)
(+T clojure.core/int :- float -> int)
(+T clojure.core/int :- long -> int)
(+T clojure.core/int :- double -> int)

(+T clojure.core/nth :- (Indexed a) => a Integer -> c)
(+T clojure.core/nth :- nil Integer -> c)
(+T clojure.core/nth :- (CharSequence a) => a Integer -> Character)
(+T clojure.core/nth :- PrimArray -> a)
(+T clojure.core/nth :- (RandomAccess a) => a Integer -> b)
(+T clojure.core/nth :- Matcher Integer -> a)
(+T clojure.core/nth :- Map.Entry Integer -> a)
(+T clojure.core/nth :- (Sequential a) => a Integer -> a)

(+T clojure.core/nth :- nil Integer b -> b)
(+T clojure.core/nth :- (CharSequence a) => a Integer b -> c)
(+T clojure.core/nth :- PrimArray Integer a -> b)
(+T clojure.core/nth :- (RandomAccess a) => a Integer b -> c)
(+T clojure.core/nth :- (Matcher a) => a Integer b -> c)
(+T clojure.core/nth :- Map.Entry Integer b -> c)
(+T clojure.core/nth :- (Sequential a) => a Integer b -> c)

(+T clojure.core/< :- Number -> true)
(+T clojure.core/< :- Number Number Number * -> Boolean)

(+T clojure.core/inc' :- Number -> Number)

(+T clojure.core/inc :- Number -> Number)

(+T clojure.core/reverse :- (ISeq a) (ISeq b) => a -> b)
(+T clojure.core/reverse :- (ISeq a) => nil -> a)
(+T clojure.core/reverse :- (Seqable a) (ISeq b) => a -> b)
(+T clojure.core/reverse :- (Iterable a) (ISeq b) => a -> b)
(+T clojure.core/reverse :- (PrimArray a) (ISeq b) => a -> b)
(+T clojure.core/reverse :- (CharSequence a) (ISeq b) => a -> b)
(+T clojure.core/reverse :- (Map a) (ISeq b) => a -> b)

(+T clojure.core/+' :- -> 0)
(+T clojure.core/+' :- a -> Number)
(+T clojure.core/+' :- a b c * -> Number)

(+T clojure.core/+ :- -> 0)
(+T clojure.core/+ :- a -> Number)
(+T clojure.core/+ :- a b c * -> Number)

(+T clojure.core/*' :- -> 1)
(+T clojure.core/*' :- a -> Number)
(+T clojure.core/*' :- a b c * -> Number)

(+T clojure.core/* :- -> 1)
(+T clojure.core/* :- a -> Number)
(+T clojure.core/* :- a b c * -> Number)

(+T clojure.core// :- a -> Number)
(+T clojure.core// :- a b c * -> Number)

(+T clojure.core/-' :- a -> Number)
(+T clojure.core/-' :- a b c * -> Number)

(+T clojure.core/- :- a -> Number)
(+T clojure.core/- :- a b c * -> Number)

;;TODO non-nil return?
(+T clojure.core/<= :- a -> true)
(+T clojure.core/<= :- Number Number Number * -> Boolean)

(+T clojure.core/>= :- a -> true)
(+T clojure.core/>= :- Number Number Number * -> Boolean)

(+T clojure.core/== :- a -> true)
(+T clojure.core/== :- Number Number Number * -> Boolean)

(+T clojure.core/max :- a -> a)
(+T clojure.core/max :- Number Number Number * -> Number)

(+T clojure.core/min :- a -> a)
(+T clojure.core/min :- Number Number Number * -> Number)

(+T clojure.core/dec :- Number -> Number)

(+T clojure.core/unchecked-inc-int :- int -> int)

(+T clojure.core/unchecked-inc :- long -> long)
(+T clojure.core/unchecked-inc :- a -> Number)
(+T clojure.core/unchecked-inc :- double -> double)

(+T clojure.core/unchecked-dec-int :- int -> int)

(+T clojure.core/unchecked-dec :- long -> long)
(+T clojure.core/unchecked-dec :- a -> Number)
(+T clojure.core/unchecked-dec :- double -> double)

(+T clojure.core/unchecked-negate-int :- int -> int)

(+T clojure.core/unchecked-negate :- long -> long)
(+T clojure.core/unchecked-negate :- a -> Number)
(+T clojure.core/unchecked-negate :- double -> double)

(+T clojure.core/unchecked-add-int :- int int -> int)

(+T clojure.core/unchecked-add :- long -> long)
(+T clojure.core/unchecked-add :- a -> Number)
(+T clojure.core/unchecked-add :- double -> double)

(+T clojure.core/unchecked-subtract-int :- int int -> int)

(+T clojure.core/unchecked-subtract :- long -> long)
(+T clojure.core/unchecked-subtract :- a -> Number)
(+T clojure.core/unchecked-subtract :- double -> double)

(+T clojure.core/unchecked-multiply-int :- int int -> int)

(+T clojure.core/unchecked-multiply :- long -> long)
(+T clojure.core/unchecked-multiply :- a -> Number)
(+T clojure.core/unchecked-multiply :- double -> double)

(+T clojure.core/unchecked-divide-int :- int int -> int)

(+T clojure.core/unchecked-remainder-int :- int int -> int)

(+T clojure.core/pos? :- Number -> Boolean)

(+T clojure.core/neg? :- Number -> Boolean)

(+T clojure.core/quot :- Number Number -> Number)

(+T clojure.core/rem :- Number Number -> Number)

(+T clojure.core/rationalize :- Number -> Number)

(+T clojure.core/bit-not :- long -> long)
(+T clojure.core/bit-not :- int -> long)
(+T clojure.core/bit-not :- short -> long)
(+T clojure.core/bit-not :- byte -> long)

(+T clojure.core/bit-and :- long long -> long)
(+T clojure.core/bit-and :- int long -> long)
(+T clojure.core/bit-and :- long int -> long)
(+T clojure.core/bit-and :- int int -> long)
(+T clojure.core/bit-and :- int short -> long)
(+T clojure.core/bit-and :- short int -> long) ;; TODO etc...

;; TODO bit-or
;; TODO bit-xor
;; TODO bit-and-not
;; TODO bit-clear
;; TODO bit-set
;; TODO bit-flip
;; TODO bit-test
;; TODO bit-shift-left
;; TODO bit-shift-right

(+T clojure.core/integer? :- Integer -> true)
(+T clojure.core/integer? :- Long -> true)
(+T clojure.core/integer? :- BigInt -> true)
(+T clojure.core/integer? :- BigInteger -> true)
(+T clojure.core/integer? :- Short -> true)
(+T clojure.core/integer? :- Byte -> true)
(+T clojure.core/integer? :- (not Integer Long BigInt BigInteger Short Byte) -> false)

(+T clojure.core/even? :- Integer -> Boolean)
(+T clojure.core/even? :- Long -> Boolean)
(+T clojure.core/even? :- BigInt -> Boolean)
(+T clojure.core/even? :- BigInteger -> Boolean)
(+T clojure.core/even? :- Short -> Boolean)
(+T clojure.core/even? :- Byte -> Boolean)

(+T clojure.core/odd? :- Integer -> Boolean)
(+T clojure.core/odd? :- Long -> Boolean)
(+T clojure.core/odd? :- BigInt -> Boolean)
(+T clojure.core/odd? :- BigInteger -> Boolean)
(+T clojure.core/odd? :- Short -> Boolean)
(+T clojure.core/odd? :- Byte -> Boolean)

(+T clojure.core/complement :- (IFn a) (IFn b) => a -> b)

(+T clojure.core/constantly :- (IFn a) (IFn b) => a -> b)

(+T clojure.core/identity :- a -> a)

(+T clojure.core/peek :- nil -> nil)
(+T clojure.core/peek :- (IPersistentStack a) => a -> b)

(+T clojure.core/pop :- nil -> nil)
(+T clojure.core/pop :- (IPersistentStack a) (IPersistentStack b) => a -> b)

(+T clojure.core/contains? :- nil a -> false)
(+T clojure.core/contains? :- (Associative a) => a b -> Boolean)
(+T clojure.core/contains? :- (IPersistentSet a) => a b -> Boolean)
(+T clojure.core/contains? :- (Map a) => a b -> Boolean)
(+T clojure.core/contains? :- (Set a) => a b -> Boolean)
(+T clojure.core/contains? :- String Number -> Boolean)

(+T clojure.core/get :- (ILookup a) => a b -> c)
(+T clojure.core/get :- nil a -> b)
(+T clojure.core/get :- (Map a) => a b -> c)
(+T clojure.core/get :- (IPersistentSet a) => a b -> c)
(+T clojure.core/get :- String Number -> c)
(+T clojure.core/get :- PrimArray Number -> c)
(+T clojure.core/get :- (not ILookup Map IPersistentSet a) => (not nil String PrimArray a) b -> nil) ;; TODO Syntax

(+T clojure.core/get :- (ILookup a) => a b c -> d)
(+T clojure.core/get :- nil b c -> c)
(+T clojure.core/get :- (Map a) => nil b c -> d) ;; TODO Return type (Union c e) ?
(+T clojure.core/get :- (IPersistentSet a) => a b c -> c)
(+T clojure.core/get :- String Number a -> b)
(+T clojure.core/get :- PrimArray Number a -> b)
(+T clojure.core/get :- (not ILookup Map IPersistentSet a) => (not nil String PrimArray a) b c -> c) ;; TODO Syntax

(+T clojure.core/dissoc :- a -> a)
(+T clojure.core/dissoc :- nil a b * -> nil)
(+T clojure.core/dissoc :- (IPersistentMap a) => a b c * -> a)

(+T clojure.core/disj :- a -> a)
(+T clojure.core/disj :- (IPersistentSet a) => a b c * -> a)

(+T clojure.core/find :- nil a -> nil)
(+T clojure.core/find :- (Associative a) => a b -> nil)
(+T clojure.core/find :- (Associative a) (IMapEntry c) => a b -> c)
(+T clojure.core/find :- (Map a) => a b -> nil)
(+T clojure.core/find :- (Map a) (IMapEntry c) => a b -> c) ;;TODO see java code, cast to Map

(+T clojure.core/select-keys :- (ISeq a) (IPersistentMap b) => nil a -> b)
(+T clojure.core/select-keys :- (Associative a) (ISeq b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Associative a) (Seqable b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Associative a) (Iterable b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Associative a) (IPersistentMap b) => a PrimArray -> b)
(+T clojure.core/select-keys :- (Associative a) (CharSequence b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Associative a) (Map b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Map a) (ISeq b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Map a) (Seqable b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Map a) (Iterable b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Map a) (IPersistentMap b) => a PrimArray -> b)
(+T clojure.core/select-keys :- (Map a) (CharSequence b) (IPersistentMap c) => a b -> c)
(+T clojure.core/select-keys :- (Map a) (Map b) (IPersistentMap c) => a b -> c)

(+T clojure.core/keys :- (ISeq a) => a nil)
(+T clojure.core/keys :- (ISeq a) (ISeq b) => a b) ;(let [a (keys [:a])] nil) ;nil

(+T clojure.core/vals :- (ISeq a) => a nil)
(+T clojure.core/vals :- (ISeq a) (ISeq b) => a b) ;(let [a (vals [:a])] nil) ;nil

(+T clojure.core/key :- Map.Entry => a)

(+T clojure.core/val :- Map.Entry => a)

(+T clojure.core/rseq :- (Reversible a) => a -> nil)
(+T clojure.core/rseq :- (Reversible a) (ISeq b) => a -> b)

(+T clojure.core/name :- String -> String)
(+T clojure.core/name :- (Named a) -> String)

(+T clojure.core/namespace :- (Named a) -> nil)
(+T clojure.core/namespace :- (Named a) -> String)

;TODO locking
;TODO ..
;TODO ->
;TODO ->>
;TODO defmulti
;TODO defmethod

(+T clojure.core/remove-all-methods :- MultiFn -> MultiFn)

(+T clojure.core/remove-method :- MultiFn a -> MultiFn)

(+T clojure.core/prefer-method :- MultiFn a b -> MultiFn)

(+T clojure.core/methods :- (IPersistentMap a) => MultiFn -> a)

(+T clojure.core/get-method :- (IFn b) => MultiFn a -> b) ;; TODO can b be nil?

(+T clojure.core/prefers :- (IPersistentMap a) => MultiFn -> a)

;TODO if-let
;TODO when-let
;TODO push-thread-bindings

(+T clojure.core/pop-thread-bindings :- -> nil)

(+T clojure.core/get-thread-bindings :- (Associative a) => -> a)

;TODO binding

(+T clojure.core/with-bindings* :- (Associative a) (IFn b) => a b c * -> nil)

;TODO with-bindings

(+T clojure.core/bound-fn* :- (IFn a) (IFn b) => a -> b)

;TODO bound-fn

(+T clojure.core/find-var :- Symbol -> nil)
(+T clojure.core/find-var :- Symbol -> Var)

(+T clojure.core/agent :- a b * -> Agent)

(+T clojure.core/send :- (IFn b) => Agent a b c * -> Agent)

(+T clojure.core/release-pending-sends :- -> int)

(+T clojure.core/add-watch :- (IRef a) (IFn c) (IRef d) => a b c -> d)

(+T clojure.core/remove-watch :- (IRef a) (IRef c) => a b -> c)

(+T clojure.core/agent-error :- (Throwable b) => Agent a -> b)
(+T clojure.core/agent-error :- Agent a -> nil)

(+T clojure.core/restart-agent :- Agent a b * -> c)

(+T clojure.core/set-error-handler! :- (IFn a) => Agent a -> nil)

(+T clojure.core/error-handler :- Agent -> nil)
(+T clojure.core/error-handler :- (IFn a) => Agent -> a)

(+T clojure.core/set-error-mode! :- Agent Keyword -> nil)

(+T clojure.core/error-mode :- Agent -> Keyword)

;clojure.core/agent-errors deprecated
;clojure.core/clear-agent-errors deprecated
;clojure.core/shutdown-agents deprecated

(+T clojure.core/ref :- a b * -> Ref)

(+T clojure.core/deref :- (IDeref a) => a -> b)
(+T clojure.core/deref :- (IBlockingDeref a) => a long b -> c)

(+T clojure.core/atom :- a b * -> Atom)

(+T clojure.core/swap! :- (IFn a) => Atom a b * -> c)

(+T clojure.core/compare-and-set! :- Atom a b -> Boolean)

(+T clojure.core/reset! :- Atom a -> a)

(+T clojure.core/set-validator! :- (IRef a) (IFn b) => a b -> nil)

(+T clojure.core/get-validator :- (IRef a) (IFn b) => a -> b)

(+T clojure.core/alter-meta! :- (IReference a) (IFn b) (ISeq d) => a b c * -> d)

(+T clojure.core/reset-meta! :- (IReference a) (IPersistentMap b) (IPersistentMap c) => a b -> c)

(+T clojure.core/commute :-  (IFn b) => Ref b c * -> d)
  )

(comment
  (+T test-wrong-def :- +integer)
  (+def test-typed-def
    1.1)
  )

(+T clojure.core/symbol? :- {:kind :fn
                          :arities [{:kind :arity
                                     :max-fixed-arity 1
                                     :variadic false
                                     :refinements 
                                     [{:kind :refinement
                                       :dom [{:kind :type
                                              :type clojure.lang.Symbol}]
                                       :rng {:kind :value
                                             :value true}}

                                     {:kind :refinement
                                      :dom [{:kind :type
                                             :not-type clojure.lang.Symbol}]
                                      :rng {:kind :value
                                            :value false}}]}]})


(+T clojure.core/instance? :- {:kind :fn
                                :arities [{:kind :arity
                                           :max-fixed-arity 2
                                           :variadic false
                                           :refinements 
                                           [(map->Refinement {
                                             :dom [(map->Type {
                                                    :type clojure.lang.Symbol})]
                                             :rng (map->Value {
                                                    :value true})})

                                            (map->Refinement {
                                             :dom [(map->NotType {
                                                    :not-type clojure.lang.Symbol})]
                                             :rng (map->Value {
                                                   :value false})})]}]})

(+T test-typed-def :- (Number a) => a)
(def test-typed-def
  (let [a (- 2 (+ 1 1))
        b 2]
    (+ a b)))

(deftype: None)
(deftype: [a] Some [[x : a]])

(def-type-alias (Opt a) (U None (Some a)))

(+T seq-opt (
(defn seq-opt [

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
