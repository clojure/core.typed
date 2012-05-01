(ns typed.base
  (:import (clojure.lang Symbol Namespace IPersistentMap Var Keyword Namespace ISeq Seqable
                         Atom IRef IObj IPersistentList IDeref IRef IMeta Named ChunkBuffer))
  (:import (java.util Collection))
  (:require [typed.core :refer [+T Any IParseType Nothing annotate-class]]))

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/import [& (U Symbol (List Symbol)) * ->  nil])
(+T clojure.core/find-ns [Symbol -> (U nil Namespace)])
(+T clojure.core/resolve 
    (Fun [Symbol -> (U nil Var Class)]
         [(Map Symbol Any) Symbol -> (U nil Var Class)]))
(+T clojure.core/refer [Symbol & Any * -> nil])
(+T clojure.core/require [& Any * -> nil])
(+T clojure.core/ns-name [Namespace -> String])
(+T clojure.core/*ns* Namespace)
(+T clojure.core/+ [& Number * -> Number])
(+T clojure.core/prn [& Any * -> nil])
(+T clojure.core/first (All [x]
                         [(Seqable x) -> x]))
(+T clojure.core/rest (All [x]
                        [(Seqable x) -> (ISeq x)]))
(+T clojure.core/next (All [x]
                        [(Seqable x) -> (ISeq x)]))
(+T clojure.core/every? (All [x y]
                          [[x -> y] (Seqable x) -> Boolean]))
(+T clojure.core/set? [Any -> Boolean])
(+T clojure.core/set-validator! (All [x r]
                                  [(IRef x) (U nil [x -> r]) -> nil]))
(+T clojure.core/cons (All [x] 
                        [x (Seqable x) -> (ISeq x)]))

(+T clojure.core/conj (All [x (y <! (ISeq x)) z]
                        [y x & x * -> y]))


(+T clojure.core/list (All [x]
                        [& x * -> (IPersistentList x)]))

(+T clojure.core/second (All [x]
                          [(Seqable x) -> x]))
(+T clojure.core/ffirst (All [y (x <! (Seqable (Seqable y)))]
                          [x -> y]))
(+T clojure.core/nfirst (All [x]
                          [(Seqable x) -> x]))
(+T clojure.core/fnext (All [y (x <! (Seqable (Seqable y)))]
                          [x -> y]))
(+T clojure.core/nnext (All [y (x <! (Seqable y))]
                          [x -> (Seqable y)]))
(+T clojure.core/seq (All [x]
                       [(Seqable x) -> (ISeq x)]))
(+T clojure.core/instance? (All [(x <! Class) y]
                             [x y -> boolean]))
(+T clojure.core/seq? (All [x]
                        [x -> boolean]))
(+T clojure.core/char? (All [x]
                         [x -> boolean]))
(+T clojure.core/map? (All [x]
                        [x -> boolean]))
(+T clojure.core/vector? (All [x]
                           [x -> boolean]))
;; TODO repeated key-values
(+T clojure.core/assoc (All [k v (x <! (Associative k v))]
                         [x k v &  -> x]))

(+T clojure.core/meta (All [x (y <! (IMeta x))]
                        [y -> x]))

(+T clojure.core/with-meta (All [(x <! IObj) (y <! IPersistentMap)]
                             [x y -> (I x y)]))

(+T clojure.core/last (All [x]
                        [(Seqable x) -> x]))
(+T clojure.core/butlast (All [x]
                           [(Seqable x) -> (ISeq x)]))
(+T clojure.core/cast (All [x]
                        [Class x -> x]))
(+T clojure.core/to-array (All [x]
                            [(Collection x) -> (Array Object)]))
(+T clojure.core/vector (All [x]
                          [& x * -> (IPersistentVector x)]))
(+T clojure.core/vec (All [x]
                       [(Seqable x) -> (IPersistentVector x)]))
;TODO type key-value pairs
(+T clojure.core/hash-map (All [...2 k v]
                            [& ...2 k v -> (IPersistentMap (IMapEntry k v))]))
(+T clojure.core/hash-set (All [x]
                            [& x * -> (IPersistentSet x)]))
;TODO key-value pairs
(+T clojure.core/sorted-map (All [...2 k v]
                              [& ...2 k v -> (I Sorted
                                                (IPersistentMap k v))]))

(+T clojure.core/sorted-map-by (All [...2 k v]
                                 [[k -> boolean] & ...2 k v -> (I Sorted
                                                                  (IPersistentMap k v))]))
(+T clojure.core/sorted-set (All [x]
                              [& x * -> (I Sorted (IPersistentSet x))]))

(+T clojure.core/sorted-set-by (All [x]
                                 [[x -> boolean] & x * -> (I Sorted
                                                             (IPersistentSet x))]))
(+T clojure.core/nil? (All [x]
                        [x -> boolean]))

(+T clojure.core/false? (All [x]
                          [x -> boolean]))
(+T clojure.core/true? (All [x]
                         [x -> boolean]))
(+T clojure.core/not (All [x]
                       [x -> boolean]))
(+T clojure.core/str [& Any * -> String])
(+T clojure.core/symbol? [Any -> boolean])
(+T clojure.core/keyword? [Any -> boolean])
(+T clojure.core/symbol (Fun [(U Symbol String) -> Symbol]
                             [String String -> Symbol]))
(+T clojure.core/gensym (Fun [-> Symbol]
                             [String -> Symbol]))
(+T clojure.core/keyword (Fun [(U Keyword String) -> Keyword]
                              [String String -> Keyword]))
(+T clojure.core/find-keyword (Fun [(U Keyword String) -> (U nil Keyword)]
                                   [String String -> (U nil Keyword)]))
;;TODO any number of prefix arguments + special last param
(+T clojure.core/list* [])

;;TODO any number of prefix arguments + special last param
(+T clojure.core/apply (All [r ... k]
                         [[& k ... k -> r] ???]))

;;TODO tricky, return arg
(+T clojure.core/vary-meta (All [y (x <! (IMeta y)) z ... k]
                             [x [y & k ... k -> z] & k ... k -> x]))

(+T clojure.core/chunk-buffer (All [x]
                                (ChunkBuffer x)))

;; other chunk fns ..

(+T clojure.core/concat (All [... k]
                          [& (Seqable k) ... k -> (ISeq k)]))

(+T clojure.core/delay? (All [x]
                          [x -> boolean]))

;; TODO something interesting here, does disjoint union get in way?
(+T clojure.core/force (All [y (x <! (U (Delay y) Any))]
                         [x -> (U x y)]))

(+T clojure.core/identical? (All [x y]
                              [x y -> boolean]))
(+T clojure.core/= (All [x y]
                     [x & y * -> boolean]))
(+T clojure.core/not= (All [x y]
                        [x & y * -> boolean]))

(+T clojure.core/compare (All [(x <! Comparable) y]
                           [x y -> Number]))

(+T clojure.core/zero? [Number -> boolean])

(+T clojure.core/count (All [(x <! Seqable)]
                         [x -> Number]))
(+T clojure.core/int [(U Character Number) -> int])

;; TODO works for regexs, how does that relate to Seqable?
(+T clojure.core/nth (All [y (x <! (Seqable y)) z]
                       (Fun [x Number -> y]
                            [x Number z -> (U z y)])))

(+T clojure.core/< [& Number * -> boolean])
(+T clojure.core/inc [Long -> Long])

(+T clojure.core/reverse (All [y (x <! (Seqable y))]
                           [x -> x]))

(+T clojure.core/+' [& Number * -> Number])
(+T clojure.core/+ [& Number * -> Number])
(+T clojure.core/*' [& Number * -> Number])
(+T clojure.core/* [& Number * -> Number])
(+T clojure.core// [Number & Number * -> Number])
(+T clojure.core/-' [Number & Number * -> Number])
(+T clojure.core/- [Number & Number * -> Number])

(+T clojure.core/<= [Number & Number * -> boolean])
(+T clojure.core/> [Number & Number * -> boolean])
(+T clojure.core/>= [Number & Number * -> boolean])

(+T clojure.core/== [Object & Object * -> boolean])

(+T clojure.core/max [Number & Number * -> Number])
(+T clojure.core/min [Number & Number * -> Number])

(+T clojure.core/dec' [Number -> Number])
(+T clojure.core/dec [Number -> Number])

;; some unchecked arith ..

;; some other arith ..

(+T clojure.core/complement (All [x ... y]
                              [[& y ... y -> x] -> [& y ... y -> boolean]]))
(+T clojure.core/constantly (All [x y]
                              [x -> [& y * -> x]]))
(+T clojure.core/identity (All [x]
                            [x -> x]))

(+T clojure.core/peek (All [y (x <! (Seqable y))]
                        [x -> y]))

(+T clojure.core/pop (All [y (x <! (Seqable y))]
                       [x -> y]))

(+T clojure.core/contains? (All [y (x <! (Seqable y)) z]
                             [x z -> boolean]))

;; has interesting filters
(+T clojure.core/get (All [k v (x <! (Associative k v)) n]
                       (Fun
                         [x k -> (U v nil)]
                         [x k n -> (U v n)])))

;doesn't track keys
(+T clojure.core/dissoc (All [y (x <! IPersistentMap)]
                          [x & y * -> x]))

(+T clojure.core/disj (All [y (x <! IPersistentSet)]
                        [x & y * -> x]))

(+T clojure.core/select-keys (All [k v (x <! (IPersistentMap k v))]
                               [x (Seqable k) * -> x]))

(+T clojure.core/keys (All [k v
                            (x <! (IPersistentMap (IMapEntry k v)))]
                        [x -> (ISeq k)]))
(+T clojure.core/vals (All [k v
                            (x <! (IPersistentMap (IMapEntry k v)))]
                        [x -> (ISeq v)]))

(+T clojure.core/key (All [k v (x <! (IMapEntry k v))]
                       [x -> k]))
(+T clojure.core/val (All [k v (x <! (IMapEntry k v))]
                       [x -> v]))

(+T clojure.core/rseq (All [y (x <! (I (Seqable y) Reversible))]
                        [x -> (ISeq y)]))
(+T clojure.core/name [(U String Named) -> String])
(+T clojure.core/namespace [Named -> (U nil String)])

;;TODO Multifn stuff ..


(+T clojure.core/agent (All [x (y <! IPersistentMap)]
                         [x & {:meta y
                               :validator [x -> Any]
                               :error-handler [Agent Throwable -> Any]
                               :error-mode (U :continue :failt)}
                          -> (I (Agent x)
                                (IReference y))]))

(+T clojure.core/deref (Fun 
                         (All [y (x <! (IDeref y))]
                           (arity [x -> y]))
                         (All [y (x <! (IBlockingDeref y))]
                           (arity [x Long Any -> y]))))

(+T clojure.core/atom (All [x r m]
                        [x & {:validator [x -> r]
                              :meta (IPersistentMap m)} *
                         -> (I (IMeta (IPersistentMap m))
                               (Atom x))]))

(+T clojure.core/swap! (All [y (x <! (Atom y)) a ... z]
                         [x [y & z ... z -> y] & z ... z -> x]))

(+T clojure.core/compare-and-set! (All [y (x <! (Atom y))]
                                    [x y y -> boolean]))

(+T clojure.core/reset! (All [y (x <! (Atom y))]
                          [x y -> y]))

;;TODO validators

;TODO tricky!
(+T clojure.core/comp (All [... f]
                        [& f ... f -> []]))

;TODO tricky!
(+T clojure.core/juxt (All [... (f <! )]
                        [& f ...f -> [&]]))

(+T clojure.core/partial (All [x y r ... z]
                           [[x & z ... z -> r] -> [& z ... z -> r]]))

(+T clojure.core/some (All [y r]
                        [[y -> r] (Seqable y) -> (U nil r)]))

(+T clojure.core/not-any? (All [x r]
                            [[y -> r] (Seqable y) -> boolean]))

(+T clojure.core/map (All [a d ... c]
                       [[a & c ... c -> d] (Seqable a) (Seqable c) ... c -> (Seqable d)]))

(+T clojure.core/mapcat (All [a d ... c]
                          [[a & c ... c -> (Seqable d)] (Seqable a) (Seqable c) ... c -> (Seqable d)]))

(+T clojure.core/filter (All [x (y <! (Seqable x)) r]
                          [[x -> r] y -> y])) 

(+T clojure.core/take (All [x]
                        [(Seqable x) Number -> (ISeq x)]))

(+T clojure.core/take-while (All [x (y <! (Seqable x)) r]
                              [[x -> r] y -> y])) 

(+T clojure.core/drop (All [x]
                        [(Seqable x) Number -> (ISeq x)]))

(+T clojure.core/drop-last (All [x]
                             (Fun [(Seqable x) -> (ISeq x)]
                                  [(Seqable x) Number -> (ISeq x)])))

(+T clojure.core/take-last (All [x]
                             (Fun [(Seqable x) -> (ISeq x)]
                                  [(Seqable x) Number -> (ISeq x)])))

(+T clojure.core/drop-while (All [x (y <! (Seqable x)) r]
                              [[x -> r] y -> y])) 

(+T clojure.core/cycle (All [x]
                         [(Seqable x) -> (ISeq x)]))

(+T clojure.core/split-at (All [x]
                            [Number (Seqable x) -> (IPersistentVector (ISeq x))]))

(+T clojure.core/split-with (All [x r]
                              [[x -> r] (Seqable x) -> (IPersistentVector (ISeq x))]))

(+T clojure.core/repeat (All [x]
                          (Fun [x -> (ISeq x)]
                               [Number x -> (ISeq x)])))

(+T clojure.core/iterate (All [x]
                           [[x -> x] x -> (ISeq x)]))

(+T clojure.core/range (Fun [-> (ISeq Number)]
                            [Number -> (ISeq Number)]
                            [Number Number -> (ISeq Number)]
                            [Number Number Number -> (ISeq Number)]))

(+T clojure.core/merge (All [(x <! IPersistentMap)]
                         [& x * -> x]))

(+T clojure.core/merge-with (All [k v (y <! (IPersistentMap k v))]
                              [[v v -> v] & y * -> y]))

(+T clojure.core/zipmap (All [k v]
                          [(Seqable k) (Seqable v) -> (IPersistentMap k v)]))

(+T clojure.core/partition (All [x]
                             [Number (Seqable x) -> (ISeq (IPersistentList x))]))

(+T clojure.core/eval (All [x y]
                        [x -> y]))
