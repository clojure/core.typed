(ns typed.base
  (:import (clojure.lang Symbol Namespace IPersistentMap Var Keyword Namespace ISeq Seqable
                         Atom IRef IObj IPersistentList IDeref IRef IMeta Named ChunkBuffer
                         IPersistentSet IPersistentVector Associative Sorted Delay IMapEntry
                         Reversible Agent IBlockingDeref IFn))
  (:import (java.util Collection))
  (:require [typed.core :refer [+T Any IParseType Nothing annotate-class]]))

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/import [& (U Symbol (IPersistentList Symbol)) * ->  nil])
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
;                          :filter 
;                          {:then [(refine-type (arg 0) NonEmptySeq)]
;                           :else 'tt}]))
(+T clojure.core/rest (All [x]
                        [(Seqable x) -> (Seqable x)]))
(+T clojure.core/next (All [x]
                        [(Seqable x) -> (Seqable x)]))
;                         :filter 
;                         {:then [(refine-type (arg 0) NonEmptySeq)]
;                          :else [(refine-type (arg 0) EmptySeq)]}]))
(+T clojure.core/every? (All [x y]
                          [[x -> y] (Seqable x) -> Boolean]))
;                           :filter
;                           {:then [(refine-type (arg 1) (Seqable (:then-filter (arg 0))))]
;                            :else 'tt}]))
(+T clojure.core/set? (predicate IPersistentSet))
(+T clojure.core/set-validator! (All [x r]
                                  [(IRef x) (U nil [x -> r]) -> nil]))
(+T clojure.core/cons (All [x] 
                        [x (Seqable x) -> (Seqable x)]))

(+T clojure.core/conj (All [x]
                        [(Seqable x) x & x * -> (Seqable x)]))


(+T clojure.core/list (All [x]
                        [& x * -> (IPersistentList x)]))

(+T clojure.core/second (All [x]
                          [(Seqable x) -> x]))
;                           :filter 
;                           {:then [(refine-type (arg 0) NonEmptySeq)]
;                            :else 'tt}]))
(+T clojure.core/ffirst (All [x]
                          [(Seqable (Seqable x)) -> x]))
(+T clojure.core/nfirst (All [x]
                          [(Seqable x) -> x]))
(+T clojure.core/fnext (All [x]
                          [(Seqable (Seqable x)) -> x]))
(+T clojure.core/nnext (All [x y]
                          [(Seqable x) -> (Seqable y)]))
(+T clojure.core/seq (All [x]
                       [(Seqable x) -> (U nil (Seqable x))]))
;                        :filter 
;                        {:then [(refine-type (arg 0) NonEmptySeq)]
;                         :else [(refine-type (arg 0) EmptySeq)]}]))
;TODO predicate on 1st argument
(+T clojure.core/instance? (All [y]
                             [Class y -> boolean]))
(+T clojure.core/seq? (predicate ISeq))
(+T clojure.core/char? (predicate Character))
(+T clojure.core/map? (predicate IPersistentMap))
(+T clojure.core/vector? (predicate IPersistentVector))
;; TODO repeated key-values
(+T clojure.core/assoc (All [k v]
                         [(Associative k v) k v & -> (Associative k v)]))

(+T clojure.core/meta (All [x]
                        [(IMeta x) -> x]))

(+T clojure.core/with-meta (All [(x <! IObj) (y <! (IPersistentMap Any Any))]
                             [x y -> (I x y)]))

(+T clojure.core/last (All [x]
                        [(Seqable x) -> x]))
(+T clojure.core/butlast (All [x]
                           [(Seqable x) -> (Seqable x)]))
(+T clojure.core/cast (All [x]
                        [Class x -> x]))
(+T clojure.core/to-array [Collection -> Any]) ;TODO array types
(+T clojure.core/vector (All [x]
                          [& x * -> (IPersistentVector x)]))
(+T clojure.core/vec (All [x]
                       [(Seqable x) -> (IPersistentVector x)]))
;TODO type key-value pairs
(+T clojure.core/hash-map (All [x]
                            [& x * -> (IPersistentMap x x)]))
(+T clojure.core/hash-set (All [x]
                            [& x * -> (IPersistentSet x)]))
;TODO key-value pairs
(+T clojure.core/sorted-map (All [x]
                              [& x * -> (I Sorted
                                           (IPersistentMap x x))]))

;; TODO key-value rest args
(+T clojure.core/sorted-map-by (All [x]
                                 [[x -> boolean] & x * -> (I Sorted
                                                             (IPersistentMap x x))]))
(+T clojure.core/sorted-set (All [x]
                              [& x * -> (I Sorted (IPersistentSet x))]))

(+T clojure.core/sorted-set-by (All [x]
                                 [[x -> boolean] & x * -> (I Sorted
                                                             (IPersistentSet x))]))
(+T clojure.core/nil? (predicate nil))

(+T clojure.core/false? (predicate false))
(+T clojure.core/true? (predicate true))
(+T clojure.core/not [Any -> boolean])
(+T clojure.core/str [& Any * -> String])
(+T clojure.core/symbol? (predicate Symbol))
(+T clojure.core/keyword? (predicate Keyword))
(+T clojure.core/symbol (Fun [(U Symbol String) -> Symbol]
                             [String String -> Symbol]))
(+T clojure.core/gensym (Fun [-> Symbol]
                             [String -> Symbol]))
(+T clojure.core/keyword (Fun [(U Keyword String) -> Keyword]
                              [String String -> Keyword]))
(+T clojure.core/find-keyword (Fun [(U Keyword String) -> (U nil Keyword)]
                                   [String String -> (U nil Keyword)]))
;;TODO any number of prefix arguments + special last param
(+T clojure.core/list* [Any -> (IPersistentList Any)])

;;TODO any number of prefix arguments + special last param
(+T clojure.core/apply (All [r]
                         [[& Any * -> r] & Any * -> r]))

;;TODO tricky, return arg
(+T clojure.core/vary-meta (All [(x <! (IMeta Any))]
                             [x [(IPersistentMap Any Any) & Any * -> Any] & Any * -> x]))

;(+T clojure.core/chunk-buffer (All [x]
;                                (ChunkBuffer x)))

;; other chunk fns ..

(+T clojure.core/concat (All [x]
                          [& (Seqable x) * -> (ISeq x)]))

(+T clojure.core/delay? (predicate clojure.lang.Delay))

;; TODO something interesting here, do disjoint union get in way?
(+T clojure.core/force (All [x y]
                         [(U (Delay x) y) -> (U x y)]))

(+T clojure.core/identical? (All [x y]
                              [x y -> boolean]))
;                               :filter
;                               {:then [(= (class (arg 0)) 
;                                          (class (arg 1)))]
;                                :else 'tt}]))
(+T clojure.core/= (All [x y]
                     [x & y * -> boolean]))
(+T clojure.core/not= (All [x y]
                        [x & y * -> boolean]))

(+T clojure.core/compare (All [(x <! Comparable) y]
                           [x y -> Number]))

(+T clojure.core/zero? [Number -> boolean])

(+T clojure.core/count [(Seqable Any) -> Number])
(+T clojure.core/int [(U Character Number) -> int])

; interdependent bounds
;; TODO works for regexs, how does that relate to Seqable?
(+T clojure.core/nth (All [x y]
                       (Fun [(Seqable x) Number -> x]
                            [(Seqable x) Number y -> (U x y)])))

(+T clojure.core/< [& Number * -> boolean])
(+T clojure.core/inc [Number -> Number])

;; TODO interdependent bound
(+T clojure.core/reverse (All [x]
                           [(Seqable x) -> (Seqable x)]))

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

; TODO dotted pretype
(+T clojure.core/complement [[& Any * -> Any] -> [& Any * -> boolean]])
(+T clojure.core/constantly (All [x]
                              [x -> [& Any * -> x]]))
(+T clojure.core/identity (All [x]
                            [x -> x]))

(+T clojure.core/peek (All [x]
                        [(Seqable x) -> x]))

;interdependent bounds
(+T clojure.core/pop (All [x]
                       [(Seqable x) -> (Seqable x)]))

(+T clojure.core/contains? [(Seqable Any) Any -> boolean])

;; interdependent bounds
;; has interesting filters
(+T clojure.core/get (All [k v n]
                       (Fun
                         [(Associative k v) Any -> (U v nil)]
                         [(Associative k v) Any n -> (U v n)])))

; interdependent bounds
;doesn't track keys
(+T clojure.core/dissoc (All [k v]
                          [(IPersistentMap k v) & Any * -> (IPersistentMap k v)]))

; TODO dotted pretype
(+T clojure.core/disj (All [x]
                        [(IPersistentSet x) & Any * -> (IPersistentSet x)]))

(+T clojure.core/select-keys (All [k v]
                               [(IPersistentMap k v) (Seqable Any) -> (IPersistentMap k v)]))

(+T clojure.core/keys (All [k]
                        [(IPersistentMap k Any) -> (Seqable k)]))
(+T clojure.core/vals (All [v]
                        [(IPersistentMap Any v) -> (Seqable v)]))

(+T clojure.core/key (All [k]
                       [(IMapEntry k Any) -> k]))
(+T clojure.core/val (All [v]
                       [(IMapEntry Any v) -> v]))

;(def-type-alias ReversibleSeq [a] (I (Seqable a) Reversible))

(+T clojure.core/rseq (All [x]
                        [(I (Seqable x) Reversible) -> (U nil (Seqable x))]))
(+T clojure.core/name [(U String Named) -> String])
(+T clojure.core/namespace [Named -> (U nil String)])

;;TODO Multifn stuff ..

(+T clojure.core/agent (All [x]
                         [x & Any * -> (Agent x)]))

;(+T clojure.core/agent (All [x (y <! IPersistentMap)]
;                         [x & {:meta y
;                               :validator [x -> Any]
;                               :error-handler [Agent Throwable -> Any]
;                               :error-mode (U :continue :failt)}
;                          -> (I (Agent x)
;                                (IReference y))]))

;TODO arities take differing type arguments
(+T clojure.core/deref (All [x y]
                            (Fun 
                              [(IDeref x) -> x]
                              [(IBlockingDeref x) Long y -> (U x y)])))

(+T clojure.core/atom (All [x]
                        [x & Any * -> (Atom x)]))

;(+T clojure.core/atom (All [x r m]
;                        [x & {:validator [x -> r]
;                              :meta (IPersistentMap m)} *
;                         -> (I (IMeta (IPersistentMap m))
;                               (Atom x))]))

; interdependent bounds
(+T clojure.core/swap! (All [x]
                         [(Atom x) [x & Any * -> x] & Any * -> x]))

; interdependent bounds
(+T clojure.core/compare-and-set! (All [x]
                                    [(Atom x) x x -> boolean]))

; interdependent bounds
(+T clojure.core/reset! (All [x]
                          [(Atom x) x -> x]))

;;TODO validators

;TODO tricky!
(+T clojure.core/comp [& IFn * -> [& Any * -> Any]])

;TODO tricky!
(+T clojure.core/juxt [& IFn * -> [& Any * -> Any]])

;TODO tricky!
(+T clojure.core/partial [[Any & Any * -> Any] -> [& Any * -> Any]])

; If truthy result, we know Seqable argument is NonEmptySeqable
; If falsy, either argument is EmptySeqable, or argument is NonEmptySeqable
;   and r cannot
(+T clojure.core/some (All [y r]
                        [[y -> r] (Seqable y) -> (U nil r)]))
;                         :filters
;                         {:then [(refine-type (arg 2) NonEmptySeqable)]
;                          :else tt}]))

(+T clojure.core/not-any? [[& Any * -> Any] (Seqable Any) -> boolean])

; dotted pretype
(+T clojure.core/map (All [a d]
                       [[a & Any * -> d] (Seqable a) & (Seqable Any) * -> (Seqable d)]))

; dotted pretype
(+T clojure.core/mapcat (All [a d]
                          [[a & Any * -> (Seqable d)] (Seqable a) & (Seqable Any) * -> (Seqable d)]))

(+T clojure.core/filter (All [x]
                          [[x -> Any] (Seqable x) -> (Seqable x)]))

(+T clojure.core/take (All [x]
                        [(Seqable x) Number -> (Seqable x)]))

(+T clojure.core/take-while (All [x]
                              [[x -> Any] (Seqable x) -> (Seqable x)]))

(+T clojure.core/drop (All [x]
                        [(Seqable x) Number -> (Seqable x)]))

(+T clojure.core/drop-last (All [x]
                             (Fun [(Seqable x) -> (Seqable x)]
                                  [(Seqable x) Number -> (Seqable x)])))

(+T clojure.core/take-last (All [x]
                             (Fun [(Seqable x) -> (Seqable x)]
                                  [(Seqable x) Number -> (Seqable x)])))

(+T clojure.core/drop-while (All [x]
                              [[x -> Any] (Seqable x) -> (Seqable x)]))

(+T clojure.core/cycle (All [x]
                         [(Seqable x) -> (Seqable x)]))

(+T clojure.core/split-at (All [x]
                            [Number (Seqable x) -> (IPersistentVector (Seqable x))]))

(+T clojure.core/split-with (All [x]
                              [[x -> Any] (Seqable x) -> (IPersistentVector (Seqable x))]))

(+T clojure.core/repeat (All [x]
                          (Fun [x -> (Seqable x)]
                               [Number x -> (Seqable x)])))

(+T clojure.core/iterate (All [x]
                           [[x -> x] x -> (Seqable x)]))

(+T clojure.core/range (Fun [-> (ISeq Number)]
                            [Number -> (ISeq Number)]
                            [Number Number -> (ISeq Number)]
                            [Number Number Number -> (ISeq Number)]))

;merging different kinds of Maps?
(+T clojure.core/merge (All [(x <! IPersistentMap)]
                         [& x * -> x]))

;interdependent bounds
(+T clojure.core/merge-with (All [k v]
                              [[v v -> v] & (IPersistentMap k v) * -> (IPersistentMap k v)]))

(+T clojure.core/zipmap (All [k v]
                          [(Seqable k) (Seqable v) -> (IPersistentMap k v)]))

(+T clojure.core/partition (All [x]
                             [Number (Seqable x) -> (Seqable (IPersistentList x))]))

(+T clojure.core/eval (All [x y]
                        [x -> y]))

;(+T clojure.core/reduce (All [x]
;                          (Fun 
;                            [[-> x] (EmptySeqable x) -> x]
;                            [[x x -> x] (NonEmptySeqable x) -> x]
;                            [[-> x] x (EmptySeqable x) -> x]
;                            [[x x -> x] x (NonEmptySeqable x) -> x])))
