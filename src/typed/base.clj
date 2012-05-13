(ns typed.base
  (:import (java.util Collection)
           (clojure.lang APersistentSet))
  (:require [typed.core :refer [IParseType +T]]
            [typed.types :refer [Any Nothing Symbol Namespace NilList Var
                                 NilMap AnyNumber NilSeqable Seq NilSeq Set NilColl
                                 Coll List]]))

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/import [& (U Symbol (NilList Symbol)) * -> nil])
(+T clojure.core/find-ns [Symbol -> (U nil Namespace)])
(+T clojure.core/resolve 
    (Fun [Symbol -> (U nil Var Class)]
         [(NilMap Symbol Any) Symbol -> (U nil Var Class)]))
(+T clojure.core/refer [Symbol & Any * -> nil])
(+T clojure.core/require [& Any * -> nil])
(+T clojure.core/ns-name [Symbol -> String])
(+T clojure.core/*ns* Namespace)
(+T clojure.core/+ [& AnyNumber * -> AnyNumber])
(+T clojure.core/prn [& Any * -> nil])
(+T clojure.core/first (All [x]
                         [(NilSeqable x) -> (U nil x)]))
(+T clojure.core/rest (All [x]
                        [(NilSeqable x) -> (Seq x)]))
(+T clojure.core/next (All [x]
                        [(NilSeqable x) -> (NilSeq x)]))
(+T clojure.core/every? (All [x y]
                          [[x -> y] (NilSeqable x) -> pboolean]))
(+T clojure.core/set? (predicate APersistentSet))
(+T clojure.core/cons (All [x] 
                        [x (NilSeqable x) -> (Seq x)]))

(+T clojure.core/conj (All [x]
                        [(NilColl x) x & x * -> (Coll x)]))

(+T clojure.core/list (All [x]
                        [& x * -> (List x)]))

(+T clojure.core/second (All [x]
                          [(NilSeqable x) -> (U nil x)]))
;(+T clojure.core/ffirst (All [x]
;                          [(nseqable (nseqable x)) -> (U nil x)]))
;(+T clojure.core/nfirst (All [x]
;                          [(nseqable (nseqable x)) -> (nseqable x)]))
;(+T clojure.core/fnext (All [x]
;                          [(nseqable (nseqable x)) -> (U nil x)]))
;(+T clojure.core/nnext (All [x y]
;                          [(nseqable x) -> (nseqable y)]))
;(+T clojure.core/seq (All [x]
;                       [(nseqable x) -> (nseq x)]))
;;TODO predicate on 1st argument
;(+T clojure.core/instance? (All [y]
;                             [Class y -> boolean]))
;(+T clojure.core/seq? (predicate iseq))
;(+T clojure.core/char? (predicate icharacter))
;(+T clojure.core/map? (predicate imap))
;(+T clojure.core/vector? (predicate ivector))
;;; TODO repeated key-values
;(+T clojure.core/assoc (All [k v]
;                         [(nassociative k v) k v & Any * -> (associative k v)]))
;
;(+T clojure.core/meta (All [x]
;                        [(U Any (meta x)) -> x]))
;
;(+T clojure.core/with-meta (All [(x <! iobj) (y <! (map Any Any))]
;                             [x y -> (I x (meta y))])) ; remove previous metadata?
;
;(+T clojure.core/last (All [x]
;                        [(nseqable x) -> (U nil x)]))
;(+T clojure.core/butlast (All [x]
;                           [(nseqable x) -> (nseqable x)]))
;(+T clojure.core/cast (All [x]
;                        [Class x -> x]))
;(+T clojure.core/to-array [Collection -> Any]) ;TODO array types
;(+T clojure.core/vector (All [x]
;                          [& x * -> (vector x)]))
;(+T clojure.core/vec (All [x]
;                       [(nseqable x) -> (vector x)]))
;;TODO type key-value pairs
;(+T clojure.core/hash-map [& Any * -> (map Any Any)])
;(+T clojure.core/hash-set (All [x]
;                            [& x * -> (set x)]))
;;TODO key-value pairs
;(+T clojure.core/sorted-map [& Any * -> (sorted-map Any Any)])
;
;;; TODO key-value rest args
;(+T clojure.core/sorted-map-by (All [x]
;                                 [[x -> boolean] & Any * -> (sorted-map Any Any)]))
;(+T clojure.core/sorted-set (All [x]
;                              [& x * -> (sorted-set x)]))
;
;(+T clojure.core/sorted-set-by (All [x]
;                                 [[x -> boolean] & x * -> (sorted-set x)]))
;(+T clojure.core/nil? (predicate nil))
;
;(+T clojure.core/false? (predicate false))
;(+T clojure.core/true? (predicate true))
;(+T clojure.core/not [Any -> boolean])
;(+T clojure.core/str [& Any * -> String])
;(+T clojure.core/symbol? (predicate symbol))
;(+T clojure.core/keyword? (predicate keyword))
;(+T clojure.core/symbol (Fun [(U symbol string) -> symbol]
;                             [string string -> symbol]))
;(+T clojure.core/gensym (Fun [-> symbol]
;                             [string -> symbol]))
;(+T clojure.core/keyword (Fun [(U keyword string) -> keyword]
;                              [string string -> keyword]))
;(+T clojure.core/find-keyword (Fun [(U keyword string) -> (U nil keyword)]
;                                   [string string -> (U nil keyword)]))
;;;TODO any number of prefix arguments + special last param
;(+T clojure.core/list* [Any & Any * -> (list Any)])
;
;;;TODO any number of prefix arguments + special last param
;(+T clojure.core/apply (All [r]
;                         [[& Any * -> r] & Any * -> r]))
;
;;;TODO tricky, return arg
;(+T clojure.core/vary-meta (All [(x <! (meta Any))]
;                             [x [(map Any Any) & Any * -> Any] & Any * -> x]))
;
;;(+T clojure.core/chunk-buffer (All [x]
;;                                (ChunkBuffer x)))
;
;;; other chunk fns ..
;
;(+T clojure.core/concat (All [x]
;                          [& (nseqable x) * -> (seq x)]))
;
;(+T clojure.core/delay? (predicate idelay))
;
;;; TODO something interesting here, do disjoint union get in way?
;(+T clojure.core/force (All [x y]
;                         [(U (delay x) y) -> (U x y)]))
;
;(+T clojure.core/identical? (All [x y]
;                              [x y -> boolean]))
;;                               :filter
;;                               {:then [(= (class (arg 0)) 
;;                                          (class (arg 1)))]
;;                                :else 'tt}]))
;(+T clojure.core/= (All [x y]
;                     [x & y * -> boolean]))
;(+T clojure.core/not= (All [x y]
;                        [x & y * -> boolean]))
;
;(+T clojure.core/compare (All [(x <! Comparable) y]
;                           [x y -> number]))
;
;(+T clojure.core/zero? [number -> boolean])
;
;(+T clojure.core/count [(nseqable Any) -> integer])
;(+T clojure.core/int [(U char number) -> int])
;
;; interdependent bounds
;;; TODO works for regexs, how does that relate to Seqable?
;;; TODO nth seems extremely forgiving
;(+T clojure.core/nth (All [x y]
;                       (Fun [(nseqable x) integer -> x]
;                            [(Seqable x) Number y -> (U x y)])))
;
;(+T clojure.core/< [& number * -> boolean])
;(+T clojure.core/inc [number -> number])
;
;;; TODO interdependent bound
;(+T clojure.core/reverse (All [x]
;                           [(nseqable x) -> (seq x)]))
;
;(+T clojure.core/+' [& number * -> number])
;(+T clojure.core/+ [& number * -> number])
;(+T clojure.core/*' [& number * -> number])
;(+T clojure.core/* [& number * -> number])
;(+T clojure.core// [number & number * -> number])
;(+T clojure.core/-' [number & number * -> number])
;(+T clojure.core/- [number & number * -> number])
;
;(+T clojure.core/<= [number & number * -> boolean])
;(+T clojure.core/> [number & number * -> boolean])
;(+T clojure.core/>= [number & number * -> boolean])
;
;(+T clojure.core/== [Object & Object * -> boolean])
;
;(+T clojure.core/max [number & number * -> number])
;(+T clojure.core/min [number & number * -> number])
;
;(+T clojure.core/dec' [number -> number])
;(+T clojure.core/dec [number -> number])
;
;;; some unchecked arith ..
;
;;; some other arith ..
;
;; TODO dotted pretype
;(+T clojure.core/complement [[& Any * -> Any] -> [& Any * -> boolean]])
;(+T clojure.core/constantly (All [x]
;                              [x -> [& Any * -> x]]))
;(+T clojure.core/identity (All [x]
;                            [x -> x]))
;
;(+T clojure.core/peek (All [x]
;                        [(nseqable x) -> (U nil x)]))
;
;;interdependent bounds
;;TODO return value
;(+T clojure.core/pop (All [x]
;                       [(nseqable x) -> (seq x)]))
;
;(+T clojure.core/contains? [(nseqable Any) Any -> boolean])
;
;;; interdependent bounds
;;; has interesting filters
;(+T clojure.core/get (All [k v n]
;                       (Fun
;                         [(Associative k v) Any -> (U v nil)]
;                         [(Associative k v) Any n -> (U v n)])))
;
;; interdependent bounds
;;doesn't track keys
;;
;(+T clojure.core/dissoc (All [k v]
;                          [(nmap k v) & Any * -> (nmap k v)]))
;
;; TODO dotted pretype
;(+T clojure.core/disj (All [x]
;                        [(nset x) & Any * -> (nset x)]))
;
;(+T clojure.core/select-keys (All [k v]
;                               [(nmap Any v) (nseqable k) -> (map k v)]))
;
;(+T clojure.core/keys (All [k]
;                        [(nmap k Any) -> (nseq k)]))
;(+T clojure.core/vals (All [v]
;                        [(nmap Any v) -> (nseq v)]))
;
;(+T clojure.core/key (All [k]
;                       [(map-entry k Any) -> k]))
;(+T clojure.core/val (All [v]
;                       [(map-entry Any v) -> v]))
;
;;(def-type-alias ReversibleSeq [a] (I (Seqable a) Reversible))
;
;(+T clojure.core/rseq (All [x]
;                        [(I (seqable x) reversible) -> (nseq x)]))
;(+T clojure.core/name [(U string named) -> string])
;(+T clojure.core/namespace [named -> (U nil string)])
;
;;;TODO Multifn stuff ..
;
;(+T clojure.core/agent (All [x]
;                         [x & Any * -> (Agent x)]))
;
;;(+T clojure.core/agent (All [x (y <! IPersistentMap)]
;;                         [x & {:meta y
;;                               :validator [x -> Any]
;;                               :error-handler [Agent Throwable -> Any]
;;                               :error-mode (U :continue :failt)}
;;                          -> (I (Agent x)
;;                                (IReference y))]))
;
;;TODO arities take differing type arguments
;(+T clojure.core/deref (All [x y]
;                            (Fun 
;                              [(IDeref x) -> x]
;                              [(IBlockingDeref x) Long y -> (U x y)])))
;
;(+T clojure.core/atom (All [x]
;                        [x & Any * -> (Atom x)]))
;
;;(+T clojure.core/atom (All [x r m]
;;                        [x & {:validator [x -> r]
;;                              :meta (IPersistentMap m)} *
;;                         -> (I (IMeta (IPersistentMap m))
;;                               (Atom x))]))
;
;; interdependent bounds
;(+T clojure.core/swap! (All [x]
;                         [(Atom x) [x & Any * -> x] & Any * -> x]))
;
;; interdependent bounds
;(+T clojure.core/compare-and-set! (All [x]
;                                    [(Atom x) x x -> boolean]))
;
;; interdependent bounds
;(+T clojure.core/reset! (All [x]
;                          [(Atom x) x -> x]))
;
;;;TODO validators
;
;;TODO tricky!
;(+T clojure.core/comp [& IFn * -> [& Any * -> Any]])
;
;;TODO tricky!
;(+T clojure.core/juxt [& IFn * -> [& Any * -> Any]])
;
;;TODO tricky!
;(+T clojure.core/partial [[Any & Any * -> Any] -> [& Any * -> Any]])
;
;; If truthy result, we know Seqable argument is NonEmptySeqable
;; If falsy, either argument is EmptySeqable, or argument is NonEmptySeqable
;;   and r cannot
;(+T clojure.core/some (All [y r]
;                        [[y -> r] (Seqable y) -> (U nil r)]))
;;                         :filters
;;                         {:then [(refine-type (arg 2) NonEmptySeqable)]
;;                          :else tt}]))
;
;(+T clojure.core/not-any? [[& Any * -> Any] (Seqable Any) -> boolean])
;
;; dotted pretype
;(+T clojure.core/map (All [a d]
;                       [[a & Any * -> d] (Seqable a) & (Seqable Any) * -> (Seqable d)]))
;
;; dotted pretype
;(+T clojure.core/mapcat (All [a d]
;                          [[a & Any * -> (Seqable d)] (Seqable a) & (Seqable Any) * -> (Seqable d)]))
;
;(+T clojure.core/filter (All [x]
;                          [[x -> Any] (Seqable x) -> (Seqable x)]))
;
;(+T clojure.core/take (All [x]
;                        [(Seqable x) Number -> (Seqable x)]))
;
;(+T clojure.core/take-while (All [x]
;                              [[x -> Any] (Seqable x) -> (Seqable x)]))
;
;(+T clojure.core/drop (All [x]
;                        [(Seqable x) Number -> (Seqable x)]))
;
;(+T clojure.core/drop-last (All [x]
;                             (Fun [(Seqable x) -> (Seqable x)]
;                                  [(Seqable x) Number -> (Seqable x)])))
;
;(+T clojure.core/take-last (All [x]
;                             (Fun [(Seqable x) -> (Seqable x)]
;                                  [(Seqable x) Number -> (Seqable x)])))
;
;(+T clojure.core/drop-while (All [x]
;                              [[x -> Any] (Seqable x) -> (Seqable x)]))
;
;(+T clojure.core/cycle (All [x]
;                         [(Seqable x) -> (Seqable x)]))
;
;(+T clojure.core/split-at (All [x]
;                            [Number (Seqable x) -> (IPersistentVector (Seqable x))]))
;
;(+T clojure.core/split-with (All [x]
;                              [[x -> Any] (Seqable x) -> (IPersistentVector (Seqable x))]))
;
;(+T clojure.core/repeat (All [x]
;                          (Fun [x -> (Seqable x)]
;                               [Number x -> (Seqable x)])))
;
;(+T clojure.core/iterate (All [x]
;                           [[x -> x] x -> (Seqable x)]))
;
;(+T clojure.core/range (Fun [-> (ISeq Number)]
;                            [Number -> (ISeq Number)]
;                            [Number Number -> (ISeq Number)]
;                            [Number Number Number -> (ISeq Number)]))
;
;;merging different kinds of Maps?
;(+T clojure.core/merge (All [(x <! IPersistentMap)]
;                         [& x * -> x]))
;
;;interdependent bounds
;(+T clojure.core/merge-with (All [k v]
;                              [[v v -> v] & (IPersistentMap k v) * -> (IPersistentMap k v)]))
;
;(+T clojure.core/zipmap (All [k v]
;                          [(Seqable k) (Seqable v) -> (IPersistentMap k v)]))
;
;(+T clojure.core/partition (All [x]
;                             [Number (Seqable x) -> (Seqable (IPersistentList x))]))
;
;(+T clojure.core/eval (All [x y]
;                        [x -> y]))
;
;;(+T clojure.core/reduce (All [x]
;;                          (Fun 
;;                            [[-> x] (EmptySeqable x) -> x]
;;                            [[x x -> x] (NonEmptySeqable x) -> x]
;;                            [[-> x] x (EmptySeqable x) -> x]
;;                            [[x x -> x] x (NonEmptySeqable x) -> x])))
