(ns typed.base
  (:import (java.util Collection))
  (:require [typed.core :refer [add-type-ann Any IParseType Nothing parse-typed-ns]]))

(let [base-ns
      (parse-typed-ns 'typed.base
                      '((:import (clojure.lang Symbol Namespace IPersistentMap Var Keyword Namespace ISeq Seqable
                                               Atom IRef IObj IPersistentList IDeref IRef IMeta Named ChunkBuffer
                                               IPersistentSet IPersistentVector Associative Sorted Delay IMapEntry
                                               Reversible Agent IBlockingDeref IFn APersistentMap))))

      _ (swap! typed.core/namespaces assoc-in [(:name base-ns)] base-ns)])

(defmacro type-ann [sym tsyn]
  `(add-type-ann '~sym '~tsyn))

(type-ann clojure.core/in-ns [symbol -> namespace])
(type-ann clojure.core/import [& (U symbol (nlist symbol)) * -> nil])
(type-ann clojure.core/find-ns [symbol -> (U nil namespace)])
(type-ann clojure.core/resolve 
    (Fun [symbol -> (U nil var Class)]
         [(nmap symbol Any) symbol -> (U nil var Class)]))
(type-ann clojure.core/refer [symbol & Any * -> nil])
(type-ann clojure.core/require [& Any * -> nil])
(type-ann clojure.core/ns-name [symbol -> string])
(type-ann clojure.core/*ns* namespace)
(type-ann clojure.core/+ [& number * -> number])
(type-ann clojure.core/prn [& Any * -> nil])
(type-ann clojure.core/first (All [x]
                         [(nseqable x) -> (U nil x)]))
(type-ann clojure.core/rest (All [x]
                        [(nseqable x) -> (seq x)]))
(type-ann clojure.core/next (All [x]
                        [(nseqable x) -> (nseq x)]))
(type-ann clojure.core/every? (All [x y]
                          [[x -> y] (nseqable x) -> boolean]))
(type-ann clojure.core/set? (predicate iset))
(type-ann clojure.core/set-validator! (All [x r]
                                  [(IRef x) (U nil [x -> r]) -> nil]))
(type-ann clojure.core/cons (All [x] 
                        [x (nseqable x) -> (seq x)]))

(type-ann clojure.core/conj (All [x]
                        [(ncoll x) x & x * -> (coll x)]))

(type-ann clojure.core/list (All [x]
                        [& x * -> (list x)]))

(type-ann clojure.core/second (All [x]
                          [(nseqable x) -> (U nil x)]))
(type-ann clojure.core/ffirst (All [x]
                          [(nseqable (nseqable x)) -> (U nil x)]))
(type-ann clojure.core/nfirst (All [x]
                          [(nseqable (nseqable x)) -> (nseqable x)]))
(type-ann clojure.core/fnext (All [x]
                          [(nseqable (nseqable x)) -> (U nil x)]))
(type-ann clojure.core/nnext (All [x y]
                          [(nseqable x) -> (nseqable y)]))
(type-ann clojure.core/seq (All [x]
                       [(nseqable x) -> (nseq x)]))
;TODO predicate on 1st argument
(type-ann clojure.core/instance? (All [y]
                             [Class y -> boolean]))
(type-ann clojure.core/seq? (predicate iseq))
(type-ann clojure.core/char? (predicate icharacter))
(type-ann clojure.core/map? (predicate imap))
(type-ann clojure.core/vector? (predicate ivector))
;; TODO repeated key-values
(type-ann clojure.core/assoc (All [k v]
                         [(nassociative k v) k v & Any * -> (associative k v)]))

(type-ann clojure.core/meta (All [x]
                        [(U Any (meta x)) -> x]))

(type-ann clojure.core/with-meta (All [(x <! iobj) (y <! (map Any Any))]
                             [x y -> (I x (meta y))])) ; remove previous metadata?

(type-ann clojure.core/last (All [x]
                        [(nseqable x) -> (U nil x)]))
(type-ann clojure.core/butlast (All [x]
                           [(nseqable x) -> (nseqable x)]))
(type-ann clojure.core/cast (All [x]
                        [Class x -> x]))
(type-ann clojure.core/to-array [Collection -> Any]) ;TODO array types
(type-ann clojure.core/vector (All [x]
                          [& x * -> (vector x)]))
(type-ann clojure.core/vec (All [x]
                       [(nseqable x) -> (vector x)]))
;TODO type key-value pairs
(type-ann clojure.core/hash-map [& Any * -> (map Any Any)])
(type-ann clojure.core/hash-set (All [x]
                            [& x * -> (set x)]))
;TODO key-value pairs
(type-ann clojure.core/sorted-map [& Any * -> (sorted-map Any Any)])

;; TODO key-value rest args
(type-ann clojure.core/sorted-map-by (All [x]
                                 [[x -> boolean] & Any * -> (sorted-map Any Any)]))
(type-ann clojure.core/sorted-set (All [x]
                              [& x * -> (sorted-set x)]))

(type-ann clojure.core/sorted-set-by (All [x]
                                 [[x -> boolean] & x * -> (sorted-set x)]))
(type-ann clojure.core/nil? (predicate nil))

(type-ann clojure.core/false? (predicate false))
(type-ann clojure.core/true? (predicate true))
(type-ann clojure.core/not [Any -> boolean])
(type-ann clojure.core/str [& Any * -> String])
(type-ann clojure.core/symbol? (predicate symbol))
(type-ann clojure.core/keyword? (predicate keyword))
(type-ann clojure.core/symbol (Fun [(U symbol string) -> symbol]
                             [string string -> symbol]))
(type-ann clojure.core/gensym (Fun [-> symbol]
                             [string -> symbol]))
(type-ann clojure.core/keyword (Fun [(U keyword string) -> keyword]
                              [string string -> keyword]))
(type-ann clojure.core/find-keyword (Fun [(U keyword string) -> (U nil keyword)]
                                   [string string -> (U nil keyword)]))
;;TODO any number of prefix arguments + special last param
(type-ann clojure.core/list* [Any & Any * -> (list Any)])

;;TODO any number of prefix arguments + special last param
(type-ann clojure.core/apply (All [r]
                         [[& Any * -> r] & Any * -> r]))

;;TODO tricky, return arg
(type-ann clojure.core/vary-meta (All [(x <! (meta Any))]
                             [x [(map Any Any) & Any * -> Any] & Any * -> x]))

;(type-ann clojure.core/chunk-buffer (All [x]
;                                (ChunkBuffer x)))

;; other chunk fns ..

(type-ann clojure.core/concat (All [x]
                          [& (nseqable x) * -> (seq x)]))

(type-ann clojure.core/delay? (predicate idelay))

;; TODO something interesting here, do disjoint union get in way?
(type-ann clojure.core/force (All [x y]
                         [(U (delay x) y) -> (U x y)]))

(type-ann clojure.core/identical? (All [x y]
                              [x y -> boolean]))
;                               :filter
;                               {:then [(= (class (arg 0)) 
;                                          (class (arg 1)))]
;                                :else 'tt}]))
(type-ann clojure.core/= (All [x y]
                     [x & y * -> boolean]))
(type-ann clojure.core/not= (All [x y]
                        [x & y * -> boolean]))

(type-ann clojure.core/compare (All [(x <! Comparable) y]
                           [x y -> number]))

(type-ann clojure.core/zero? [number -> boolean])

(type-ann clojure.core/count [(nseqable Any) -> integer])
(type-ann clojure.core/int [(U char number) -> int])

; interdependent bounds
;; TODO works for regexs, how does that relate to Seqable?
;; TODO nth seems extremely forgiving
(type-ann clojure.core/nth (All [x y]
                       (Fun [(nseqable x) integer -> x]
                            [(Seqable x) Number y -> (U x y)])))

(type-ann clojure.core/< [& number * -> boolean])
(type-ann clojure.core/inc [number -> number])

;; TODO interdependent bound
(type-ann clojure.core/reverse (All [x]
                           [(nseqable x) -> (seq x)]))

(type-ann clojure.core/+' [& number * -> number])
(type-ann clojure.core/+ [& number * -> number])
(type-ann clojure.core/*' [& number * -> number])
(type-ann clojure.core/* [& number * -> number])
(type-ann clojure.core// [number & number * -> number])
(type-ann clojure.core/-' [number & number * -> number])
(type-ann clojure.core/- [number & number * -> number])

(type-ann clojure.core/<= [number & number * -> boolean])
(type-ann clojure.core/> [number & number * -> boolean])
(type-ann clojure.core/>= [number & number * -> boolean])

(type-ann clojure.core/== [Object & Object * -> boolean])

(type-ann clojure.core/max [number & number * -> number])
(type-ann clojure.core/min [number & number * -> number])

(type-ann clojure.core/dec' [number -> number])
(type-ann clojure.core/dec [number -> number])

;; some unchecked arith ..

;; some other arith ..

; TODO dotted pretype
(type-ann clojure.core/complement [[& Any * -> Any] -> [& Any * -> boolean]])
(type-ann clojure.core/constantly (All [x]
                              [x -> [& Any * -> x]]))
(type-ann clojure.core/identity (All [x]
                            [x -> x]))

(type-ann clojure.core/peek (All [x]
                        [(nseqable x) -> (U nil x)]))

;interdependent bounds
;TODO return value
(type-ann clojure.core/pop (All [x]
                       [(nseqable x) -> (seq x)]))

(type-ann clojure.core/contains? [(nseqable Any) Any -> boolean])

;; interdependent bounds
;; has interesting filters
(type-ann clojure.core/get (All [k v n]
                       (Fun
                         [(Associative k v) Any -> (U v nil)]
                         [(Associative k v) Any n -> (U v n)])))

; interdependent bounds
;doesn't track keys
;
(type-ann clojure.core/dissoc (All [k v]
                          [(nmap k v) & Any * -> (nmap k v)]))

; TODO dotted pretype
(type-ann clojure.core/disj (All [x]
                        [(nset x) & Any * -> (nset x)]))

(type-ann clojure.core/select-keys (All [k v]
                               [(nmap Any v) (nseqable k) -> (map k v)]))

(type-ann clojure.core/keys (All [k]
                        [(nmap k Any) -> (nseq k)]))
(type-ann clojure.core/vals (All [v]
                        [(nmap Any v) -> (nseq v)]))

(type-ann clojure.core/key (All [k]
                       [(map-entry k Any) -> k]))
(type-ann clojure.core/val (All [v]
                       [(map-entry Any v) -> v]))

;(def-type-alias ReversibleSeq [a] (I (Seqable a) Reversible))

(type-ann clojure.core/rseq (All [x]
                        [(I (seqable x) reversible) -> (nseq x)]))
(type-ann clojure.core/name [(U string named) -> string])
(type-ann clojure.core/namespace [named -> (U nil string)])

;;TODO Multifn stuff ..

(type-ann clojure.core/agent (All [x]
                         [x & Any * -> (Agent x)]))

;(type-ann clojure.core/agent (All [x (y <! IPersistentMap)]
;                         [x & {:meta y
;                               :validator [x -> Any]
;                               :error-handler [Agent Throwable -> Any]
;                               :error-mode (U :continue :failt)}
;                          -> (I (Agent x)
;                                (IReference y))]))

;TODO arities take differing type arguments
(type-ann clojure.core/deref (All [x y]
                            (Fun 
                              [(IDeref x) -> x]
                              [(IBlockingDeref x) Long y -> (U x y)])))

(type-ann clojure.core/atom (All [x]
                        [x & Any * -> (Atom x)]))

;(type-ann clojure.core/atom (All [x r m]
;                        [x & {:validator [x -> r]
;                              :meta (IPersistentMap m)} *
;                         -> (I (IMeta (IPersistentMap m))
;                               (Atom x))]))

; interdependent bounds
(type-ann clojure.core/swap! (All [x]
                         [(Atom x) [x & Any * -> x] & Any * -> x]))

; interdependent bounds
(type-ann clojure.core/compare-and-set! (All [x]
                                    [(Atom x) x x -> boolean]))

; interdependent bounds
(type-ann clojure.core/reset! (All [x]
                          [(Atom x) x -> x]))

;;TODO validators

;TODO tricky!
(type-ann clojure.core/comp [& IFn * -> [& Any * -> Any]])

;TODO tricky!
(type-ann clojure.core/juxt [& IFn * -> [& Any * -> Any]])

;TODO tricky!
(type-ann clojure.core/partial [[Any & Any * -> Any] -> [& Any * -> Any]])

; If truthy result, we know Seqable argument is NonEmptySeqable
; If falsy, either argument is EmptySeqable, or argument is NonEmptySeqable
;   and r cannot
(type-ann clojure.core/some (All [y r]
                        [[y -> r] (Seqable y) -> (U nil r)]))
;                         :filters
;                         {:then [(refine-type (arg 2) NonEmptySeqable)]
;                          :else tt}]))

(type-ann clojure.core/not-any? [[& Any * -> Any] (Seqable Any) -> boolean])

; dotted pretype
(type-ann clojure.core/map (All [a d]
                       [[a & Any * -> d] (Seqable a) & (Seqable Any) * -> (Seqable d)]))

; dotted pretype
(type-ann clojure.core/mapcat (All [a d]
                          [[a & Any * -> (Seqable d)] (Seqable a) & (Seqable Any) * -> (Seqable d)]))

(type-ann clojure.core/filter (All [x]
                          [[x -> Any] (Seqable x) -> (Seqable x)]))

(type-ann clojure.core/take (All [x]
                        [(Seqable x) Number -> (Seqable x)]))

(type-ann clojure.core/take-while (All [x]
                              [[x -> Any] (Seqable x) -> (Seqable x)]))

(type-ann clojure.core/drop (All [x]
                        [(Seqable x) Number -> (Seqable x)]))

(type-ann clojure.core/drop-last (All [x]
                             (Fun [(Seqable x) -> (Seqable x)]
                                  [(Seqable x) Number -> (Seqable x)])))

(type-ann clojure.core/take-last (All [x]
                             (Fun [(Seqable x) -> (Seqable x)]
                                  [(Seqable x) Number -> (Seqable x)])))

(type-ann clojure.core/drop-while (All [x]
                              [[x -> Any] (Seqable x) -> (Seqable x)]))

(type-ann clojure.core/cycle (All [x]
                         [(Seqable x) -> (Seqable x)]))

(type-ann clojure.core/split-at (All [x]
                            [Number (Seqable x) -> (IPersistentVector (Seqable x))]))

(type-ann clojure.core/split-with (All [x]
                              [[x -> Any] (Seqable x) -> (IPersistentVector (Seqable x))]))

(type-ann clojure.core/repeat (All [x]
                          (Fun [x -> (Seqable x)]
                               [Number x -> (Seqable x)])))

(type-ann clojure.core/iterate (All [x]
                           [[x -> x] x -> (Seqable x)]))

(type-ann clojure.core/range (Fun [-> (ISeq Number)]
                            [Number -> (ISeq Number)]
                            [Number Number -> (ISeq Number)]
                            [Number Number Number -> (ISeq Number)]))

;merging different kinds of Maps?
(type-ann clojure.core/merge (All [(x <! IPersistentMap)]
                         [& x * -> x]))

;interdependent bounds
(type-ann clojure.core/merge-with (All [k v]
                              [[v v -> v] & (IPersistentMap k v) * -> (IPersistentMap k v)]))

(type-ann clojure.core/zipmap (All [k v]
                          [(Seqable k) (Seqable v) -> (IPersistentMap k v)]))

(type-ann clojure.core/partition (All [x]
                             [Number (Seqable x) -> (Seqable (IPersistentList x))]))

(type-ann clojure.core/eval (All [x y]
                        [x -> y]))

;(type-ann clojure.core/reduce (All [x]
;                          (Fun 
;                            [[-> x] (EmptySeqable x) -> x]
;                            [[x x -> x] (NonEmptySeqable x) -> x]
;                            [[-> x] x (EmptySeqable x) -> x]
;                            [[x x -> x] x (NonEmptySeqable x) -> x])))
