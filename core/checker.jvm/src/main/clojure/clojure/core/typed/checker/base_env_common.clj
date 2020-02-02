;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.base-env-common
  "Utilities for all implementations of the type checker"
  (:require [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.tools.reader :as rdr]
            [clojure.tools.reader.reader-types :as rdrs]))

(defmacro delay-and-cache-env [sym & body]
  (let [generator-sym (symbol (str "generator-" sym))
        cache-sym (symbol (str "cache-" sym))
        thread-bindings (symbol (str "thread-bindings-" sym))
        interface-sym sym]
    `(do
       (def ~thread-bindings (get-thread-bindings))
       (defn ~(with-meta generator-sym {:private true}) []
         ; switch namespace to where this def is defined
         ; Also helps parse CLJS syntax.
         (let [r# (with-bindings ~thread-bindings
                    ~@body)]
           ;(prn "r" r#)
           r#))
       ; cache is original nil, then is updated only once
       (def ~(with-meta cache-sym {:private true})
         (atom nil))
       (defn ~interface-sym []
         (if-let [hit# (deref ~cache-sym)]
           hit#
           (let [calc# (~generator-sym)]
             (reset! ~cache-sym calc#)))))))


(defn parse-cljs-ann-map
  [ann-map]
  (into {}
        (map (fn [[sym ann]]
               [(symbol "cljs.core" (name sym))
                (prs/parse-type ann)])
             ann-map)))


(defn parse-clj-ann-map
  [ann-map]
  (let [conveyed-parse (bound-fn* prs/parse-type)]
    (into {}
          (map (fn [[sym ann]]
                 [sym (delay (conveyed-parse ann))])
               ann-map))))


;;these annotations can be parsed in either {cljs,clojure}.core.typed
;;and have the same meaning.
  ;;ordered the same as in cljs.core
(def common-ann*
  "
    clojure.core/*1 Any
    clojure.core/*2 Any
    clojure.core/*3 Any
    clojure.core/identical? [Any Any -> Boolean]
    clojure.core/number? (Pred Number)
    clojure.core/not [Any -> Boolean]
    clojure.core/string? (Pred String)
    clojure.core/type [Any -> Any]
    clojure.core/aclone (All [x] [(ReadOnlyArray x) -> (Array x)])

    clojure.core/aget
    (All [x]
         (IFn [(ReadOnlyArray x)
               AnyInteger -> x]
              [(ReadOnlyArray (ReadOnlyArray x))
               AnyInteger AnyInteger -> x]
              [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))
               AnyInteger AnyInteger AnyInteger -> x]
              [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x))))
               AnyInteger AnyInteger AnyInteger AnyInteger -> x]
                                        ; don't support unsound cases
              [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))))
               AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger -> x]))
    clojure.core/aset
    (All [x]
         (IFn
          [(Array x) AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger x -> x]
          [(Array x) AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger AnyInteger x -> x]))

    clojure.core/alength [(ReadOnlyArray Any) -> AnyInteger]
    ;clojure.core/instance? [Class Any -> Booelan]
    clojure.core/symbol? (Pred Symbol)
    clojure.core/symbol (IFn [(U Symbol String) -> Symbol]
                             [(U nil String) String -> Symbol])
    clojure.core/seq (All [x]
                          (IFn
                           [(NonEmptyColl x) -> (NonEmptyASeq x)]
                           [(Option (Coll x)) -> (Option (NonEmptyASeq x))
                            :filters {:then (& (is NonEmptyCount 0)
                                               (! nil 0))
                                      :else (| (is nil 0)
                                               (is EmptyCount 0))}]
                           [(Option (Seqable x)) -> (Option (NonEmptyASeq x))]))
    clojure.core/first (All [x]
                            (IFn [(HSequential [x Any *]) -> x
                                  :object {:id 0 :path [(Nth 0)]}]
                                 [(Option (EmptySeqable x)) -> nil]
                                 [(NonEmptySeqable x) -> x]
                                 [(Option (Seqable x)) -> (Option x)]))
    clojure.core/rest (All [x]
                           [(Option (Seqable x)) -> (ASeq x)])
    clojure.core/next (All [x]
                           (IFn [(Option (Coll x)) -> (Option (NonEmptyASeq x))
                                 :filters {:then (& (is (CountRange 2) 0)
                                                    (! nil 0))
                                           :else (| (is (CountRange 0 1) 0)
                                                    (is nil 0))}]
                                [(Option (Seqable x)) -> (Option (NonEmptyASeq x))]))
    clojure.core/= [Any Any * -> Boolean]
    ;clojure.core/reduced (All [x] [x -> (Reduced x)])
    ;clojure.core/reduced? (Pred (Reduced Any))
    clojure.core/second
    (All [x]
         (IFn [(HSequential [Any x Any *]) -> x
               :object {:id 0 :path [(Nth 1)]}]
              [(Option (I (Seqable x) (CountRange 0 1))) -> nil]
              [(I (Seqable x) (CountRange 2)) -> x]
              [(Option (Seqable x)) -> (Option x)]))
    clojure.core/ffirst (All [x]
                             [(Option (Seqable (U nil (Seqable x)))) -> (Option x)])
                                        ;clojure.core/nfirst
    #_(All [x]
           [(Option (Seqable (Option (Seqable x))))
            -> (Option (NonEmptyASeq x))])
                                        ;clojure.core/fnext
    #_(All [x]
           [(Option (Seqable (Option (Seqable x)))) -> (Option x)])
    clojure.core/nnext (All [x]
                            [(Option (Seqable x)) -> (Option (NonEmptyASeq x))])
    clojure.core/last (All [x]
                           (IFn [(NonEmptySeqable x) -> x]
                                [(Option (Seqable x)) -> (U nil x)]))
    clojure.core/conj (All [x y]
                           (IFn [(IPersistentVector x) x x * -> (IPersistentVector x)]
                                [(APersistentMap x y)
                                 (U nil (Seqable (IMapEntry x y))
                                    (IMapEntry x y) '[x y] (Map x y))
                                 (U nil (Seqable (IMapEntry x y))
                                    (IMapEntry x y) '[x y] (Map x y)) *
                                    -> (APersistentMap x y)]
                                [(IPersistentMap x y)
                                 (U nil (Seqable (IMapEntry x y))
                                    (IMapEntry x y) '[x y] (Map x y))
                                 (U nil (Seqable (IMapEntry x y))
                                    (IMapEntry x y) '[x y] (Map x y)) * -> (IPersistentMap x y)]
                                [(IPersistentSet x) x x * -> (IPersistentSet x)]
                                [(ASeq x) x x * -> (ASeq x)]
                                ;[nil x x * -> (clojure.lang.PersistentList x)]
                                [(Coll Any) Any Any * -> (Coll Any)]))
    clojure.core/get (All [x y]
                          (IFn
                           ;;no default
                           [(U nil (Set x) (ILookup Any x)) Any -> (Option x)]
                           ;[(Option String) Any -> (Option Character)]
                           ;;default
                           [(U nil (Set x) (ILookup Any x)) Any y -> (U y x)]
                           [(Option (Map x y)) x -> (Option y)]
                           [(Option (Map x y)) x y -> y]
                           [(Option (Map Any Any)) Any -> (Option Any)]
                           [(Option (Map Any Any)) Any y -> (U y Any)]
                           ;[(Option String) Any y -> (U y Character)]
                           ))
    clojure.core/assoc (All [b c d]
                            (IFn [(Map b c) b c -> (Map b c)]
                                 [(Vec d) AnyInteger d -> (Vec d)]))
    clojure.core/dissoc (All [k v]
                             (IFn [(Map k v) Any * -> (Map k v)]))
                                        ;clojure.core/with-meta
    #_(All [[x :< clojure.lang.IObj]]
           [x (U nil (Map Any Any)) -> x])
    clojure.core/meta [Any -> (U nil (Map Any Any))]
    clojure.core/peek
    (All [x]
           (IFn [(I NonEmptyCount (Stack x)) -> x]
                [(Stack x) -> x]))
    ;;clojure.core/pop (All [x]
    ;;                      (IFn
    ;;                       [(List x) -> (List x)]
    ;;                       [(Vec x) -> (Vec x)]
    ;;                       [(Stack x) -> (Stack x)]))
    clojure.core/disj
    (All [x]
         (IFn #_[(SortedSet x) Any Any * -> (SortedSet x)]
              [(Set x) Any Any * -> (Set x)]))
    clojure.core/hash [Any -> AnyInteger]
    clojure.core/empty? (IFn [(Option (HSequential [Any *])) -> Boolean
                              :filters {:then (| (is EmptyCount 0)
                                                 (is nil 0))
                                        :else (is NonEmptyCount 0)}]
                             [(Option (Coll Any)) -> Boolean
                              :filters {:then (| (is EmptyCount 0)
                                                 (is nil 0))
                                        :else (is NonEmptyCount 0)}]
                             [(Option (Seqable Any)) -> Boolean])
    clojure.core/coll? (Pred (Coll Any))
                                        ;clojure.core/set? (Pred (Set Any))
    ;clojure.core/associative? (Pred (clojure.lang.Associative Any Any Any))
    ;clojure.core/sequential? (Pred clojure.lang.Sequential)
                                        ;clojure.core/sorted? (Pred Sorted)
    clojure.core/map? (Pred (Map Any Any))
    clojure.core/vector? (Pred (Vec Any))
    clojure.core/chunked-seq? [Any -> Any]
    clojure.core/false? (Pred false)
    clojure.core/true? (Pred true)
    clojure.core/seq? (Pred (Seq Any))
    clojure.core/boolean [Any -> Boolean]
    clojure.core/integer? (Pred AnyInteger)
    clojure.core/contains? [(Option (Seqable Any)) Any -> Boolean]
    clojure.core/find (All [x y]
                           [(U nil (Associative Any x y)) Any -> (U nil (HVec [x y]))])
    clojure.core/distinct? [Any Any * -> Boolean]
    clojure.core/compare [Any Any -> Number]
    clojure.core/sort (All [x]
                           (IFn [(U nil (Seqable x)) -> (U nil (ASeq x))]
                                [[x x -> AnyInteger] (U nil (Seqable x)) -> (U nil (ASeq x))]))
    clojure.core/shuffle (All [x]
                              (IFn [(I (Collection x) (Seqable x)) -> (Vec x)]
                                   [(Collection x) -> (Vec x)]))
    clojure.core/reduce
    (All [a c]
         (IFn
          ;;Without accumulator
          ;; default
          ;; (reduce + my-coll)
          [[a c -> (U (Reduced a) a)] (NonEmptySeqable c) -> a]
          [(IFn [a c -> (U (Reduced a) a)] [-> (U (Reduced a) a)]) (Option (Seqable c)) -> a]
          ;; default
          ;; (reduce + 3 my-coll)
          [[a c -> (U (Reduced a) a)] a (Option (Seqable c)) -> a]))
    clojure.core/reduce-kv
    (All [a c k v]
         [[a k v -> (U (Reduced a) a)] a (Option (Associative Any k v)) -> a])
    clojure.core/< [Number Number * -> Boolean]
    clojure.core/<= [Number Number * -> Boolean]
    clojure.core/> [Number Number * -> Boolean]
    clojure.core/>= [Number Number * -> Boolean]
    clojure.core/== [Number Number * -> Boolean]
    clojure.core/max [Number Number * -> Number]
    clojure.core/min [Number Number * -> Number]
    clojure.core/int (IFn [Number -> Integer]
                          ;[Character -> Integer]
                          )
    clojure.core/booleans [Any -> (Array boolean)]
    clojure.core/ints [Any -> (Array int)]
    clojure.core/mod (IFn [AnyInteger AnyInteger -> AnyInteger]
                          [Number Number -> Number])
    clojure.core/rand (IFn [-> Number]
                           [Number -> Number])
    clojure.core/rand-int [Int -> Int]
    clojure.core/bit-xor [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
    clojure.core/bit-or [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
    clojure.core/bit-and-not [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
    clojure.core/bit-clear [AnyInteger AnyInteger -> AnyInteger]
    clojure.core/bit-flip [AnyInteger AnyInteger -> AnyInteger]
    clojure.core/bit-not [AnyInteger -> AnyInteger]
    clojure.core/bit-set [AnyInteger AnyInteger -> AnyInteger]
    clojure.core/bit-test [AnyInteger AnyInteger -> AnyInteger]
    clojure.core/bit-shift-left [AnyInteger AnyInteger -> AnyInteger]
    clojure.core/bit-shift-right [AnyInteger AnyInteger -> AnyInteger]
    clojure.core/pos? (IFn [Number -> Boolean])
    clojure.core/neg? (IFn [Number -> Boolean])
    clojure.core/nthnext
    (All [x]
         (IFn [nil AnyInteger -> nil]
              [(Option (Seqable x)) AnyInteger -> (Option (NonEmptyASeq x))]))
    clojure.core/str [Any * -> String]
    clojure.core/subs (IFn [String AnyInteger -> String]
                           [String AnyInteger AnyInteger -> String])
    clojure.core/hash-combine [AnyInteger Any -> AnyInteger]
    ;clojure.core/rseq
    #_(All [x]
         [(Reversible x) -> (Option (NonEmptyASeq x))])
    clojure.core/reverse (All [x]
                              [(Option (Seqable x)) -> (ASeq x)])
    clojure.core/list (All [x] [x * -> (PersistentList x)])
    clojure.core/cons (All [x]
                           [x (Option (Seqable x)) -> (ASeq x)])
    clojure.core/list? (Pred (List Any))
    clojure.core/keyword? (Pred Keyword)
    clojure.core/namespace [(U Symbol String Keyword) -> (Option String)]
    clojure.core/keyword (IFn [(U Keyword Symbol String) -> Keyword]
                              [String String -> Keyword])
    #_clojure.core/chunk-cons
    #_(All [x]
         [(clojure.lang.IChunk x) (Option (Seqable x)) -> (Option (Seqable x))])
    #_clojure.core/chunk-append 
    #_(All [x]
                                   [(clojure.lang.ChunkBuffer x) x -> Any])
    #_clojure.core/chunk
    #_(All [x]
         [(clojure.lang.ChunkBuffer x) -> (clojure.lang.IChunk x)])
    #_clojure.core/chunk-first #_(All [x]
                                  ;;should be IChunkedSeq -> IChunk
                                  [(Seqable x) -> (clojure.lang.IChunk x)])
    #_clojure.core/chunk-rest
    #_(All [x]
         ;;should be IChunkRest -> Seq
         [(clojure.lang.Seqable x) -> (ASeq x)])
    clojure.core/int-array
    (IFn [(U nil Number (Seqable Number)) -> (Array int)]
         [Number (U nil Number (Seqable Number)) -> (Array int)])
    clojure.core/concat (All [x] [(Option (Seqable x)) * -> (ASeq x)])
    clojure.core/list*
    (All [x]
         (IFn [(U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x x x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]
              [x x x x x x x x x x (U nil (Seqable x)) -> (NilableNonEmptyASeq x)]))
    clojure.core/apply
    (All [y a b c d r z ...]
         (IFn [[z ... z -> y] (HSequential [z ... z]) -> y]
              [[a z ... z -> y] a (HSequential [z ... z]) -> y]
              [[a b z ... z -> y] a b (HSequential [z ... z]) -> y]
              [[a b c z ... z -> y] a b c (HSequential [z ... z]) -> y]
              [[a b c d z ... z -> y] a b c d (HSequential [z ... z]) -> y]
              [[r * -> y] (U nil (Seqable r)) -> y]
              [[a r * -> y] a (U nil (Seqable r)) -> y]
              [[a b r * -> y] a b (U nil (Seqable r)) -> y]
              [[a b c r * -> y] a b c (U nil (Seqable r)) -> y]
              [[a b c d r * -> y] a b c d (U nil (Seqable r)) -> y]))
                                        ;clojure.core/vary-meta
    #_(All [[x :< clojure.lang.IObj] b ...]
           [x [(U nil (Map Any Any)) b ... b -> (U nil (Map Any Any))] b ... b -> x])
    clojure.core/not= [Any Any * -> Boolean]
    clojure.core/every?
    (All [x y]
         (IFn [[x -> Any :filters {:then (is y 0)}] (Coll x) -> Boolean
               :filters {:then (is (Coll y) 1)}]
                                        ; argument could be nil
              [[x -> Any :filters {:then (is y 0)}] (U nil (Coll x)) -> Boolean
               :filters {:then (is (U nil (Coll y)) 1)}]
              [[x -> Any] (U nil (Seqable x)) -> Boolean]))
    clojure.core/some (All [x y] [[x -> y] (Option (Seqable x)) -> (Option y)])
    clojure.core/even? [AnyInteger -> Boolean]
    clojure.core/odd? [AnyInteger -> Boolean]
    clojure.core/identity (All [x] [x -> x
                                    :filters {:then (! (U nil false) 0)
                                              :else (is (U nil false) 0)}
                                    :object {:id 0}])
    clojure.core/complement (All [x] [[x -> Any] -> [x -> Boolean]])
    clojure.core/constantly (All [x] [x -> [Any * -> x]])
    clojure.core/comp (All [x y b ...]
                           [[x -> y] [b ... b -> x] -> [b ... b -> y]])
    clojure.core/partial
    (All [y a b c d z ...]
         (IFn [[z ... z -> y] -> [z ... z -> y]]
              [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a * -> y] a * -> [a * -> y]]))
    clojure.core/fnil
    (All [x y z a b ...]
         (IFn [[x b ... b -> a] x -> [(U nil x) b ... b -> a]]
              [[x y b ... b -> a] x y -> [(U nil x) (U nil y) b ... b -> a]]
              [[x y z b ... b -> a] x y z -> [(U nil x) (U nil y) (U nil z) b ... b -> a]]))
    clojure.core/map-indexed
    (All [x y] [[AnyInteger x -> y] (Option (Seqable x)) -> (Seqable y)])

    clojure.core/every-pred
    (All [t0 t1 t2 t3 t4 t5]
         (IFn [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
               -> (IFn [Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
                       [Any * -> Any])]
              [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
               [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
               -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1) 0) :else (! (I t0 t1) 0)}]
                       [Any * -> Any])]
              [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
               [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
               [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
               -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2) 0) :else (! (I t0 t1 t2) 0)}]
                       [Any * -> Any])]
              [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
               [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
               [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
               [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
               -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2 t3) 0) :else (! (I t0 t1 t2 t3) 0)}]
                       [Any * -> Any])]
              [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
               [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
               [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
               [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
               [Any -> Boolean :filters {:then (is t4 0) :else (! t4 0)}]
               -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2 t3 t4) 0) :else (! (I t0 t1 t2 t3 t4) 0)}]
                       [Any * -> Any])]
              [[Any -> Any :filters {:then (is t0 0) :else (! t0 0)}]
               [Any -> Any :filters {:then (is t1 0) :else (! t1 0)}]
               [Any -> Any :filters {:then (is t2 0) :else (! t2 0)}]
               [Any -> Any :filters {:then (is t3 0) :else (! t3 0)}]
               [Any -> Any :filters {:then (is t4 0) :else (! t4 0)}]
               [Any -> Any :filters {:then (is t5 0) :else (! t5 0)}]
               -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2 t3 t4 t5) 0) :else (! (I t0 t1 t2 t3 t4 t5) 0)}]
                       [Any * -> Any])]
              [[Any -> Any] [Any -> Any] * -> [Any * -> Any]]))


    clojure.core/map
    (All [c a b ...]
         (IFn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptyASeq c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (ASeq c)]))

    clojure.core/mapcat
    (All [c b ...]
         [[b ... b -> (Option (Seqable c))] (Option (Seqable b)) ... b -> (ASeq c)])

    clojure.core/pmap
    (All [c a b ...]
         (IFn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptyASeq c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (ASeq c)]))

    clojure.core/take (All [x]
                           [AnyInteger (Seqable x) -> (ASeq x)])
    clojure.core/drop (All [x]
                           [AnyInteger (Seqable x) -> (ASeq x)])
    clojure.core/drop-last (All [x]
                                [AnyInteger (Seqable x) -> (NilableNonEmptyASeq x)])
    clojure.core/take-last (All [x]
                                [AnyInteger (Seqable x) -> (NilableNonEmptyASeq x)])
    clojure.core/drop-while (All [x]
                                 [[x -> Any] (Option (Seqable x)) -> (ASeq x)])
    clojure.core/cycle (All [x]
                            [(U nil (Seqable x)) -> (ASeq x)])
    clojure.core/split-at
    (All [x y z]
         [AnyInteger (Option (Seqable x)) -> '[(ASeq x) (ASeq x)]])
    clojure.core/repeat (All [x]
                             (IFn [x -> (ASeq x)]
                                  [AnyInteger x -> (ASeq x)]))
    clojure.core/repeatedly (All [x]
                                 (IFn [[-> x] -> (ASeq x)]
                                      [AnyInteger [-> x] -> (ASeq x)]))
    clojure.core/iterate (All [x]
                              [[x -> x] x -> (ASeq x)])
    clojure.core/interleave (All [x] [(Option (Seqable x)) (Option (Seqable x)) (Option (Seqable x)) * -> (ASeq x)])
    clojure.core/interpose (All [x] (IFn [x (Option (Seqable x)) -> (ASeq x)]))
    clojure.core/filter
    (All [x y]
         (IFn
          [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (ASeq y)]
          [[x -> Any :filters {:then (! y 0)}] (Option (Seqable x)) -> (ASeq (I x (Not y)))]
          [[x -> Any] (Option (Seqable x)) -> (ASeq x)]))
    clojure.core/remove
    (All [x y]
         (IFn
          [[x -> Any :filters {:else (is y 0)}] (Option (Seqable x)) -> (ASeq y)]
          [[x -> Any :filters {:else (! y 0)}] (Option (Seqable x)) -> (ASeq (I x (Not y)))]
          [[x -> Any] (Option (Seqable x)) -> (ASeq x)]))
    clojure.core/flatten [Any -> Any]
    clojure.core/into
    (All [x y]
         (IFn [(Map x y) (U nil (Seqable (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]))) -> (Map x y)]
              [(Vec x) (U nil (Seqable x)) -> (Vec x)]
              [(Set x) (U nil (Seqable x)) -> (Set x)]
              [(Coll Any) (U nil (Seqable Any)) -> (Coll Any)]))
    clojure.core/mapv
    (All [c a b ...]
         (IFn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptyAVec c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (AVec c)]))
    clojure.core/filterv
    (All [x y]
         (IFn
          [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (AVec y)]
          [[x -> Any] (Option (Seqable x)) -> (AVec x)]))
    clojure.core/get-in
    (IFn [Any (U nil (Seqable Any)) -> Any]
         [Any (U nil (Seqable Any)) Any -> Any])
    clojure.core/assoc-in
    [(U nil (Associative Any Any Any)) (Seqable Any) Any -> Any]
    clojure.core/vec (All [x] [(Option (Seqable x)) -> (AVec x)])
    clojure.core/vector (All [r b ...]
                             (IFn [b ... b -> '[b ... b]]
                                  [r * -> (AVec r)]))
    clojure.core/subvec (All [x]
                             (IFn [(Vec x) AnyInteger -> (Vec x)]
                                  [(Vec x) AnyInteger AnyInteger -> (Vec x)]))
    clojure.core/keys
    (All [k]
         [(Map k Any) -> (ASeq k) :object {:id 0 :path [Keys]}])
    clojure.core/key (All [x]
                          [(IMapEntry x Any) -> x])
    clojure.core/vals
    (All [v]
         [(Map Any v) -> (ASeq v) :object {:id 0 :path [Vals]}])
    clojure.core/val (All [x]
                          [(IMapEntry Any x) -> x])
    clojure.core/merge
    (All [k v]
         (IFn [nil * -> nil]
              [(IPersistentMap k v) (IPersistentMap k v) * -> (IPersistentMap k v)]
              [(Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))]))
    clojure.core/merge-with
    (All [k v]
         (IFn [[v v -> v] nil * -> nil]
              [[v v -> v] (Map k v) * -> (Map k v)]
              [[v v -> v] (Option (Map k v)) * -> (Option (Map k v))]))
    clojure.core/select-keys (All [k v] [(Map k v) (U nil (Seqable Any))
                                         -> (Map k v)])
                                        ;clojure.core/set (All [x] [(Option (Seqable x)) -> (PersistentHashSet x)])
                                        ;clojure.core/hash-set (All [x] [x * -> (PersistentHashSet x)])
                                        ;clojure.core/sorted-set (All [x] [x * -> (PersistentTreeSet x)])
                                        ;clojure.core/sorted-set-by (All [x] [[x x -> AnyInteger] x * -> (PersistentTreeSet x)])
    clojure.core/distinct (All [x] [(U nil (Seqable x)) -> (ASeq x)])
    clojure.core/butlast (All [x]
                              [(Option (Seqable x)) -> (ASeq x)])
                                        ;clojure.core/name [(U String Named) -> String]
    clojure.core/zipmap
    (All [k v]
         [(U nil (Seqable k)) (U nil (Seqable v)) -> (APersistentMap k v)])
    clojure.core/max-key (All [x]
                              [[x -> Number] x x x * -> x])
    clojure.core/min-key (All [x]
                              [[x -> Number] x x x * -> x])
    clojure.core/partition-all
    (All [x]
         (IFn [Int (Nilable (Seqable x)) -> (ASeq (ASeq x))]
              [Int Int (Nilable (Seqable x)) -> (ASeq (ASeq x))]))
    clojure.core/take-while
    (All [x y]
         (IFn
          [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (ASeq y)]
          [[x -> Any] (Option (Seqable x)) -> (ASeq x)]))
    clojure.core/range
    (IFn [-> (ASeq AnyInteger)]
         [Number -> (ASeq AnyInteger)]
         [AnyInteger Number -> (ASeq AnyInteger)]
         [Number Number -> (ASeq Number)]
         [AnyInteger Number AnyInteger -> (ASeq AnyInteger)]
         [Number Number Number -> (ASeq Number)])
    clojure.core/take-nth (All [x] [AnyInteger (U nil (Seqable x)) -> (ASeq x)])
    clojure.core/split-with
    (All [x y z]
         (IFn
          [[x -> Any :filters {:then (is y 0), :else (is z 0)}] (Option (Seqable x)) -> '[(ASeq y) (ASeq z)]]
          [[x -> Any] (Option (Seqable x)) -> '[(ASeq x) (ASeq x)]]))
    clojure.core/dorun (IFn [(U nil (Seqable Any)) -> nil]
                            [AnyInteger (U nil (Seqable Any)) -> nil])
    clojure.core/doall (All [[c :< (U nil (Seqable Any))]]
                            (IFn [c -> c]
                                 [AnyInteger c -> c]))
    clojure.core/newline [-> nil]
    clojure.core/prn-str [Any * -> String]
    clojure.core/pr-str [Any * -> String]
    clojure.core/pr [Any * -> nil]
    clojure.core/print [Any * -> nil]
    clojure.core/println [Any * -> nil]
    clojure.core/print-str [Any * -> String]
    clojure.core/println-str [Any * -> String]
    clojure.core/prn [Any * -> nil]
    clojure.core/atom
    (All [x]
         [x & :optional {:validator (U nil [x -> Any]) :meta Any} -> (Atom2 x x)])
    clojure.core/reset! (All [w r]
                             [(Atom2 w r) w -> w])
                                        ;clojure.core/deref
    #_(All [x y]
           (IFn
            [(Deref x) -> x]
            [(U (Deref Any) java.util.concurrent.Future) -> Any]
            [(BlockingDeref x) AnyInteger y -> (U x y)]
            [(U java.util.concurrent.Future (BlockingDeref Any)) AnyInteger Any -> Any]))
    clojure.core/swap! (All [w r b ...]
                            [(Atom2 w r) [r b ... b -> w] b ... b -> w])
    clojure.core/compare-and-set!
    (All [w]
         [(Atom2 w Any) Any w -> Boolean])

                                        ;clojure.core/set-validator!
    #_(All [w]
           [(clojure.lang.IRef w Any) (U nil [w -> Any]) -> Any])
                                        ;clojure.core/get-validator
    #_(All [w]
           [(clojure.lang.IRef w Any) -> (U nil [w -> Any])])
                                        ;clojure.core/alter-meta!
    #_(All [b ...]
           [clojure.lang.IReference [(U nil (Map Any Any)) b ... b -> (U nil (Map Any Any))] b ... b -> (U nil (Map Any Any))])
                                        ;clojure.core/reset-meta! [clojure.lang.IReference (U nil (Map Any Any)) -> (U nil (Map Any Any))]
                                        ;clojure.core/add-watch
    #_(All [x [a :< (IRef Nothing x)]]
           (IFn
            ;; this arity remembers the type of reference we pass to the function
            [a Any [Any a x x -> Any] -> Any]
            ;; if the above cannot be inferred,
            [(IRef Nothing x) Any [Any (IRef Nothing x) x x -> Any] -> Any]))
                                        ;clojure.core/remove-watch [(IRef Nothing Any) Any -> Any]
    clojure.core/gensym (IFn [-> Symbol]
                             [(U Symbol String) -> Symbol])
    clojure.core/delay? (Pred (Delay Any))
    clojure.core/force (All [x]
                            (IFn [(Delay x) -> x]
                                 [Any -> Any]))
    ;clojure.core/realized? [clojure.lang.IPending -> Boolean]
    clojure.core/memoize (All [x y ...]
                              [[y ... y -> x] -> [y ... y -> x]])
    clojure.core/trampoline
    (All [r b ...]
         [[b ... b -> (Rec [f] (U r [-> (U f r)]))]
          b ... b -> r])
                                        ;clojure.core/make-hierarchy [-> Hierarchy]
    clojure.core/isa? (IFn [Any Any -> Boolean]
                                        ;[Hierarchy Any Any -> Boolean]
                           )
                                        ;clojure.core/derive
    #_(IFn [(U Symbol Keyword Class) (U Symbol Keyword) -> nil]
                                        ;[Hierarchy (U Symbol Keyword Class) (U Symbol Keyword) -> Hierarchy]
           )
                                        ;clojure.core/prefer-method [Multi Any Any -> Any]
                                        ;clojure.core/methods [Multi -> (Map Any Any)]
                                        ;clojure.core/ex-info
    #_(IFn [(U nil String) (Map Any Any) -> ExInfo]
           [(U nil String) (Map Any Any) (U nil Throwable) -> ExInfo])
                                        ;clojure.core/ex-data
    #_(IFn [ExInfo -> (Map Any Any)]
           [Any -> (U nil (Map Any Any))])
    clojure.core/special-symbol? [Any -> Boolean]



    ;;;;;;;; add-hook annotations just to improve coverage. correctness isn't assured
    clojure.core/reductions (All [a b] (IFn [[a b -> a] (Seqable b) -> (ASeq a)]
                                            [[a b -> a] a (Seqable b) -> (ASeq a)]))
    clojure.core/reduced? [Any -> Boolean]
    clojure.core/sequence (All [a] [(U nil (Seqable a)) -> (ASeq a)])
    clojure.core/dec [Number -> Number]
    clojure.core/inc [Number -> Number]
    clojure.core/set (All [a] [(Coll a) -> (Set a)])
    clojure.core/nfirst (All [a b c] [(Seqable (Seqable a)) -> (ASeq a)])
    clojure.core/keep (All [a b] [[a -> (Option b)] (Coll a) -> (Option (ASeq b))])
    clojure.core/seqable? (Pred (Seqable Any))
    clojure.core/sort-by (All [a] (IFn [(Coll a) -> (ASeq a)]
                                       [[a -> Number] (Coll a) -> (ASeq a)]))
    clojure.core/replicate (All [a] [Number a -> (ASeq a)])
    clojure.core/quot [Number Number -> Number]
    clojure.core/partition (All [a] (IFn [Number (Coll a) -> (ASeq (ASeq a))]
                                         [Number Number (Coll a) -> (ASeq (ASeq a))]
                                         [Number Number Number (Coll a) -> (ASeq (ASeq a))]))
    clojure.core/name [(U Keyword String Symbol) -> String]
    ;todo clojure.core/replace
    clojure.core/fnext (All [a] [(Seqable a) -> a])
    clojure.core/rem [Number Number -> Number]
    clojure.core/frequencies (All [a] [(Seqable a) -> (Map a Int)])")

(def common-var-annotations
  (delay
    (let [r (rdrs/string-push-back-reader common-ann*)
          eof (Object.)
          os (loop [os []]
               (let [a (rdr/read r false eof)]
                 (if (identical? eof a)
                   os
                   (recur (conj os a)))))
          _ (assert (even? (count os)))]
      (apply hash-map os))))
