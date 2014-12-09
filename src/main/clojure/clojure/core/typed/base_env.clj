(ns clojure.core.typed.base-env
  (:import (clojure.lang Named IMapEntry AMapEntry Seqable
                         LazySeq PersistentHashSet PersistentTreeSet PersistentList
                         IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap ISeq IPersistentCollection
                         ILookup Indexed Associative
                         IRef Reduced)
           (java.util Comparator Collection))
  (:require [clojure.core.typed.base-env-helper :as h]
            [clojure.core.typed.base-env-clj-rclass :as base-rclass]
            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.fold-default]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.subst]
            [clojure.core.typed.current-impl :as impl :refer [v]]
            [clojure.core.typed :refer [Any Nothing TFn Rec
                                        Pred U I All IFn
                                        HVec HSequential Keyword]
             :as t]))

(defn- aset-*-type [t]
  (impl/with-clojure-impl
    (let [arr-t (prs/parse-type `(~'Array ~t))
          rtn-type (prs/parse-type t)
          num-t (prs/parse-type 'Number)]
      (apply r/make-FnIntersection
             (map r/make-Function
                  (loop [num 1
                         result []
                         dom [arr-t num-t]]
                    (if (> num 10)
                      result
                      (recur (inc num)
                             (conj result (conj dom rtn-type))
                             (conj dom num-t))))
                  (repeat rtn-type)
                  (repeat nil)
                  (repeat nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial type aliases

(defmacro insert-aliases []
  `(delay-and-cache-env ~'init-alias-env
    (base-rclass/reset-rclass-env!)
    (h/alias-mappings
      ~@impl/init-aliases)))

;defines init-alias-env
(insert-aliases)

(defn reset-alias-env! []
  (let [alias-env (init-alias-env)]
    (nme-env/reset-name-env! alias-env)))

(delay-and-cache-env ^:private init-protocol-env 
                     {}
   #_(protocol-mappings
clojure.java.io/IOFactory 
     [[]
      :methods
      {
       make-reader
       [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedReader]

       make-writer 
       [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedWriter]

       make-input-stream 
       [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedInputStream]

       make-output-stream
       [clojure.java.io/IOFactory '{:append Any, :encoding (U nil String)} -> java.io.BufferedOutputStream]
       }]

     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

;;for parsing init-var-env
; must be after init-alias-env def as vars are interned there
(let [interns '[Option AnyInteger Id Coll Seq EmptySeqable
                NonEmptySeqable Map EmptyCount NonEmptyCount SortedSet Set
                Vec NonEmptyColl NonEmptyLazySeq NilableNonEmptyASeq
                Hierarchy Nilable Int Var1 Var2 Future Promise Agent1 Agent2
                Symbol Namespace Atom2 Ref1 Ref2 Delay Proxy List Stack ExInfo
                Multi Deref BlockingDeref SequentialSeqable ASeq NonEmptyASeq
                AVec NonEmptyAVec Sequential]]
  (when (some resolve interns)
    (doseq [i interns]
      (ns-unmap *ns* i)))
  (refer 'clojure.core.typed :only interns))

(defn ^:private count-type []
  (impl/with-clojure-impl
    (r/make-FnIntersection
      (r/make-Function 
        [(prs/parse-type '(U nil (clojure.core.typed/Seqable Any) clojure.lang.Counted))]
        (prs/parse-type '(U java.lang.Integer java.lang.Long))
        nil nil
        :object (obj/-path [(pe/CountPE-maker)] 0)))))

(defn ^:private nth-type []
  (impl/with-clojure-impl
    (prs/parse-type
      '(All [x y]
            (IFn 
              [(U (Indexed x) (SequentialSeqable x)) AnyInteger -> x]
              [(U (Indexed x) (SequentialSeqable x) nil) AnyInteger y -> (U x y)]
              [(U (Indexed x) (SequentialSeqable x) nil) AnyInteger -> (U x nil)])))))

(delay-and-cache-env ^:private init-var-env
  (reset-alias-env!)
  (merge
   (common/parse-clj-ann-map common/common-var-annotations)
   (h/var-mappings

clojure.core.typed/check-ns (IFn [Symbol -> Any]
                                [-> Any])
;; Internal annotations

clojure.core.typed.current-impl/*current-impl* Any
clojure.core.typed.current-impl/clojure Any
clojure.core.typed.current-impl/clojurescript Any
clojure.core.typed/ann* [Any Any Any -> Any]
clojure.core.typed/def-alias* [Any Any -> Any]
clojure.core.typed/declare-names* [Any -> Any]
clojure.core.typed/typed-deps* [Any -> Any]
clojure.core.typed/warn-on-unannotated-vars* [-> Any]
clojure.core.typed/ann-datatype* [Any Any Any Any -> Any]
clojure.core.typed/ann-protocol* [Any Any Any -> Any]
      ; REMOVED
clojure.core.typed/ann-pprotocol* [Any Any Any -> Any]
clojure.core.typed/ann-record* [Any Any Any Any -> Any]
clojure.core.typed/ann-pdatatype* [Any Any Any Any -> Any]
clojure.core.typed/ann-precord* [Any Any Any Any -> Any]
clojure.core.typed/declare-datatypes* [Any -> Any]
clojure.core.typed/declare-protocols* [Any -> Any]
clojure.core.typed/non-nil-return* [Any Any -> Any]
clojure.core.typed/nilable-param* [Any Any -> Any]
clojure.core.typed/override-constructor* [Any Any -> Any]
clojure.core.typed/override-method* [Any Any -> Any]
clojure.core.typed/typed-deps* [Any -> Any]
clojure.core.typed/load-if-needed [-> Any]
clojure.core.typed/*collect-on-eval* Any
; should always be special cased
;clojure.core.typed/var>* [Any -> (Var2 Nothing Any)]

;; core annotations

clojure.core/*ns* Namespace
clojure.core/pop-thread-bindings [-> Any]
clojure.core/load [String * -> Any]
clojure.core/read-string [String -> Any]
clojure.core/read (IFn [-> Any]
                      [java.io.Reader -> Any]
                      [java.io.Reader Boolean Any -> Any]
                      [java.io.Reader Boolean Any Boolean -> Any])
clojure.core/read-line [-> (U nil String)]

clojure.core/add-classpath [(U String java.net.URL) -> nil]

clojure.core/*1 Any
clojure.core/*2 Any
clojure.core/*3 Any
clojure.core/*e Throwable
clojure.core/*agent* (U nil (Agent2 Nothing Any))
clojure.core/*allow-unresolved-vars* Any
clojure.core/*assert* Any
clojure.core/*data-readers* (Map Symbol (Var2 Nothing Any))
clojure.core/*default-data-reader-fn* (U nil [Any Any -> Any])
clojure.core/*fn-loader* Any
clojure.core/*math-context* Any
clojure.core/*source-path* String
clojure.core/*use-context-classloader* Any

clojure.core/alength [(ReadOnlyArray Any) -> AnyInteger]
clojure.core/aclone (All [x] [(ReadOnlyArray x) -> (Array x)])
clojure.core/aget (All [x] (IFn [(ReadOnlyArray x) 
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

clojure.core/macroexpand-1 [Any -> Any]
clojure.core/macroexpand [Any -> Any]

clojure.core/create-struct [Any * -> (Map Any Any)]

clojure.core/find-ns [Symbol -> Namespace]
clojure.core/create-ns [Symbol -> Namespace]
clojure.core/remove-ns [Symbol -> Namespace]

clojure.core/namespace [(U Symbol String Keyword) -> (Option String)]
clojure.core/ns-name [(U Symbol Namespace) -> Symbol]
clojure.core/ns-map [(U Symbol Namespace) -> Symbol]
clojure.core/ns-aliases [(U Symbol Namespace) -> (Map Symbol Namespace)]
clojure.core/name [(U String Named) -> String]
clojure.core/the-ns [(U Symbol Namespace) -> Namespace]
clojure.core/in-ns [Symbol -> nil]
clojure.core/import [Any * -> nil]
clojure.core/identity (All [x] [x -> x
                                :filters {:then (! (U nil false) 0)
                                          :else (is (U nil false) 0)}
                                :object {:id 0}])
clojure.core/gensym (IFn [-> Symbol]
                        [(U Symbol String) -> Symbol])
clojure.core/intern (IFn [(U Symbol Namespace) Symbol -> (Var2 Nothing Any)]
                        [(U Symbol Namespace) Symbol Any -> (Var2 Nothing Any)])


clojure.core/doall (All [[c :< (U nil (Seqable Any))]]
                     (IFn [c -> c]
                         [AnyInteger c -> c]))
clojure.core/dorun (IFn [(U nil (Seqable Any)) -> nil]
                       [AnyInteger (U nil (Seqable Any)) -> nil])
clojure.core/iterate (All [x]
                       [[x -> x] x -> (ASeq x)])
clojure.core/memoize (All [x y ...]
                            [[y ... y -> x] -> [y ... y -> x]])

clojure.core/key (All [x]
                           [(IMapEntry x Any) -> x])
clojure.core/val (All [x]
                           [(IMapEntry Any x) -> x])

;clojure.core/juxt
;(All [b1 ...]
;(All [x r b2 ...]
;     (Fn [[b1 ... b1 -> b2] ... b2 -> [b1 ... b1 -> '[b2 ... b2]]]
;         [[b1 ... b1 -> r] * -> [b1 ... b1 -> (Vec r)]]
;         [[x * -> b2] ... b2 -> [x * -> '[b2 ... b2]]]
;         [[x * -> r] * -> [x * -> (Vec r)]])))


;TODO flip filters
clojure.core/complement (All [x] [[x -> Any] -> [x -> Boolean]])
; should preserve filters
clojure.core/boolean [Any -> Boolean]

clojure.core/filter (All [x y]
                           (IFn
                             [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (ASeq y)]
                             [[x -> Any :filters {:then (! y 0)}] (Option (Seqable x)) -> (ASeq (I x (Not y)))]
                             [[x -> Any] (Option (Seqable x)) -> (ASeq x)]))
clojure.core/filterv (All [x y]
                          (IFn
                            [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (AVec y)]
                            [[x -> Any] (Option (Seqable x)) -> (AVec x)]))
clojure.core/remove (All [x y]
                           (IFn
                             [[x -> Any :filters {:else (is y 0)}] (Option (Seqable x)) -> (ASeq y)]
                             [[x -> Any :filters {:else (! y 0)}] (Option (Seqable x)) -> (ASeq (I x (Not y)))]
                             [[x -> Any] (Option (Seqable x)) -> (ASeq x)]
                             ))


clojure.core/take-while (All [x y]
                               (IFn 
                                 [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (ASeq y)]
                                 [[x -> Any] (Option (Seqable x)) -> (ASeq x)]))
clojure.core/drop-while (All [x]
                               [[x -> Any] (Option (Seqable x)) -> (ASeq x)])

clojure.core/split-with
     (All [x y z] 
       (IFn
         [[x -> Any :filters {:then (is y 0), :else (is z 0)}] (Option (Seqable x)) -> '[(ASeq y) (ASeq z)]]
         [[x -> Any] (Option (Seqable x)) -> '[(ASeq x) (ASeq x)]]))

clojure.core/split-at
     (All [x y z] 
          [AnyInteger (Option (Seqable x)) -> '[(ASeq x) (ASeq x)]])

clojure.core/partition-all (All [x] 
                             (IFn [Int (Nilable (Seqable x)) -> (ASeq (ASeq x))] 
                                 [Int Int (Nilable (Seqable x)) -> (ASeq (ASeq x))]))

clojure.core/repeatedly
     (All [x]
          (IFn [[-> x] -> (ASeq x)]
              [AnyInteger [-> x] -> (ASeq x)]))


clojure.core/some (All [x y] [[x -> y] (Option (Seqable x)) -> (Option y)])

; Unions need to support dots for this to work:
;
; (All [t0 b ...]
;    (IFn [[Any -> Any :filters {:then (is t0 0) :else (! t0 0)}] 
;         [Any -> Any :filters {:then (is b 0) :else (! b 0)}] ... b
;         -> (IFn [Any -> Any :filters {:then (is (U t0 b ... b) 0) :else (! (U t0 b ... b) 0)}]
;                [Any * -> Any])]))
clojure.core/some-fn 
  (All [t0 t1 t2 t3 t4 t5]
    (IFn [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         -> (IFn [Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1) 0) :else (! (U t0 t1) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2) 0) :else (! (U t0 t1 t2) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2 t3) 0) :else (! (U t0 t1 t2 t3) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         [Any -> Boolean :filters {:then (is t4 0) :else (! t4 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2 t3 t4) 0) :else (! (U t0 t1 t2 t3 t4) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         [Any -> Boolean :filters {:then (is t4 0) :else (! t4 0)}]
         [Any -> Boolean :filters {:then (is t5 0) :else (! t5 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2 t3 t4 t5) 0) :else (! (U t0 t1 t2 t3 t4 t5) 0)}]
                [Any * -> Any])]
        [[Any -> Any] [Any -> Any] * -> [Any * -> Any]]))
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

clojure.core/concat (All [x] [(Option (Seqable x)) * -> (ASeq x)])

clojure.core/set (All [x] [(Option (Seqable x)) -> (PersistentHashSet x)])
clojure.core/hash-set (All [x] [x * -> (PersistentHashSet x)])
clojure.core/sorted-set (All [x] [x * -> (PersistentTreeSet x)])
clojure.core/sorted-set-by (All [x] [[x x -> AnyInteger] x * -> (PersistentTreeSet x)])
clojure.core/list (All [x] [x * -> (PersistentList x)])
clojure.core/list* (All [x] 
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

clojure.core/list? (Pred (List Any))

clojure.core/load-reader [java.io.Reader -> Any]

clojure.core/methods [Multi -> (Map Any Any)]

clojure.core/munge (IFn [Symbol -> Symbol]
                       [Any -> Any])

clojure.core/pos? (IFn [Number -> Boolean])
clojure.core/neg? (IFn [Number -> Boolean])

clojure.core/nthrest (All [x] [(U nil (Seqable x)) AnyInteger 
                               -> (ASeq x)])

clojure.core/vector (All [r b ...]
                         (IFn [b ... b -> '[b ... b]]
                             [r * -> (AVec r)]))
clojure.core/vec (All [x] [(Option (Seqable x)) -> (AVec x)])

clojure.core/not [Any -> Boolean]
clojure.core/constantly (All [x] [x -> [Any * -> x]])

clojure.core/bound? [(Var2 Nothing Any) * -> Boolean]
clojure.core/thread-bound? [(Var2 Nothing Any) * -> Boolean]
clojure.core/bases [(Nilable Class) -> (NilableNonEmptyASeq Class)]

clojure.core/make-hierarchy [-> Hierarchy]
clojure.core/isa? (IFn [Any Any -> Boolean]
                      [Hierarchy Any Any -> Boolean])

clojure.core/disj
     (All [x]
          (IFn [(SortedSet x) Any Any * -> (SortedSet x)]
              [(Set x) Any Any * -> (Set x)]))

clojure.core/assoc
     (All [b c d]
       (IFn [(Map b c) b c -> (Map b c)]
           [(Vec d) AnyInteger d -> (Vec d)]))

clojure.core/dissoc
     (All [k v]
       (IFn [(Map k v) Any * -> (Map k v)]))
)
    (h/var-mappings

clojure.core/zipmap
     (All [k v]
       [(U nil (Seqable k)) (U nil (Seqable v)) -> (APersistentMap k v)])

clojure.core/keys
(All [k]
     [(Map k Any) -> (ASeq k) :object {:id 0 :path [Keys]}])

clojure.core/vals
(All [v]
     [(Map Any v) -> (ASeq v) :object {:id 0 :path [Vals]}])

;most useful case
clojure.core/comp
     (All [x y b ...]
          [[x -> y] [b ... b -> x] -> [b ... b -> y]])


;apply: wishful thinking
;     (All [b1 ...]
;     (All [y b2 ...]
;          (IFn [[b1 ... b1 b2 ... b2 -> y] b1 ... b1 (HSequential [b2 ... b2]) -> y]
;              [[b1 ... b1 r * -> y] b1 ... b1 (U nil (Seqable r)) -> y])))

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

;partial: wishful thinking (replaces the first 4 arities)
; (All [b1 ...]
; (All [r b2 ...]
;    [[b1 ... b1 b2 ... b2 -> r] b1 ... b1 -> [b2 ... b2 -> r]]))

clojure.core/partial
     (All [y a b c d z ...]
          (IFn [[z ... z -> y] -> [z ... z -> y]]
              [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a * -> y] a * -> [a * -> y]]))

clojure.core/str [Any * -> String]
clojure.core/prn-str [Any * -> String]
clojure.core/pr-str [Any * -> String]
clojure.core/newline [-> nil]

clojure.core/print [Any * -> nil]
clojure.core/println [Any * -> nil]
clojure.core/print-str [Any * -> String]
clojure.core/println-str [Any * -> String]
clojure.core/printf [String Any * -> nil]
clojure.core/format [String Any  * -> String]
clojure.core/pr [Any * -> nil]
clojure.core/prn [Any * -> nil]
clojure.core/flush [-> nil]
clojure.core/*print-length* (U nil false AnyInteger)
clojure.core/*print-level* (U nil false AnyInteger)
clojure.core/*verbose-defrecords* Boolean
clojure.core/print-ctor [Object [Object java.io.Writer -> Any] java.io.Writer -> nil]

clojure.core/prefer-method [Multi Any Any -> Any]
clojure.core/print-simple [Any java.io.Writer -> nil]
clojure.core/char-escape-string (Map Character String)
clojure.core/char-name-string (Map Character String)
clojure.core/primitives-classnames (Map Class String)

clojure.core/namespace-munge [(U Symbol Namespace) -> String]

;clojure.core/find-protocol-impl ['{:on-interface Class
;                                   :impls ?}]


clojure.core/re-matcher [java.util.regex.Pattern String -> java.util.regex.Matcher]
clojure.core/re-groups [java.util.regex.Matcher -> (U nil String (Vec (Option String)))]
clojure.core/re-find (IFn [java.util.regex.Matcher -> (U nil String (Vec (Option String)))]
                              [java.util.regex.Pattern String -> (U nil String (Vec (Option String)))])
clojure.core/re-seq [java.util.regex.Pattern String -> (ASeq (U nil String (Vec (Option String))))]

clojure.core/subs (IFn [String AnyInteger -> String]
                           [String AnyInteger AnyInteger -> String])

;TODO
;clojure.core/spit [java.io.Writer Any]

clojure.core/future-call (All [x] [[-> x] -> (Future x)])

clojure.core/atom (All [x]
                       [x & :optional {:validator (U nil [x -> Any]) :meta Any} -> (Atom2 x x)])

clojure.core/set-validator! (All [x]
                                 [(clojure.lang.IRef Any x) [x -> Any] -> nil])

clojure.core/deref (All [x y]
                     (IFn 
                         [(Deref x) -> x]
                         [(U (Deref Any) java.util.concurrent.Future) -> Any]
                         [(BlockingDeref x) AnyInteger y -> (U x y)]
                         [(U java.util.concurrent.Future (BlockingDeref Any)) AnyInteger Any -> Any]))

clojure.core/delay? (Pred (Delay Any))

clojure.core/future-cancelled? [java.util.concurrent.Future -> Boolean]

clojure.core/future-cancel [java.util.concurrent.Future -> Any]

clojure.core/future? (Pred java.util.concurrent.Future)

clojure.core/future-done? [java.util.concurrent.Future -> Boolean]

clojure.core/force (All [x]
                        (IFn [(Delay x) -> x]
                            [Any -> Any]))

clojure.core/realized? [clojure.lang.IPending -> Boolean]

clojure.core/select-keys (All [k v] [(Map k v) (U nil (Seqable Any))
                                     -> (Map k v)])

; could possibly return nil in some insane mutable situtation
clojure.core/sort (All [x] 
                       (IFn [(U nil (Seqable x)) -> (U nil (ASeq x))]
                           [(I Comparator [x x -> AnyInteger]) 
                            (U nil (Seqable x)) -> (U nil (ASeq x))]))

; this is insane
;clojure.core/test

clojure.core/reset! (All [w r]
                              [(Atom2 w r) w -> w])

clojure.core/swap! (All [w r b ...] 
                             [(Atom2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/compare-and-set!
                   (All [w]
                     [(Atom2 w Any) Any w -> Boolean])

clojure.core/set-validator!
                   (All [w]
                     [(clojure.lang.IRef w Any) (U nil [w -> Any]) -> Any])

clojure.core/get-validator
                   (All [w]
                     [(clojure.lang.IRef w Any) -> (U nil [w -> Any])])

clojure.core/alter-var-root (All [w r b ...] 
                              [(Var2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/method-sig [java.lang.reflect.Method -> '[Any (U nil (NonEmptySeqable Any)) Any]]
clojure.core/proxy-name [Class (U nil (Seqable Class)) -> String]
clojure.core/get-proxy-class [Class * -> Class]
clojure.core/construct-proxy [Class Any * -> Any]
clojure.core/init-proxy [Proxy (Map String Any) -> Proxy]
clojure.core/update-proxy [Proxy (Map String Any) -> Proxy]
clojure.core/proxy-mappings [Proxy -> (Map String Any)]
clojure.core/proxy-call-with-super (All [x] [[-> x] Proxy String -> x])
clojure.core/bean [Object -> (Map Any Any)]

clojure.core/fnil (All [x y z a b ...]
                    (IFn [[x b ... b -> a] x -> [(U nil x) b ... b -> a]]
                        [[x y b ... b -> a] x y -> [(U nil x) (U nil y) b ... b -> a]]
                        [[x y z b ... b -> a] x y z -> [(U nil x) (U nil y) (U nil z) b ... b -> a]]))

clojure.core/symbol
     (IFn [(U Symbol String) -> Symbol]
         [(U nil String) String -> Symbol])

clojure.core/keyword
     (IFn [(U Keyword Symbol String) -> Keyword]
         [String String -> Keyword])

clojure.core/find-keyword
     (IFn [(U Keyword Symbol String) -> (Option Keyword)]
         [String String -> (Option Keyword)])

clojure.core/derive (IFn [(U Symbol Keyword Class) (U Symbol Keyword) -> nil]
                        [Hierarchy (U Symbol Keyword Class) (U Symbol Keyword) -> Hierarchy])

clojure.core/compare [Any Any -> Number]

clojure.core/require [Any * -> nil]
clojure.core/use [Any * -> nil]
clojure.core/refer [Symbol & :optional {:exclude (Seqable Symbol)
                                        :only (Seqable Symbol)
                                        :rename (Map Symbol Symbol)}
                    -> nil]

clojure.core/*loaded-libs* (Ref1 (Set Symbol))

clojure.core/seq? (Pred (Seq Any))
clojure.core/set? (Pred (Set Any))
clojure.core/vector? (Pred (Vec Any))
clojure.core/nil? (Pred nil)
clojure.core/false? (Pred false)
clojure.core/true? (Pred true)
clojure.core/zero? (Pred (Value 0))
clojure.core/symbol? (Pred Symbol)
clojure.core/keyword? (Pred Keyword)
clojure.core/map? (Pred (Map Any Any))
)
    (h/var-mappings

clojure.core/cast (All [x] [Class x -> x])

clojure.core/associative? (Pred (clojure.lang.Associative Any Any))
clojure.core/coll? (Pred (Coll Any))
      ;TODO should these be parameterised?
clojure.core/sequential? (Pred Sequential)
;clojure.core/sorted? (Pred Sorted)
clojure.core/meta [Any -> (U nil (Map Any Any))]
clojure.core/with-meta (All [[x :< clojure.lang.IObj]]
                            [x (U nil (Map Any Any)) -> x])
clojure.core/vary-meta (All [[x :< clojure.lang.IObj] b ...]
                            [x [(U nil (Map Any Any)) b ... b -> (U nil (Map Any Any))] b ... b -> x])

clojure.core/reset-meta! [clojure.lang.IReference (U nil (Map Any Any)) -> (U nil (Map Any Any))]
clojure.core/alter-meta! 
      (All [b ...]
      [clojure.lang.IReference [(U nil (Map Any Any)) b ... b -> (U nil (Map Any Any))] b ... b -> (U nil (Map Any Any))])

clojure.core/commute
      (All [w r b ...] 
           [(Ref2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/alter
      (All [w r b ...] 
           [(Ref2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/cycle
      (All [x]
           [(U nil (Seqable x)) -> (ASeq x)])

clojure.core/compile [Symbol -> Symbol]

clojure.core/comparator
      (All [x y]
           [[x y -> Any] -> (I Comparator [x y -> AnyInteger])])

clojure.core/destructure [Any -> Any]

clojure.core/distinct (All [x] [(U nil (Seqable x)) -> (ASeq x)])

clojure.core/string? (Pred String)
clojure.core/char? (Pred Character)

clojure.string/split
     (IFn [String java.util.regex.Pattern -> (AVec String)]
         [String java.util.regex.Pattern AnyInteger -> (AVec String)])

clojure.string/join
     (IFn [(Option (Seqable Any)) -> String]
         [Any (Option (Seqable Any)) -> String])

clojure.string/upper-case
      [CharSequence -> String]

clojure.string/blank? [(U nil String) -> Boolean]
clojure.string/capitalize [String -> String]
clojure.string/lower-case [String -> String]
clojure.string/replace (IFn [String String String -> String]  [String Character Character -> String]  [String java.util.regex.Pattern (U String [String -> String]) -> String] )
clojure.string/replace-first (IFn [String String String -> String]  [String Character Character -> String]  [String java.util.regex.Pattern (U String [String -> String]) -> String] )
clojure.string/reverse [String -> String]
clojure.string/trim [String -> String]
clojure.string/trimr [String -> String]
clojure.string/triml [String -> String]



clojure.core/interpose (All [x] (IFn [x (Option (Seqable x)) -> (ASeq x)]))
clojure.core/interleave (All [x] [(Option (Seqable x)) (Option (Seqable x)) (Option (Seqable x)) * -> (ASeq x)])

clojure.core/repeat (All [x] 
                         (IFn [x -> (ASeq x)]
                             [AnyInteger x -> (ASeq x)]))

;clojure.core/every? (All [x y] 
;                         (IFn [[x -> Any :filters {:then (is y 0)}] (Coll x) -> Boolean
;                              :filters {:then (is (Coll (I x y)) 1)}]
;                             ; argument could be nil
;                             [[x -> Any :filters {:then (is y 0)}] (U nil (Coll x)) -> Boolean
;                              :filters {:then (is (U nil (Coll (I x y))) 1)}]
;                             [[x -> Any] (U nil (Seqable x)) -> Boolean]))
clojure.core/every? (All [x y]
                         (IFn [[x -> Any :filters {:then (is y 0)}] (Coll x) -> Boolean
                              :filters {:then (is (Coll y) 1)}]
                             ; argument could be nil
                             [[x -> Any :filters {:then (is y 0)}] (U nil (Coll x)) -> Boolean
                              :filters {:then (is (U nil (Coll y)) 1)}]
                             [[x -> Any] (U nil (Seqable x)) -> Boolean]))

clojure.core/range
(IFn [-> (ASeq AnyInteger)]
    [Number -> (ASeq AnyInteger)]
    [AnyInteger Number -> (ASeq AnyInteger)]
    [Number Number -> (ASeq Number)]
    [AnyInteger Number AnyInteger -> (ASeq AnyInteger)]
    [Number Number Number -> (ASeq Number)])

clojure.core/class (IFn [nil -> nil :object {:id 0 :path [Class]}]
                            [Object -> Class :object {:id 0 :path [Class]}]
                            [Any -> (Option Class) :object {:id 0 :path [Class]}])

; need better metadata support if this even has a chance of working
; like class
clojure.core/type [Any -> Any]

clojure.core/seq (All [x]
                        (IFn 
                          [(NonEmptyColl x) -> (NonEmptyASeq x)]
                          [(Option (Coll x)) -> (Option (NonEmptyASeq x))
                           :filters {:then (& (is NonEmptyCount 0)
                                              (! nil 0))
                                     :else (| (is nil 0)
                                              (is EmptyCount 0))}]
                          [(Option (Seqable x)) -> (Option (NonEmptyASeq x))]))

; Seqable [[x :variance :covariant]
;          :count [l :variance :covariant :< AnyCountRange]
;          :to-seq [sfn :kind (TFn [[x :variance :covariant]]
;                               (I IWithMeta (IMeta nil) (ISeq x) (ICollection x) 
;                                  IEmptyableCollection ISequential))]]

; clojure.core/seq (All [x
;                        [sfn :kind [* -> *]]
;                    (IFn
;                      [(Seqable x :count (CountRange 1) :to-seq sfn) -> (sfn x)]
;                      [(Seqable x :count AnyCountRange :to-seq sfn) -> (U nil (sfn x))]))

clojure.core/empty? (IFn [(Option (HSequential [Any *])) -> Boolean
                          :filters {:then (| (is EmptyCount 0)
                                             (is nil 0))
                                    :else (is NonEmptyCount 0)}]
                        [(Option (Coll Any)) -> Boolean
                          :filters {:then (| (is EmptyCount 0)
                                             (is nil 0))
                                    :else (is NonEmptyCount 0)}]
                        [(Option (Seqable Any)) -> Boolean])

clojure.core/map
     (All [c a b ...]
          (IFn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptyASeq c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (ASeq c)]))

clojure.core/mapv
     (All [c a b ...]
          (IFn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptyAVec c)]
               [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (AVec c)]))

clojure.core/mapcat
     (All [c b ...]
          [[b ... b -> (Option (Seqable c))] (Option (Seqable b)) ... b -> (ASeq c)])

clojure.core/pmap
     (All [c a b ...]
          (IFn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptyASeq c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (ASeq c)]))

clojure.core/pcalls
      (All [r]
           [[-> r] * -> (ASeq r)])

clojure.core/*clojure-version* '{:major Any
                                 :minor Any
                                 :incremental Any
                                 :qualifier Any}

clojure.core/clojure-version [-> String]

clojure.core/promise
        (All [x]
           [-> (Promise x)])

clojure.core/deliver (All [x] [(Promise x) x -> (U nil (Promise x))])

clojure.core/flatten [Any -> Any]

;TODO review
;clojure.core/group-by (All [x y] [[x -> y] (U nil (Seqable x)) -> (Map y (Vec x))])

clojure.core/map-indexed
     (All [x y] [[AnyInteger x -> y] (Option (Seqable x)) -> (Seqable y)])

clojure.core/merge-with
     (All [k v]
          (IFn [[v v -> v] nil * -> nil]
              [[v v -> v] (Map k v) * -> (Map k v)]
              [[v v -> v] (Option (Map k v)) * -> (Option (Map k v))]))

clojure.core/reduce
     (All [a c]
          (IFn 
            ;Without accumulator
            ; default
            ; (reduce + my-coll)
            [[a c -> (U (Reduced a) a)] (NonEmptySeqable c) -> a]
            [(IFn [a c -> (U (Reduced a) a)] [-> (U (Reduced a) a)]) (Option (Seqable c)) -> a]
            ; default
            ; (reduce + 3 my-coll)
            [[a c -> (U (Reduced a) a)] a (Option (Seqable c)) -> a]))

clojure.core/reduce-kv
    (All [a c k v]
      [[a k v -> (U (Reduced a) a)] a (Option (Associative k v)) -> a])

clojure.core/reduced (All [x] [x -> (Reduced x)])
clojure.core/reduced? (Pred (Reduced Any))

#_(comment
  clojure.core/reduce
       (All [a c d]
            (IFn 
              ;Without accumulator
              ; empty coll, f takes no args
              ; (reduce + []) => 0, (reduce + nil) => 0
              [[-> c] (U nil (I (ExactCount 0) (Seqable c))) -> c]
              ; coll count = 1, f is not called
              ; (reduce + [1]) => 1
              [Any (I (ExactCount 1) (Seqable c)) -> c]
              ; coll count >= 2
              ; (reduce + [1 2]) => 3
              [[c c -> c] (I (CountRange 2) (Seqable c)) -> c]
              ; default
              ; (reduce + my-coll)
              [(IFn [c c -> c] [-> c]) (U nil (Seqable c)) -> c]
              ;With accumulator
              ; empty coll, f not called, returns accumulator
              ; (reduce + 3 []) => 3
              [Any a (U nil (I (ExactCount 0) (Seqable Any))) -> a]
              ; default
              ; (reduce + 3 my-coll)
              [[a c -> a] a (U nil (Seqable c)) -> a]))
  )

;should be special cased
clojure.core/not= [Any Any * -> Boolean]

clojure.core/first
     (All [x]
          (IFn [(HSequential [x Any *]) -> x
                :object {:id 0 :path [(Nth 0)]}]
               [(Option (EmptySeqable x)) -> nil]
               [(NonEmptySeqable x) -> x]
               [(Option (Seqable x)) -> (Option x)]))

clojure.core/second
     (All [x]
          (IFn [(HSequential [Any x Any *]) -> x
                :object {:id 0 :path [(Nth 1)]}]
               [(Option (I (Seqable x) (CountRange 0 1))) -> nil]
               [(I (Seqable x) (CountRange 2)) -> x]
               [(Option (Seqable x)) -> (Option x)]))

clojure.core/ffirst
     (All [x]
          [(Option (Seqable (U nil (Seqable x)))) -> (Option x)])

clojure.core/nfirst
(All [x]
     [(Option (Seqable (Option (Seqable x)))) -> (Option (NonEmptyASeq x))])

clojure.core/fnext
(All [x]
     [(Option (Seqable (Option (Seqable x)))) -> (Option x)])

clojure.core/nnext
(All [x]
     [(Option (Seqable x)) -> (Option (NonEmptyASeq x))])

clojure.core/nthnext
(All [x]
     (IFn [nil AnyInteger -> nil]
          [(Option (Seqable x)) AnyInteger -> (Option (NonEmptyASeq x))]))

clojure.core/rest
     (All [x]
          [(Option (Seqable x)) -> (ASeq x)])

clojure.core/last
     (All [x]
          (IFn [(NonEmptySeqable x) -> x]
               [(Option (Seqable x)) -> (U nil x)]))

clojure.core/butlast
     (All [x]
          [(Option (Seqable x)) -> (ASeq x)])

clojure.core/next
     (All [x]
          (IFn [(Option (Coll x)) -> (Option (NonEmptyASeq x))
               :filters {:then (& (is (CountRange 2) 0)
                                  (! nil 0))
                         :else (| (is (CountRange 0 1) 0)
                                  (is nil 0))}]
              [(Option (Seqable x)) -> (Option (NonEmptyASeq x))]))

clojure.core/into
      (All [x y]
           (IFn [(Map x y) (U nil (Seqable (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]))) -> (Map x y)]
               [(Vec x) (U nil (Seqable x)) -> (Vec x)]
               [(Set x) (U nil (Seqable x)) -> (Set x)]
               [(Coll Any) (U nil (Seqable Any)) -> (Coll Any)]))

clojure.core/conj
;     (All [e
;           [Arg :< (TFn [[x :variance :covariant]] Any)]
;           [Res :< (TFn [[x :variance :covariant]]
;                     (Coll Any))]]
;          (IFn [(clojure.lang.IPersistentCollection e Arg Res) (Arg e) (Arg e) * -> (Res e)]
;              [nil e e * -> (clojure.lang.PersistentList e)]))


     (All [x y]
          (IFn [(IPersistentVector x) x x * -> (IPersistentVector x)]
              [(APersistentMap x y)
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) *
               -> (APersistentMap x y)]
              [(IPersistentMap x y)
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) * -> (IPersistentMap x y)]
              [(IPersistentSet x) x x * -> (IPersistentSet x)]
              [(ASeq x) x x * -> (ASeq x)]
              [nil x x * -> (clojure.lang.PersistentList x)]
              [(Coll Any) Any Any * -> (Coll Any)]
              ))

; IPersistentCollection [[x :variance :covariant]
;                        :conj-fn [conj-fn :kind (TFn [[x :variance :covariant]] (IPersistentCollection x))]
;                        :empty-fn [empty-fn :kind (TFn [] (IPersistentCollection Nothing :count (ExactCount 0)))]]

; clojure.core/conj
;   (All [x conj-fn]
;     [(IPersistentCollection x :conj-fn conj-fn) x -> (conj-fn x)]
;     [nil x -> (PersistentList x)]
;     [(U nil (IPersistentCollection x :conj-fn conj-fn)) x -> (U nil (conj-fn x))])

; clojure.core/empty
;   (All [x empty-fn]
;      [(IPersistentCollection Any :empty-fn empty-fn) -> (empty-fn)]
;      [nil -> nil]
;      [(U nil (IPersistentCollection Any :empty-fn empty-fn)) -> (U nil (empty-fn))])

clojure.core/find
     (All [x y]
          [(U nil (Associative x y)) Any -> (U nil (HVec [x y]))])

; same as clojure.lang.RT/get
clojure.core/get
     (All [x y]
          (IFn 
            ;no default
            [(U nil (Set x) (ILookup Any x)) Any -> (Option x)]
            [(Option java.util.Map) Any -> Any]
            [(Option String) Any -> (Option Character)]
            ;default
            [(U nil (Set x) (ILookup Any x)) Any y -> (U y x)]
            [(Option java.util.Map) Any y -> (U y Any)]
            [(Option String) Any y -> (U y Character)]
            ))
)
    (h/var-mappings

clojure.core/get-in
    (IFn [Any (U nil (Seqable Any)) -> Any]
        [Any (U nil (Seqable Any)) Any -> Any])

clojure.core/assoc-in
    [(U nil (Associative Any Any)) (Seqable Any) Any -> Any]

;FIXME maps after the first can always be nil
clojure.core/merge 
     (All [k v]
          (IFn [nil * -> nil]
              [(IPersistentMap k v) (IPersistentMap k v) * -> (IPersistentMap k v)]
              [(Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))]))

;more to be said here?
clojure.core/contains? [(Option (Seqable Any)) Any -> Boolean]

clojure.core/= [Any Any * -> (U true false)]
clojure.core/identical? [Any Any -> Boolean]
clojure.core/distinct? [Any Any * -> Boolean]

clojure.core/decimal? (Pred BigDecimal)

clojure.core/denominator [clojure.lang.Ratio -> Number]

clojure.core/mod (IFn [AnyInteger AnyInteger -> AnyInteger]
                      [Number Number -> Number])

clojure.core/var-get (All [r] [(Var2 Nothing r) -> r])
clojure.core/var-set (All [w] [(Var2 w Any) w -> w])

clojure.core/supers [Class -> (U nil (I NonEmptyCount (Set Class)))]

clojure.core/take-nth (All [x] [AnyInteger (U nil (Seqable x)) -> (ASeq x)])

clojure.core/shuffle (All [x] 
                          (IFn [(I (Collection x) (Seqable x)) -> (Vec x)]
                               [(Collection x) -> (Vec x)]))

clojure.core/special-symbol? [Any -> Boolean]

clojure.core/integer? (Pred AnyInteger)
clojure.core/number? (Pred Number)
clojure.core/var? (Pred (Var2 Nothing Any))
clojure.core/class? (Pred Class)

clojure.core/resolve (IFn [Symbol -> (U (Var2 Nothing Any) Class nil)]
                         ; should &env arg be more accurate?
                         [Any Symbol -> (U (Var2 Nothing Any) Class nil)])

clojure.core/ns-resolve (IFn [(U Symbol Namespace) Symbol -> (U (Var2 Nothing Any) Class nil)]
                            ; should &env arg be more accurate?
                            [(U Symbol Namespace) Any Symbol -> (U (Var2 Nothing Any) Class nil)])

clojure.core/extenders [Any -> (U nil (Seqable (U Class nil)))]

clojure.core/+ (IFn [Long * -> Long]
                    [(U Long Double) * -> Double]
                    [AnyInteger * -> AnyInteger]
                    [Number * -> Number])
clojure.core/- (IFn [Long Long * -> Long]
                    [(U Long Double) (U Long Double) * -> Double]
                    [AnyInteger AnyInteger * -> AnyInteger]
                    [Number Number * -> Number])
clojure.core/* (IFn [Long * -> Long]
                    [(U Long Double) * -> Double]
                    [AnyInteger * -> AnyInteger]
                    [Number * -> Number])
clojure.core// (IFn [Double Double * -> Double]
                    [Number Number * -> Number])

clojure.core/+' (IFn [AnyInteger * -> AnyInteger]
                     [Number * -> Number])
clojure.core/-' (IFn [AnyInteger AnyInteger * -> AnyInteger]
                     [Number Number * -> Number])
clojure.core/*' (IFn [AnyInteger * -> AnyInteger]
                    [Number * -> Number])
clojure.core/quot (IFn [Long Long -> Long]
                       [(U Long Double) (U Long Double) -> Double]
                       [AnyInteger AnyInteger -> AnyInteger] 
                       [Number Number -> Number])

clojure.core/unchecked-inc (IFn [AnyInteger -> AnyInteger]
                                [Number -> Number])
clojure.core/unchecked-inc-int [Number -> AnyInteger]
clojure.core/unchecked-dec (IFn [AnyInteger -> AnyInteger]
                                [Number -> Number])
clojure.core/unchecked-dec-int [Number -> AnyInteger]
clojure.core/unchecked-subtract (IFn [AnyInteger AnyInteger -> AnyInteger]
                                     [Number Number -> Number])
clojure.core/unchecked-subtract-int [Number Number -> AnyInteger]
clojure.core/unchecked-negate (IFn [AnyInteger -> AnyInteger]
                                   [Number -> Number])
clojure.core/unchecked-negate-int [Number -> AnyInteger]
clojure.core/unchecked-add (IFn [AnyInteger AnyInteger -> AnyInteger]
                                [Number Number -> Number])
clojure.core/unchecked-add-int [Number Number -> AnyInteger]
clojure.core/unchecked-multiply (IFn [AnyInteger AnyInteger -> AnyInteger]
                                     [Number Number -> Number])
clojure.core/unchecked-multiply-int [Number Number -> AnyInteger]
clojure.core/unchecked-divide-int [Number Number -> AnyInteger]
clojure.core/unchecked-remainder-int [Number Number -> AnyInteger]
clojure.core/inc (IFn [Long -> Long]
                      [Double -> Double]
                      [AnyInteger -> AnyInteger]
                      [Number -> Number])
clojure.core/dec (IFn [Long -> Long]
                      [Double -> Double]
                      [AnyInteger -> AnyInteger]
                      [Number -> Number])

clojure.core/inc' (IFn [AnyInteger -> AnyInteger]
                       [Number -> Number])
clojure.core/dec' (IFn [AnyInteger -> AnyInteger]
                          [Number -> Number])

clojure.core/rationalize [Number -> Number]

clojure.core/bit-not [AnyInteger -> AnyInteger]
clojure.core/bit-and [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
clojure.core/bit-or [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
clojure.core/bit-xor [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
clojure.core/bit-and-not [AnyInteger AnyInteger AnyInteger * -> AnyInteger]
clojure.core/bit-clear [AnyInteger AnyInteger -> AnyInteger]
clojure.core/bit-set [AnyInteger AnyInteger -> AnyInteger]
clojure.core/bit-flip [AnyInteger AnyInteger -> AnyInteger]
clojure.core/bit-test [AnyInteger AnyInteger -> AnyInteger]
clojure.core/bit-shift-left [AnyInteger AnyInteger -> AnyInteger]
clojure.core/bit-shift-right [AnyInteger AnyInteger -> AnyInteger]
clojure.core/unsigned-bit-shift-right [AnyInteger AnyInteger -> AnyInteger]

clojure.core/even? [AnyInteger -> Boolean]
clojure.core/odd? [AnyInteger -> Boolean]

clojure.core/peek (All [x]
                       (IFn [(I NonEmptyCount (Stack x)) -> x]
                           [(Stack x) -> x]))
clojure.core/pop (All [x]
                      (IFn
                        [(List x) -> (List x)]
                        [(Vec x) -> (Vec x)]
                        [(Stack x) -> (Stack x)]))

clojure.core/get-thread-bindings
    [-> (Map (Var2 Nothing Any) Any)]
clojure.core/bound-fn*
    (All [r b ...]
         [[b ... b -> r] -> [b ... b -> r]])

clojure.core/find-var
    [Symbol -> (U nil (Var2 Nothing Any))]

clojure.core/agent
    (All [x] [x & :optional {:validator (U nil [x -> Any]) :meta Any
                             :error-handler (U nil [(Agent1 x) Throwable -> Any])
                             :error-mode (U ':continue ':fail)} 
              -> (Agent1 x)])

clojure.core/set-agent-send-executor!
    [java.util.concurrent.ExecutorService -> Any]

clojure.core/set-agent-send-off-executor!
    [java.util.concurrent.ExecutorService -> Any]

clojure.core/send-via (All [w r b ...] 
                           [(Agent2 w r) [r b ... b -> w] b ... b -> (Agent2 w r)])

clojure.core/send (All [w r b ...] 
                           [(Agent2 w r) [r b ... b -> w] b ... b -> (Agent2 w r)])

clojure.core/send-off (All [w r b ...] 
                           [(Agent2 w r) [r b ... b -> w] b ... b -> (Agent2 w r)])

clojure.core/await [(Agent2 Nothing Any) * -> nil]
clojure.core/await-for [AnyInteger (Agent2 Nothing Any) * -> Boolean]
clojure.core/await1 (All [w r] [(Agent2 w r) -> (Agent2 w r)])

clojure.core/release-pending-sends [-> AnyInteger]

clojure.core/add-watch
        (All [x [a :< (IRef Nothing x)]]
             (IFn 
               ; this arity remembers the type of reference we pass to the function
               [a Any [Any a x x -> Any] -> Any]
               ; if the above cannot be inferred, 
               [(IRef Nothing x) Any [Any (IRef Nothing x) x x -> Any] -> Any]))

clojure.core/remove-watch [(IRef Nothing Any) Any -> Any]

clojure.core/agent-error [(Agent2 Nothing Any) -> (U nil Throwable)]

clojure.core/restart-agent
(All [w]
     ; w is invariant
     [(Agent2 w Any) w & :optional {:clear-actions Any} -> Any])

clojure.core/set-error-handler!
(All [w r]
    [(Agent2 w r) [(Agent2 w r) Throwable -> Any] -> Any])

clojure.core/error-handler
(All [w r]
    [(Agent2 w r) -> (U nil [(Agent2 w r) Throwable -> Any])])

clojure.core/set-error-mode!
    [(Agent2 Nothing Any) (U ':fail ':continue) -> Any]

clojure.core/error-mode
    [(Agent2 Nothing Any) -> Any]

clojure.core/agent-errors
    [(Agent2 Nothing Any) -> (U nil (ASeq Throwable))]
clojure.core/clear-agent-errors
    [(Agent2 Nothing Any) -> Any]

clojure.core/shutdown-agents [-> Any]

clojure.core/take
     (All [x]
       [AnyInteger (U nil (Seqable x)) -> (ASeq x)])

clojure.core/drop
     (All [x]
       [AnyInteger (U nil (Seqable x)) -> (ASeq x)])

clojure.core/take-last
     (All [x]
       [AnyInteger (U nil (Seqable x)) -> (NilableNonEmptyASeq x)])

clojure.core/drop-last
     (All [x]
       [AnyInteger (U nil (Seqable x)) -> (ASeq x)])

clojure.core/hash [Any -> AnyInteger]
clojure.core/hash-combine [AnyInteger Any -> AnyInteger]

clojure.core/ifn? (Pred clojure.lang.IFn)
clojure.core/fn? (Pred t/Fn)

clojure.core/instance? [Class Any -> Boolean]

clojure.core/cons
     (All [x]
       [x (Option (Seqable x)) -> (ASeq x)])

clojure.core/reverse
     (All [x]
       [(Option (Seqable x)) -> (ASeq x)])

clojure.core/rseq
     (All [x]
       [(clojure.core.typed/Reversible x) -> (Option (NonEmptyASeq x))])

;coercions
;TODO maybe these argument type shouldn't be Any
clojure.core/bigdec [Number -> BigDecimal]
clojure.core/bigint [Number -> clojure.lang.BigInt]
clojure.core/biginteger [Number -> java.math.BigInteger]
clojure.core/boolean [Any -> Boolean]
clojure.core/byte [Any -> Byte]
clojure.core/char [Any -> Character]
clojure.core/double [Any -> Double]
clojure.core/float [Any -> Float]
clojure.core/int [Any -> Integer]
clojure.core/long [Any -> Long]
clojure.core/num [Any -> Number]
clojure.core/short [Any -> Short]

;array ctors
clojure.core/boolean-array (IFn [(U nil Number (Seqable Boolean)) -> (Array boolean)]
                                    [Number (U nil Boolean (Seqable Boolean)) -> (Array boolean)])
clojure.core/byte-array (IFn [(U nil Number (Seqable Byte)) -> (Array byte)]
                                 [Number (U nil Byte (Seqable Byte)) -> (Array byte)])
clojure.core/char-array (IFn [(U nil Number (Seqable Character)) -> (Array char)]
                            [Number (U nil Number (Seqable Character)) -> (Array char)])
clojure.core/short-array (IFn [(U nil Number (Seqable Short)) -> (Array short)]
                                  [Number (U nil Short (Seqable Short)) -> (Array short)])
clojure.core/int-array (IFn [(U nil Number (Seqable Number)) -> (Array int)]
                                [Number (U nil Number (Seqable Number)) -> (Array int)])
clojure.core/double-array (IFn [(U nil Number (Seqable Number)) -> (Array double)]
                                   [Number (U nil Number (Seqable Number)) -> (Array double)])

;cast to java array
clojure.core/booleans [Any -> (Array boolean)]
clojure.core/bytes [Any -> (Array byte)]
clojure.core/chars [Any -> (Array char)]
clojure.core/shorts [Any -> (Array short)]
clojure.core/ints [Any -> (Array int)]
clojure.core/longs [Any -> (Array long)]
clojure.core/floats [Any -> (Array float)]
clojure.core/doubles [Any -> (Array double)]

clojure.core/max-key (All [x] 
                          [[x -> Number] x x x * -> x])
clojure.core/min-key (All [x] 
                          [[x -> Number] x x x * -> x])

clojure.core/< [Number Number * -> Boolean]

clojure.core/<= [Number Number * -> Boolean]

clojure.core/> [Number Number * -> Boolean]

clojure.core/>= [Number Number * -> Boolean]

clojure.core/== [Number Number * -> Boolean]

clojure.core/max [Number Number * -> Number]
clojure.core/min [Number Number * -> Number]

clojure.core/ref (All [x] [x & :optional {:validator (U nil [x -> Any]) :meta (U nil (Map Any Any))
                                          :min-history (U nil AnyInteger)
                                          :max-history (U nil AnyInteger)}
                           -> (clojure.lang.Ref x x)])

clojure.core/rand (IFn [-> Number]
                      [Number -> Number])

clojure.core/rand-int [Int -> Int]

clojure.core/ex-info (IFn [(U nil String) (Map Any Any) -> ExInfo]
                         [(U nil String) (Map Any Any) (U nil Throwable) -> ExInfo])

clojure.core/ex-data (IFn [ExInfo -> (Map Any Any)]
                         [Any -> (U nil (Map Any Any))])


;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (Pred (IChunk Any)).
clojure.core/chunked-seq? [Any -> Any]
clojure.core/chunk-first 
     (All [x]
          ;should be IChunkedSeq -> IChunk
          [(Seqable x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-rest
     (All [x]
          ;should be IChunkRest -> Seq
          [(clojure.lang.Seqable x) -> (ASeq x)])
clojure.core/chunk-buffer
     (All [x]
          [(U Integer Long) -> (clojure.lang.ChunkBuffer x)])
clojure.core/chunk
     (All [x]
          [(clojure.lang.ChunkBuffer x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-cons
     (All [x]
          [(clojure.lang.IChunk x) (Option (Seqable x)) -> (Option (Seqable x))])
clojure.core/chunk-append
     (All [x]
          [(clojure.lang.ChunkBuffer x) x -> Any])
;;END CHUNK HACKS


clojure.core/subvec (All [x] 
                     (IFn [(Vec x) AnyInteger -> (Vec x)]
                         [(Vec x) AnyInteger AnyInteger -> (Vec x)]))

clojure.core/alias [Symbol Symbol -> nil]
clojure.core/all-ns [-> (Nilable (NonEmptyASeq Namespace))]

clojure.core/*file* String
clojure.core/*command-line-args* (U nil (NonEmptyASeq String))
clojure.core/*warn-on-reflection* Boolean
clojure.core/*compile-path* String
clojure.core/*compile-files* Boolean
clojure.core/*unchecked-math* Boolean
clojure.core/*compiler-options* (Map Any Any)
clojure.core/*in* java.io.Reader
clojure.core/*out* java.io.Writer
clojure.core/*err* java.io.Writer
clojure.core/*flush-on-newline* Boolean
clojure.core/*print-meta* Boolean
clojure.core/*print-dup* Boolean
clojure.core/*print-readably* Boolean
clojure.core/*read-eval* (U ':unknown Boolean)

clojure.core/trampoline 
       (All [r b ...]
         [[b ... b -> (Rec [f] (U r [-> (U f r)]))]
          b ... b -> r])


;; math.numeric-tower

clojure.math.numeric-tower/floor
(IFn [AnyInteger -> AnyInteger]
    [Number -> Number])

clojure.math.numeric-tower/abs
(IFn [AnyInteger -> AnyInteger]
    [Number -> Number])

;; core.match

clojure.core.match/backtrack Exception

clojure.core/eval [Any -> Any]
clojure.core/rand-nth (All [x] [(U (Indexed x) (SequentialSeqable x)) -> x])

      )
(h/var-mappings
clojure.set/union (All [x] [(Set x) * -> (Set x)])
clojure.set/intersection (All [x] [(Set x) (Set x) * -> (Set x)])
clojure.set/difference (All [x] [(Set x) (Set Any) * -> (Set x)])

clojure.repl/pst (IFn [-> nil]
                      [(U Int Throwable) -> nil]
                      [Throwable Int -> nil])
  )
    {'clojure.core/count (count-type)
     'clojure.core/aset-boolean (aset-*-type 'boolean)
     'clojure.core/aset-byte (aset-*-type 'byte)
     'clojure.core/aset-char (aset-*-type 'char)
     'clojure.core/aset-short (aset-*-type 'short)
     'clojure.core/aset-int (aset-*-type 'int)
     'clojure.core/aset-long (aset-*-type 'long)
     'clojure.core/aset-float (aset-*-type 'float)
     'clojure.core/aset-double (aset-*-type 'double)
     'clojure.core/nth (nth-type)
     }
))


;(comment
;  (aget my-array 0 1 2)
;  (aget (aget my-array 0) 1 2)
;  (aget (aget (aget my-array 0) 1) 2)
;
;  (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
;       (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
;            (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
;                 (Associative Keyword Number)
;                 :a 1)
;            :b 2)
;       :c 3)
;
;  (assoc my-map :a 1 :b 2 :c 3)
;  (assoc (assoc my-map :a 1) :b 2 :c 3)
;  (assoc (assoc (assoc my-map :a 1) :b 2) :c 3)
;
;  clojure.core/aset
;       (Label [rec]
;              (All [w [v :< w] :dotted [b]]
;                   [(Array w _) AnyInteger v -> v]
;                   [(Array _ r) AnyInteger b ... b
;                    :recur (rec r b ... b)]))
;
;  clojure.core/aget 
;       (Label [rec]
;              (All [x :dotted [b]] 
;                   (IFn [(Array _ x) AnyInteger -> x]
;                       [(Array _ x) AnyInteger b ... b
;                        :recur 
;                        (rec x b ... b)])))
;
;  clojure.core/assoc 
;       (All [[h <: (IPersistentMap Any Any)]
;             a b e ...2]
;         [h k ...2 a b -> (Assoc h k ...2 a b)])
;
;       (Label [rec]
;              (All [[h :< (HMap {})] x y [k :< (I AnyValue Keyword)] [e :< k] :dotted [b]]
;                   [h k v -> (I h (HMap k v))]
;                   [(Associative y x) y x -> (Associative y x)]
;                   [h k v b ... b
;                    :recur (rec (I h (HMap {k v})) b ... b)]
;                   [(Associative y x) y x b ... b
;                    :recur (rec (Associative y x) b ... b)]
;                   ))
;
;  clojure.core/dissoc
;       (Label [rec]
;              (All [[m :< (Associative _ _)] :dotted [b]]
;                   [nil Any * -> nil]
;                   [m -> m]
;                   [m k b ... b
;                    :recur
;                    (rec (I m (HMap {} :without [k])) b ... b)]))
;
;  (update-in {:a {:b 1}} [:a :b] inc)
;  (update-in 
;    (update-in {:a {:b 1}} [:a] inc) 
;    [:b] 
;    inc)
;
;  clojure.core/update-in
;       (FixedPoint
;         (All [[x :< (U nil (Associative Any Any))] k [l :< k] v r e
;               :dotted [a b]]
;              (IFn [(HMap {l v}) (Vector* k) [v a ... a -> r] a ... a -> (I x (HMap {l r}))]
;                  [(HMap {l r}) (Vector* k b ... b) [v a ... a -> e] a ... a
;                   :recur
;                   [r (Vector* b ... b) [v a ... a -> e] a ... a]])))
;
;  ;clojure.core/get-in 
;  ;     (Label [rec]
;  ;       (All [[x :< (U nil (Associative Any Any))] k :dotted [b]]
;  ;            (IFn [x (Vector*) -> x]
;  ;                [x (Vector*) _ -> x]
;  ;                [(U nil (Associative _ y) (Vector* k b ... b) a -> x
;  ;                ;TODO
;  ;                [(U nil (Associative Any y)) (Vector* k) -> (U nil x)]
;  ;                    ))))
;
;  clojure.core/partial 
;       (Label [rec]
;              (All [x [a :< x] r :dotted [b c]]
;                   (IFn [[x c ... c -> r] a -> [c ... c -> r]]
;                       [[x c ... c -> r] a b ... b
;                        :recur
;                        (rec [c ... c -> r] b ... b)])))
;
;  ;                                [[y -> x] [b ... b -> y] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [b ... b -> z] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [b ... b -> k] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [b ... b -> l] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [m -> l] [b ... b -> m] -> [b ... b -> x]]
;
;  clojure.core/juxt
;                  (All [y b ... c ...]
;                       [[b ... b -> y] [b ... b -> c] ... c -> [b ... b -> (DottedVec y c ... c)]])
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nocheck env

(delay-and-cache-env ^:private init-var-nochecks
  (set (keys (init-var-env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method param annotations

(delay-and-cache-env ^:private init-method-nilable-param-env {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method return annotations

(delay-and-cache-env ^:private init-method-nonnilable-return-env
  (h/method-nonnilable-return-mappings

java.lang.Object/getClass #{0}
clojure.lang.Compiler/munge :all
java.lang.Class/getName :all
java.lang.Class/forName :all

java.lang.Object/toString :all
java.lang.String/toUpperCase :all
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override annotations

(delay-and-cache-env ^:private init-method-override-env
  (reset-alias-env!)
  (merge
    {'clojure.lang.RT/nth (nth-type)}
    (h/method-override-mappings

clojure.lang.RT/isReduced [Any -> Boolean]

clojure.lang.Indexed/nth
  (All [x y]
       (IFn [(Indexed x) AnyInteger -> x]
           [(Indexed x) AnyInteger y -> (U x y)]))


;what about combinations of references and primitives?
clojure.lang.RT/box
(All [x]
     (IFn [Character -> Character]
          [Integer -> Integer]
          [Short -> Short]
          [Boolean -> Boolean]
          [Byte -> Byte]
          [Long -> Long]
          [Float -> Float]
          [Double -> Double]
          [(U Byte Short Integer Long) -> AnyInteger]
          [(U Float Double) -> Number]
          [nil -> nil]
          [x -> x]))

clojure.lang.RT/booleanCast [Any -> Boolean]

clojure.lang.Numbers/char_array (IFn [(U nil Number (Seqable Character)) -> (Array char)]
                                    [Number (U Number (Seqable Character)) -> (Array char)])


clojure.lang.LockingTransaction/runInTransaction
                 (All [x]
                   [[-> x] -> x])

;array ops
clojure.lang.RT/alength [(ReadOnlyArray Any) -> Integer]

clojure.lang.RT/aget (All [o]
                        [(ReadOnlyArray o) Integer -> o])

clojure.lang.RT/aset (All [i o]
                          [(Array2 i o) AnyInteger i -> o])

;get
;same as clojure.core/get
clojure.lang.RT/get (All [x y]
                         (IFn 
                           ;no default
                           [(IPersistentSet x) Any -> (Option x)]
                           [nil Any -> nil]
                           [(Option (ILookup Any x)) Any -> (Option x)]
                           [java.util.Map Any -> (Option Any)]
                           [String Any -> (Option Character)]
                           ;default
                           [(IPersistentSet x) Any y -> (U y x)]
                           [nil Any y -> y]
                           [(Option (ILookup Any x)) Any y -> (U y x)]
                           [java.util.Map Any y -> (U y Any)]
                           [String Any y -> (U y Character)]))

;numbers
clojure.lang.Numbers/add (IFn [Long Long -> Long]
                              [Double Double -> Double]
                              [AnyInteger AnyInteger -> AnyInteger]
                              [Number Number -> Number])
clojure.lang.Numbers/inc (IFn [Long -> Long]
                              [Double -> Double]
                              [AnyInteger -> AnyInteger]
                              [Number -> Number])
clojure.lang.Numbers/dec (IFn [Long -> Long]
                              [Double -> Double]
                              [AnyInteger -> AnyInteger]
                              [Number -> Number])
clojure.lang.Numbers/quotient (IFn [Long Long -> Long]
                                   [(U Long Double) (U Long Double) -> Double]
                                   [AnyInteger AnyInteger -> AnyInteger]
                                   [Number Number -> Number])
clojure.lang.Numbers/incP (IFn [Long -> (U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [AnyInteger -> AnyInteger]
                               [Number -> Number])
clojure.lang.Numbers/decP (IFn [Long -> (U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [AnyInteger -> AnyInteger]
                               [Number -> Number])
clojure.lang.Numbers/unchecked_inc (IFn [Long -> Long]
                                        [Double -> Double]
                                        [AnyInteger -> AnyInteger]
                                        [Number -> Number])
clojure.lang.Numbers/unchecked_dec (IFn [Long -> Long]
                                        [Double -> Double]
                                        [AnyInteger -> AnyInteger]
                                        [Number -> Number])
clojure.lang.Numbers/unchecked_int_inc [Number -> AnyInteger]
clojure.lang.Numbers/unchecked_int_dec [Number -> AnyInteger]
clojure.lang.Numbers/unchecked_int_negate [Number -> AnyInteger]
clojure.lang.Numbers/unchecked_int_subtract [Number Number -> AnyInteger]
clojure.lang.Numbers/unchecked_int_add [Number -> AnyInteger]
clojure.lang.Numbers/unchecked_minus (IFn 
                                       ; negate
                                       [Long -> Long]
                                       [Double -> Double]
                                       [AnyInteger AnyInteger -> AnyInteger]
                                       [Number Number -> Number]
                                       ; subtract
                                       [Long Long -> Long]
                                       [(U Long Double) (U Long Double) -> Double]
                                       [AnyInteger -> AnyInteger]
                                       [Number -> Number])
clojure.lang.Numbers/minus (IFn
                             ; negate
                             [Long -> Long]
                             [Double -> Double]
                             [AnyInteger -> AnyInteger]
                             [Number -> Number]
                             ;minus
                             [Long Long -> Long]
                             [(U Double Long) (U Double Long) -> Long]
                             [AnyInteger AnyInteger -> AnyInteger]
                             [Number Number -> Number])
clojure.lang.Numbers/unchecked_multiply (IFn [Long Long -> Long]
                                             [(U Long Double) (U Long Double) -> Double]
                                             [AnyInteger AnyInteger -> AnyInteger]
                                             [Number Number -> Number])
clojure.lang.Numbers/unchecked_int_multiply [Number Number -> AnyInteger]
clojure.lang.Numbers/unchecked_int_divide [Number Number -> AnyInteger]
clojure.lang.Numbers/unchecked_int_remainder [Number Number -> AnyInteger]
clojure.lang.Numbers/multiply (IFn [Long Long -> Long]
                                   [(U Double Long) (U Double Long) -> Double]
                                   [AnyInteger AnyInteger -> AnyInteger]
                                   [Number Number -> Number])
clojure.lang.Numbers/divide (IFn [Long Long -> Long]
                                   [(U Double Long) (U Double Long) -> Double]
                                   [AnyInteger AnyInteger -> AnyInteger]
                                   [Number Number -> Number])
      ;bit-not
clojure.lang.Numbers/not [AnyInteger -> Long]
;bit-and
clojure.lang.Numbers/and [AnyInteger AnyInteger -> Long]
;bit-or
clojure.lang.Numbers/or [AnyInteger AnyInteger -> Long]
;bit-xor
clojure.lang.Numbers/xor [AnyInteger AnyInteger -> Long]
;bit-and-not
clojure.lang.Numbers/andNot [AnyInteger AnyInteger -> Long]
; unsigned-bit-shift-right 
clojure.lang.Numbers/unsignedShiftRight [AnyInteger AnyInteger -> Long]

clojure.lang.Numbers/max (IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [Number Number -> Number])
clojure.lang.Numbers/min (IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [Number Number -> Number])

clojure.lang.Numbers/lt [Number Number -> Boolean]
clojure.lang.Numbers/lte [Number Number -> Boolean]
clojure.lang.Numbers/gt [Number Number -> Boolean]
clojure.lang.Numbers/gte [Number Number -> Boolean]

clojure.lang.Numbers/isZero (Pred (Value 0))

clojure.lang.Util/compare [Any Any -> Number]

; this is overloaded in interesting ways, but this is good enough for destructuring purposes
clojure.lang.PersistentHashMap/create [(U nil (ISeq Any) java.util.Map (ReadOnlyArray Object)) -> (Map Any Any)]
    )
    {'clojure.lang.RT/count (count-type)}))

(comment
  clojure.lang.IFn/invoke (All [r a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 arest]
                               (IFn
                                 [[-> r] -> r]
                                 [[a0 -> r] a0 -> r]
                                 [[a0 a1 -> r] a0 a1 -> r]
                                 [[a0 a1 a2 -> r] a0 a1 a2 -> r]
                                 [[a0 a1 a2 a3 -> r] a0 a1 a2 a3 -> r]
                                 [[a0 a1 a2 a3 a4 -> r] a0 a1 a2 a3 a4 -> r]
                                 [[a0 a1 a2 a3 a4 a5 -> r] a0 a1 a2 a3 a4 a5 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 -> r] a0 a1 a2 a3 a4 a5 a6 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 -> r] a0 a1 a2 a3 a4 a5 a6 a7 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 arest * -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 (Seqable arest) -> r]
                                 )))

(delay-and-cache-env ^:private init-ctor-override-env
  (reset-alias-env!)
  (h/ctor-override-mappings

clojure.lang.LazySeq (All [x]
                          [[-> (Option (Seqable x))] -> (LazySeq x)])
clojure.lang.Delay (All [x]
                        [[-> x] -> (clojure.lang.Delay x)])
    ))

(delay-and-cache-env ^:private init-declared-kinds {})

(delay-and-cache-env ^:private init-datatype-env {})

(delay-and-cache-env ^:private init-datatype-ancestor-env {})

(defn reset-clojure-envs! []
  (impl/with-clojure-impl
    (reset-alias-env!)
    ((v 'clojure.core.typed.var-env/reset-var-type-env!)
     (init-var-env) 
     (init-var-nochecks))
    ((v 'clojure.core.typed.method-return-nilables/reset-nonnilable-method-return-env!) 
     (init-method-nonnilable-return-env))
    ((v 'clojure.core.typed.method-param-nilables/reset-method-nilable-param-env!)
     (init-method-nilable-param-env))
    ((v 'clojure.core.typed.method-override-env/reset-method-override-env!)
     (init-method-override-env))
    ((v 'clojure.core.typed.ctor-override-env/reset-constructor-override-env!) 
     (init-ctor-override-env))
    ((v 'clojure.core.typed.protocol-env/reset-protocol-env!) 
     (init-protocol-env))
    (base-rclass/reset-rclass-env!)
    ((v 'clojure.core.typed.declared-kind-env/reset-declared-kinds!) 
     (init-declared-kinds))
    ((v 'clojure.core.typed.datatype-env/reset-datatype-env!) 
     (init-datatype-env))
    ((v 'clojure.core.typed.datatype-ancestor-env/reset-datatype-ancestors!)
     (init-datatype-ancestor-env)))
  nil)
