(ns clojure.core.typed.base-env
  (:import (clojure.lang Atom Symbol Namespace Keyword Named IMapEntry Seqable
                         LazySeq PersistentHashSet PersistentList APersistentVector
                         APersistentSet Sorted IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap IDeref ISeq IMeta ASeq IPersistentCollection
                         ILookup Indexed Associative IPersistentStack PersistentVector Cons
                         IPersistentList IRef IReference AReference ARef Var))
  (:require [clojure.core.typed
             [base-env-helper :as h]
             [parse-unparse :as prs]
             [type-rep :as r]
             [type-ctors :as c]
             [path-rep :as pe]
             [object-rep :as obj]
             [fold-default]
             [name-env :as nme-env]
             [rclass-env :as rcls]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

;; TODO fix metadata representation
;; TODO remove redundant ancestors, add tests to ensure they are preserved.

#_(alter-class IFn [[a :variance :covariant
                   :< AnyFunction]])

#_(alter-class clojure.lang.Fn [[a :variance :covariant
                               :< AnyFunction]])

#_(alter-class AFn [[a :variance :covariant
                   :< AnyFunction]]
             :replace
             {IFn (IFn a)})

#_(alter-class AFunction [[a :variance :covariant
                         :< AnyFunction]]
             :replace
             {AFn (AFn a)
              IFn (IFn a)
              clojure.lang.Fn (clojure.lang.Fn a)})


(def init-altered-env
  (h/alters

Seqable [[[a :variance :covariant]]
         ]

IMeta [[[a :variance :covariant]]]

IPersistentCollection [[[a :variance :covariant]]]

ISeq [[[a :variance :covariant]]
      :replace
      {Seqable (Seqable a)
       IPersistentCollection (IPersistentCollection a)}]

clojure.lang.ChunkBuffer [[[a :variance :invariant]]]

clojure.lang.IChunkedSeq [[[a :variance :covariant]]
                          :replace
                          {Seqable (Seqable a)
                           IPersistentCollection (IPersistentCollection a)
                           ISeq (ISeq a)}]

clojure.lang.Indexed [[[a :variance :covariant]]]

clojure.lang.IChunk [[[a :variance :covariant]]
                     :replace
                     {clojure.lang.Indexed (clojure.lang.Indexed a)}]

ILookup [[[a :variance :covariant]
          [b :variance :covariant]]]

IPersistentSet [[[a :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection a)
                 Seqable (Seqable a)}]

APersistentSet [[[a :variance :covariant]]
                :replace
                {Seqable (Seqable a)
                 IFn [Any -> (U a nil)]
                 AFn [Any -> (U a nil)]
                 IPersistentCollection (IPersistentCollection a)
                 IPersistentSet (IPersistentSet a)}]

PersistentHashSet [[[a :variance :covariant]]
                   :replace
                   {Seqable (Seqable a)
                    APersistentSet (APersistentSet a)
                    IFn [Any -> (U a nil)]
                    AFn [Any -> (U a nil)]
                    IPersistentSet (IPersistentSet a)
                    IPersistentCollection (IPersistentCollection a)
                    IMeta (IMeta Any)}]

Associative [[[a :variance :covariant]
              [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection Any)
              Seqable (Seqable Any)
              ILookup (ILookup a b)}]

IMapEntry [[[a :variance :covariant]
            [b :variance :covariant]]]

IPersistentMap [[[a :variance :covariant]
                 [b :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection (IMapEntry a b))
                 Seqable (Seqable (IMapEntry a b))
                 ILookup (ILookup a b)
                 Associative (Associative a b)}]

ASeq [[[a :variance :covariant]]
      :replace
      {IPersistentCollection (IPersistentCollection a)
       Seqable (Seqable a)
       ISeq (ISeq a)
       IMeta (IMeta Any)}]

IPersistentStack [[[a :variance :covariant]]
                  :replace
                  {IPersistentCollection (IPersistentCollection a)
                   Seqable (Seqable a)}]

IPersistentVector [[[a :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection a)
                    Seqable (Seqable a)
                    IPersistentStack (IPersistentStack a)
                    ILookup (ILookup Number a)
                    Associative (Associative Number a)
                    Indexed (Indexed a)}]

APersistentMap [[[a :variance :covariant] 
                 [b :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection (IMapEntry a b))
                 IPersistentMap (IPersistentMap a b)
                 Seqable (Seqable (IMapEntry a b))
                 IFn (All [d]
                          (Fn [Any -> (U nil b)]
                              [Any d -> (U b d)]))
                 ILookup (ILookup a b)
                 Associative (Associative Number a)}]

APersistentVector [[[a :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection a)
                    Seqable (Seqable a)
                    IPersistentVector (IPersistentVector a)
                    IFn [Number -> a]
                    IPersistentStack (IPersistentStack a)
                    ILookup (ILookup Number a)
                    Associative (Associative Number a)
                    Indexed (Indexed a)}]

PersistentVector [[[a :variance :covariant]]
                  :replace
                  {APersistentVector (APersistentVector a)
                   IPersistentCollection (IPersistentCollection a)
                   Seqable (Seqable a)
                   IPersistentVector (IPersistentVector a)
                   IFn [Number -> a]
                   IPersistentStack (IPersistentStack a)
                   ILookup (ILookup Number a)
                   IMeta (IMeta Any)
                   Associative (Associative Number a)
                   Indexed (Indexed a)}]

Cons [[[a :variance :covariant]]
      :replace
      {IPersistentCollection (IPersistentCollection a)
       ASeq (ASeq a)
       Seqable (Seqable a)
       ISeq (ISeq a)
       IMeta (IMeta Any)}]

IPersistentList [[[a :variance :covariant]]
                 :replace
                 {IPersistentCollection (IPersistentCollection a)
                  Seqable (Seqable a)
                  IPersistentStack (IPersistentStack a)}]

PersistentList [[[a :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection a)
                 ASeq (ASeq a)
                 Seqable (Seqable a)
                 IPersistentList (IPersistentList a)
                 ISeq (ISeq a)
                 IPersistentStack (IPersistentStack a)
                 IMeta (IMeta Any)}]

Symbol [[]
             :replace
             {IMeta (IMeta Any)}]

IDeref [[[r :variance :covariant]]]


IRef [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IDeref (IDeref r)}]

IReference [[[w :variance :contravariant]
             [r :variance :covariant]]
            :replace
            {IMeta (IMeta Any)}]

AReference [[[w :variance :contravariant]
             [r :variance :covariant]]
            :replace
            {IMeta (IMeta Any)
             IReference (IReference w r)}]

ARef [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IRef (IRef w r)
       IMeta (IMeta Any)
       AReference (AReference w r)
       IDeref (IDeref r)
       IReference (IReference w r)}]

Var [[]
     :replace
     {AReference (AReference Any Any)
      IReference (IReference Any Any)
      IRef (IRef Any Any)
      ARef (ARef Any Any)
      IDeref (IDeref Any)
      IMeta (IMeta Any)}]

Atom [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IRef (IRef w r)
       IMeta (IMeta Any)
       AReference (AReference w r)
       ARef (ARef w r)
       IDeref (IDeref r)
       IReference (IReference w r)}]

LazySeq [[[a :variance :covariant]]
         :replace
         {Seqable (Seqable a)
          ISeq (ISeq a)
          IMeta (IMeta Any)
          IPersistentCollection (IPersistentCollection a)}]

; Hack for Seqable things. Not needed if Seqable was a protocol.

java.lang.CharSequence [[]
                        :unchecked-ancestors
                        #{(Seqable Character)}]

;FIXME Need to correctly check ancestors, this shouldn't be necessary because String is a CharSequence
; CTYP-15
java.lang.String [[]
                  :unchecked-ancestors
                  #{(Seqable Character)}]
))

(rcls/reset-rclass-env! init-altered-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial type aliases

(def init-alias-env
  (h/alias-mappings

clojure.core.typed/AnyInteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte)
clojure.core.typed/AnyPrimitive (U char int short boolean byte short long float double)

clojure.core.typed/Atom1 (TFn [[x :variance :invariant]] (Atom x x))
clojure.core.typed/Option (TFn [[x :variance :covariant]] (U nil x))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

;;for parsing init-var-env
; must be after init-alias-env def as vars are interned there
(when (resolve 'Option)
  (ns-unmap *ns* 'Option)
  (ns-unmap *ns* 'AnyInteger))
(refer 'clojure.core.typed :only '[Option AnyInteger])
(nme-env/reset-name-env! init-alias-env)

(defn ^:private count-type []
  (r/make-FnIntersection
    (r/make-Function 
      [(c/Un r/-nil (c/RClass-of Seqable [r/-any]) (c/RClass-of clojure.lang.Counted))]
      (prs/parse-type '(U Integer Long))
      nil nil
      :object (obj/->Path [(pe/->CountPE)] 0))))

(def init-var-env
  (merge
    (h/var-mappings

clojure.core.typed/check-ns (Fn [Symbol -> Any]
                                [-> Any])
;; Internal annotations

clojure.core.typed/ensure-clojure [-> Any]
clojure.core.typed/ann* [Any Any -> Any]
clojure.core.typed/def-alias* [Any Any -> Any]
clojure.core.typed/declare-names* [Any -> Any]
clojure.core.typed/typed-deps* [Any -> Any]
clojure.core.typed/ann-datatype* [Any Any Any -> Any]
clojure.core.typed/ann-record* [Any Any Any -> Any]
clojure.core.typed/ann-pdatatype* [Any Any Any Any -> Any]
clojure.core.typed/ann-precord* [Any Any Any Any -> Any]
clojure.core.typed/declare-datatypes* [Any -> Any]
clojure.core.typed/declare-protocols* [Any -> Any]

;; core annotations

clojure.core/*ns* Namespace
clojure.core/*out* java.io.Writer
clojure.core/*err* java.io.Writer
clojure.core/*warn-on-reflection* Any
clojure.core/pop-thread-bindings [-> Any]
clojure.core/load [String * -> Any]
clojure.core/read-string [String -> Any]

clojure.core/namespace [(U Symbol String Keyword) -> (Option String)]
clojure.core/ns-name [Namespace -> Symbol]
clojure.core/name [(U String Named) -> String]
clojure.core/in-ns [Symbol -> nil]
clojure.core/import [Any * -> nil]
clojure.core/identity (All [x] [x -> x
                                     :filters {:then (! (U nil false) 0)
                                               :else (is (U nil false) 0)}
                                     :object {:id 0}])
clojure.core/gensym (Fn [-> Symbol]
                             [String -> Symbol])
clojure.core/intern (Fn [(U Symbol Namespace) Symbol -> Var]
                        [(U Symbol Namespace) Symbol Any -> Var])


clojure.core/memoize (All [x y ...]
                            [[y ... y -> x] -> [y ... y -> x]])

clojure.core/key (All [x]
                           [(IMapEntry x Any) -> x])

;TODO flip filters
clojure.core/complement (All [x] [[x -> Any] -> [x -> boolean]])
; should preserve filters
clojure.core/boolean [Any -> boolean]

clojure.core/filter (All [x y]
                           (Fn
                             [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seqable y)]
                             [[x -> Any] (Option (Seqable x)) -> (Seqable x)]))
clojure.core/remove (All [x y]
                           (Fn 
                             [[x -> Any :filters {:else (is y 0)}] (Option (Seqable x)) -> (Seqable y)]
                             [[x -> Any] (Option (Seqable x)) -> (Seqable x)]
                             ))


clojure.core/take-while (All [x y]
                               (Fn 
                                 [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seqable y)]
                                 [[x -> Any] (Option (Seqable x)) -> (Seqable x)]))
clojure.core/drop-while (All [x]
                               [[x -> Any] (Option (Seqable x)) -> (Seqable x)])

clojure.core/split-with 
     (All [x y z] 
       (Fn
         [[x -> Any :filters {:then (is y 0), :else (is z 0)}] (Option (Seqable x)) -> '[(Seqable y) (Seqable z)]]
         [[x -> Any] (Option (Seqable x)) -> '[(Seqable x) (Seqable x)]]))


clojure.core/repeatedly 
     (All [x]
          (Fn [[-> x] -> (LazySeq x)]
              [AnyInteger [-> x] -> (LazySeq x)]))


clojure.core/some (All [x y] [[x -> y] (Option (Seqable x)) -> (Option y)])

clojure.core/concat (All [x] [(Option (Seqable x)) * -> (Seqable x)])

clojure.core/set (All [x] [(Option (Seqable x)) -> (PersistentHashSet x)])
clojure.core/list (All [x] [x * -> (PersistentList x)])
clojure.core/vector (All [x] [x * -> (APersistentVector x)])
clojure.core/vec (All [x] [(Option (Seqable x)) -> (APersistentVector x)])

clojure.core/not [Any -> boolean]
clojure.core/constantly (All [x y] [x -> [y * -> x]])

clojure.core/disj
     (All [x]
          (Fn [(I (APersistentSet x) Sorted) Any Any * -> (I (APersistentSet x) Sorted)]
              [(APersistentSet x) Any Any * -> (APersistentSet x)]
              [(I (APersistentSet x) Sorted) Any Any * -> (I (IPersistentSet x) Sorted)]
              [(IPersistentSet x) Any Any * -> (IPersistentSet x)]))

clojure.core/assoc
     (All [b c d]
       (Fn [(IPersistentMap b c) b c -> (IPersistentMap b c)]
           [(IPersistentVector d) AnyInteger d -> (IPersistentVector d)]))

clojure.core/zipmap
     (All [k v]
       [(U nil (Seqable k)) (U nil (Seqable v)) -> (APersistentMap k v)])


;most useful case
clojure.core/comp
     (All [x y b ...]
          [[x -> y] [b ... b -> x] -> [b ... b -> y]])

clojure.core/partial 
     (All [y a b c d e f g h i j k l m n o p z ...]
          (Fn [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a b c d e z ... z -> y] a b c d e -> [z ... z -> y]]
              [[a b c d e f z ... z -> y] a b c d e f -> [z ... z -> y]]
              [[a b c d e f g z ... z -> y] a b c d e f g -> [z ... z -> y]]
              [[a b c d e f g h z ... z -> y] a b c d e f g h -> [z ... z -> y]]
              [[a b c d e f g h i z ... z -> y] a b c d e f g h i -> [z ... z -> y]]
              [[a b c d e f g h i j z ... z -> y] a b c d e f g h i j -> [z ... z -> y]]
              [[a b c d e f g h i j k z ... z -> y] a b c d e f g h i j k -> [z ... z -> y]]
              [[a b c d e f g h i j k l z ... z -> y] a b c d e f g h i j k l -> [z ... z -> y]]
              [[a b c d e f g h i j k l m z ... z -> y] a b c d e f g h i j k l m -> [z ... z -> y]]
              [[a b c d e f g h i j k l m n z ... z -> y] a b c d e f g h i j k l m n -> [z ... z -> y]]
              [[a b c d e f g h i j k l m n o z ... z -> y] a b c d e f g h i j k l m n o -> [z ... z -> y]]
              [[a b c d e f g h i j k l m n o p z ... z -> y] a b c d e f g h i j k l m n o p -> [z ... z -> y]]))

clojure.core/str [Any * -> String]
clojure.core/prn-str [Any * -> String]
clojure.core/pr-str [Any * -> String]

clojure.core/print [Any * -> nil]
clojure.core/println [Any * -> nil]
clojure.core/pr [Any * -> nil]
clojure.core/prn [Any * -> nil]

clojure.core/format [String Any * -> String]


clojure.core/re-matcher [java.util.regex.Pattern String -> java.util.regex.Matcher]
clojure.core/re-groups [java.util.regex.Matcher -> (U nil String (APersistentVector (Option String)))]
clojure.core/re-find (Fn [java.util.regex.Matcher -> (U nil String (APersistentVector (Option String)))]
                              [java.util.regex.Pattern String -> (U nil String (APersistentVector (Option String)))])

clojure.core/subs (Fn [String AnyInteger -> String]
                           [String AnyInteger AnyInteger -> String])

;clojure.core/atom (All [x [y :< (U )]]
;                         (Fn 
;                           [x & {:validator (Option [Any -> Any])} :mandatory {:meta (Option y)}
;                            -> (I (Atom x x)
;                                  (IMeta y))]
;                           [x & {:validator (Option [Any -> Any])} -> (Atom x x)]))

clojure.core/atom (All [x]
                         [x Any * -> (Atom x x)])

clojure.core/deref (All [x y]
                             (Fn [(IDeref x) -> x]
                                 [(IDeref x) AnyInteger y -> (U x y)]))
clojure.core/reset! (All [w r]
                              [(Atom w r) w -> r])

clojure.core/swap! (All [w r b ...] 
                             [(Atom w r) [r b ... b -> w] b ... b -> w])

clojure.core/symbol
     (Fn [(U Symbol String) -> Symbol]
         [String String -> Symbol])

clojure.core/seq? (predicate (ISeq Any))
clojure.core/set? (predicate (IPersistentSet Any))
clojure.core/vector? (predicate (IPersistentVector Any))
clojure.core/nil? (predicate nil)
clojure.core/symbol? (predicate Symbol)
clojure.core/map? (predicate (IPersistentMap Any Any))

clojure.core/meta (All [x]
                            (Fn [(IMeta x) -> x]
                                [Any -> nil]))
clojure.core/with-meta (All [[x :< clojure.lang.IObj] y]
                              [x y -> (I x (IMeta y))])

clojure.core/string? (predicate String)

clojure.string/split
     (Fn [String java.util.regex.Pattern -> (APersistentVector String)]
         [String java.util.regex.Pattern AnyInteger -> (APersistentVector String)])

clojure.string/join
     (Fn [(Option (Seqable Any)) -> String]
         [Any (Option (Seqable Any)) -> String])

;usually for string manipulation, accurate enough?
clojure.core/interpose (Fn [Any (Option (Seqable Any)) -> (Seqable Any)])
clojure.core/interleave (All [x] [(Option (Seqable x)) -> (Seqable x)])

clojure.core/repeat (All [x] 
                         (Fn [x -> (Seqable x)]
                             [x AnyInteger -> (Seqable x)]))

clojure.core/class (Fn [nil -> nil :object {:id 0 :path [Class]}]
                            [Object -> Class :object {:id 0 :path [Class]}]
                            [Any -> (Option Class) :object {:id 0 :path [Class]}])

clojure.core/seq (All [x]
                        (Fn 
                          [(I (Seqable x) (CountRange 1)) -> (I (ISeq x) (CountRange 1))]
                          [(Option (Seqable x)) -> (Option (I (ISeq x) (CountRange 1)))
                           :filters {:then (& (is (CountRange 1) 0)
                                              (! nil 0))
                                     :else (| (is nil 0)
                                              (is (ExactCount 0) 0))}]))

clojure.core/empty? [(Option (Seqable Any)) -> boolean
                          :filters {:then (| (is (ExactCount 0) 0)
                                             (is nil 0))
                                    :else (is (CountRange 1) 0)}]

clojure.core/map
     (All [c a b ...]
          [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (LazySeq c)])

clojure.core/mapcat
     (All [c b ...]
          [[b ... b -> (Option (Seqable c))] (Option (Seqable b)) ... b -> (LazySeq c)])

clojure.core/map-indexed
     (All [x y] [[x -> y] (Option (Seqable x)) -> (Seqable '[AnyInteger (Seqable y)])])

clojure.core/merge-with
     (All [k v]
          (Fn [[v v -> v] nil * -> nil]
              [[v v -> v] (IPersistentMap k v) * -> (IPersistentMap k v)]
              [[v v -> v] (Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))]))

clojure.core/reduce
     (All [a c]
          (Fn 
            ;Without accumulator
            ; default
            ; (reduce + my-coll)
            [[a c -> a] (I (Seqable c) (CountRange 1)) -> a]
            [(Fn [a c -> a] [-> a]) (Option (Seqable c)) -> a]
            ; default
            ; (reduce + 3 my-coll)
            [[a c -> a] a (Option (Seqable c)) -> a]))

#_(comment
  clojure.core/reduce
       (All [a c d]
            (Fn 
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
              [(Fn [c c -> c] [-> c]) (U nil (Seqable c)) -> c]
              ;With accumulator
              ; empty coll, f not called, returns accumulator
              ; (reduce + 3 []) => 3
              [Any a (U nil (I (ExactCount 0) (Seqable Any))) -> a]
              ; default
              ; (reduce + 3 my-coll)
              [[a c -> a] a (U nil (Seqable c)) -> a]))
  )

;should be special cased
clojure.core/not= [Any Any * -> boolean]

clojure.core/first
     (All [x]
          (Fn [(Option (I (Seqable x) (ExactCount 0))) -> nil]
              [(I (Seqable x) (CountRange 1)) -> x]
              [(Option (Seqable x)) -> (Option x)]))

clojure.core/second
     (All [x]
          (Fn [(Option (I (Seqable x) (CountRange 0 1))) -> nil]
              [(I (Seqable x) (CountRange 2)) -> x]
              [(Option (Seqable x)) -> (Option x)]))

clojure.core/rest
     (All [x]
          [(Option (Seqable x)) -> (ISeq x)])

clojure.core/last
     (All [x]
          [(Option (Seqable x)) -> (U nil x)])

clojure.core/butlast
     (All [x]
          [(Option (Seqable x)) -> (ISeq x)])

clojure.core/next
     (All [x]
          [(Option (Seqable x)) -> (Option (I (ISeq x) (CountRange 1)))
           :filters {:then (& (is (CountRange 2) 0)
                              (! nil 0))
                     :else (| (is (CountRange 0 1) 0)
                              (is nil 0))}])

clojure.core/conj
     (All [x y]
          (Fn [(IPersistentVector x) x x * -> (IPersistentVector x)]
              [(APersistentMap x y)
               (Option (IMapEntry x y) (Vector* x y))
               (Option (IMapEntry x y) (Vector* x y)) * -> (APersistentMap x y)]
              [(IPersistentMap x y)
               (Option (IMapEntry x y) (Vector* x y))
               (Option (IMapEntry x y) (Vector* x y)) * -> (IPersistentMap x y)]
              [(IPersistentSet x) x x * -> (IPersistentSet x)]
              [(ISeq x) x x * -> (ASeq x)]
              [nil x x * -> (clojure.lang.PersistentList x)]
              [(IPersistentCollection Any) Any Any * -> (IPersistentCollection Any)]
              ))

clojure.core/find
     (All [x y]
          [(IPersistentMap x y) Any -> (Option (Vector* x y))])

; same as clojure.lang.RT/get
clojure.core/get
     (All [x y]
          (Fn 
            ;no default
            [(IPersistentSet x) Any -> (Option x)]
            [java.util.Map Any -> (Option Any)]
            [String Any -> (Option Character)]
            [nil Any -> nil]
            [(Option (ILookup Any x)) Any -> (Option x)]
            ;default
            [(IPersistentSet x) Any y -> (U y x)]
            [java.util.Map Any y -> (U y Any)]
            [String Any y -> (U y Character)]
            [nil Any y -> y]
            [(Option (ILookup Any x)) Any y -> (U y x)]))

clojure.core/merge 
     (All [k v]
          (Fn [nil * -> nil]
              [(IPersistentMap k v) * -> (IPersistentMap k v)]
              [(Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))]))

;more to be said here?
clojure.core/contains? [(Option (Seqable Any)) Any -> boolean]

clojure.core/= [Any Any * -> (U true false)]


clojure.core/integer? (predicate AnyInteger)
clojure.core/number? (predicate Number)

clojure.core/+ (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number])
clojure.core/- (Fn [AnyInteger AnyInteger * -> AnyInteger]
                        [Number Number * -> Number])
clojure.core/* (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number])
clojure.core// [Number Number * -> Number]

clojure.core/inc (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number])
clojure.core/dec (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number])

clojure.core/even? [AnyInteger -> boolean]
clojure.core/odd? [AnyInteger -> boolean]

clojure.core/take
     (All [x]
       [AnyInteger (Seqable x) -> (LazySeq x)])

clojure.core/cons
     (All [x]
       [x (Option (Seqable x)) -> (ASeq x)])

clojure.core/reverse
     (All [x]
       [(Option (Seqable x)) -> (Seqable x)])

;coercions
clojure.core/bigdec [Any -> BigDecimal]
clojure.core/bigint [Any -> clojure.lang.BigInt]
clojure.core/boolean [Any -> boolean]
clojure.core/byte [Any -> byte]
clojure.core/char [Any -> char]
clojure.core/double [Any -> double]
clojure.core/float [Any -> float]
clojure.core/int [Any -> int]
clojure.core/long [Any -> long]
clojure.core/num [Any -> Number]
clojure.core/short [Any -> short]

;array ctors
clojure.core/int-array (Fn [(U nil Number (Seqable Number)) -> (Array int)]
                                [Number (U Number (Seqable Number)) -> (Array int)])
clojure.core/double-array (Fn [(U nil Number (Seqable Number)) -> (Array double)]
                                   [Number (U Number (Seqable Number)) -> (Array double)])
clojure.core/short-array (Fn [(U nil Number (Seqable Short)) -> (Array short)]
                                  [Number (U Short (Seqable Short)) -> (Array short)])

clojure.core/< [Number Number * -> boolean]

clojure.core/<= [Number Number * -> boolean]

clojure.core/> [Number Number * -> boolean]

clojure.core/>= [Number Number * -> boolean]

clojure.core/ref (All [x] [x -> (clojure.lang.ARef x x)])

;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (predicate (IChunk Any)).
clojure.core/chunked-seq? [Any -> Any]
clojure.core/chunk-first 
     (All [x]
          [(Seqable x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-rest
     (All [x]
          [(Seqable x) -> (Seqable x)])
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

      )
    {'clojure.core/count (count-type)}
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
;                   (Fn [(Array _ x) AnyInteger -> x]
;                       [(Array _ x) AnyInteger b ... b
;                        :recur 
;                        (rec x b ... b)])))
;
;  clojure.core/assoc 
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
;              (Fn [(HMap {l v}) (Vector* k) [v a ... a -> r] a ... a -> (I x (HMap {l r}))]
;                  [(HMap {l r}) (Vector* k b ... b) [v a ... a -> e] a ... a
;                   :recur
;                   [r (Vector* b ... b) [v a ... a -> e] a ... a]])))
;
;  ;clojure.core/get-in 
;  ;     (Label [rec]
;  ;       (All [[x :< (U nil (Associative Any Any))] k :dotted [b]]
;  ;            (Fn [x (Vector*) -> x]
;  ;                [x (Vector*) _ -> x]
;  ;                [(U nil (Associative _ y) (Vector* k b ... b) a -> x
;  ;                ;TODO
;  ;                [(U nil (Associative Any y)) (Vector* k) -> (U nil x)]
;  ;                    ))))
;
;  clojure.core/partial 
;       (Label [rec]
;              (All [x [a :< x] r :dotted [b c]]
;                   (Fn [[x c ... c -> r] a -> [c ... c -> r]]
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
;; Method param annotations

(def init-method-nilable-param-env {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method return annotations

(def init-method-nonnilable-return-env
  (h/method-nonnilable-return-mappings

java.lang.Object/getClass #{0}
clojure.lang.Compiler/munge :all

java.lang.Object/toString :all
java.lang.String/toUpperCase :all
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override annotations

(def init-method-override-env
  (merge
    (h/method-override-mappings

clojure.lang.RT/nth (All [x y]
                         (Fn [(U (Indexed x) (Seqable x)) AnyInteger -> x]
                             [(U (Indexed x) (Seqable x)) AnyInteger y -> (U x y)]))

clojure.lang.Indexed/nth
  (All [x y]
       (Fn [(Indexed x) AnyInteger -> x]
           [(Indexed x) AnyInteger y -> (U x y)]))


;what about combinations of references and primitives?
clojure.lang.RT/box
(All [[x :< (U nil Object)]]
     (Fn [char -> Character]
         [int -> Integer]
         [short -> Short]
         [boolean -> Boolean]
         [byte -> Byte]
         [short -> Short]
         [long -> Long]
         [float -> Float]
         [double -> Double]
         [(U byte short int long) -> AnyInteger]
         [(U float double) -> Number]
         [nil -> nil]
         [x -> x]))

clojure.lang.LockingTransaction/runInTransaction
                 (All [x]
                   [[-> x] -> x])

;array ops
clojure.lang.RT/alength [(ReadOnlyArray Any) -> int]

clojure.lang.RT/aget (All [o]
                        [(ReadOnlyArray o) int -> o])

clojure.lang.RT/aset (All [i o]
                          [(Array2 i o) AnyInteger i -> o])

;get
;same as clojure.core/get
clojure.lang.RT/get (All [x y]
                         (Fn 
                           ;no default
                           [(IPersistentSet x) Any -> (Option x)]
                           [java.util.Map Any -> (Option Any)]
                           [String Any -> (Option Character)]
                           [nil Any -> nil]
                           [(Option (ILookup Any x)) Any -> (Option x)]
                           ;default
                           [(IPersistentSet x) Any y -> (U y x)]
                           [java.util.Map Any y -> (U y Any)]
                           [String Any y -> (U y Character)]
                           [nil Any y -> y]
                           [(Option (ILookup Any x)) Any y -> (U y x)]))

;numbers
clojure.lang.Numbers/add (Fn [AnyInteger AnyInteger -> AnyInteger]
                             [Number Number -> Number])
clojure.lang.Numbers/inc (Fn [AnyInteger -> AnyInteger]
                                              [Number -> Number])
clojure.lang.Numbers/dec (Fn [AnyInteger -> AnyInteger]
                             [Number -> Number])
clojure.lang.Numbers/minus (Fn [AnyInteger AnyInteger -> AnyInteger]
                                                [Number Number -> Number])
clojure.lang.Numbers/multiply (Fn [AnyInteger AnyInteger -> AnyInteger]
                                                   [Number Number -> Number])
clojure.lang.Numbers/divide [Number Number -> Number]

clojure.lang.Numbers/lt [Number Number -> boolean]
clojure.lang.Numbers/lte [Number Number -> boolean]
clojure.lang.Numbers/gt [Number Number -> boolean]
clojure.lang.Numbers/gte [Number Number -> boolean]

clojure.lang.Numbers/isZero [Number -> boolean :filters {:then (is (Value 0) 0)
                                                         :else (! (Value 0) 0)}]
    )
    {'clojure.lang.RT/count (count-type)}))


(def init-ctor-override-env
  (h/ctor-override-mappings

clojure.lang.LazySeq (All [x]
                          [[-> (Option (Seqable x))] -> (LazySeq x)])
    ))
