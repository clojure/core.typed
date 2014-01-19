(ns clojure.core.typed.base-env
  (:import (clojure.lang Atom Symbol Namespace Keyword Named IMapEntry Seqable
                         LazySeq PersistentHashSet PersistentTreeSet PersistentList APersistentVector
                         APersistentSet Sorted IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap IDeref IBlockingDeref ISeq IMeta ASeq IPersistentCollection
                         ILookup Indexed Associative IPersistentStack PersistentVector Cons
                         IPersistentList IRef IReference AReference ARef Var Delay Reversible
                         ITransientCollection ITransientSet ITransientAssociative ITransientMap
                         ITransientVector PersistentHashMap Reduced))
  (:require [clojure.core.typed.base-env-helper :as h]
            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.fold-default]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.subst]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.current-impl :as impl :refer [v]]
            [clojure.set :as set]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

;; TODO fix metadata representation
;; TODO remove redundant ancestors, add tests to ensure they are preserved.


(delay-and-cache-env init-altered-env
                     (assert (class? Seqable))
  (h/alters

Seqable [[[a :variance :covariant]]
         ]

Reversible [[[a :variance :covariant]]
            ]

IMeta [[[a :variance :covariant]]]

IPersistentCollection [[[a :variance :covariant]
                        ]
                       :replace
                       {Seqable (Seqable a)}]

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
                 IPersistentCollection (IPersistentCollection a)
                 IPersistentSet (IPersistentSet a)}
                :unchecked-ancestors
                #{[Any -> (U a nil)]}
                ]

PersistentHashSet [[[a :variance :covariant]]
                   :replace
                   {Seqable (Seqable a)
                    APersistentSet (APersistentSet a)
                    IPersistentSet (IPersistentSet a)
                    IPersistentCollection (IPersistentCollection a)
                    IMeta (IMeta Any)}
                   :unchecked-ancestors
                   #{[Any -> (U a nil)]}]

PersistentTreeSet [[[a :variance :covariant]]
                   :replace
                   {Seqable (Seqable a)
                    Reversible (Reversible a)
                    APersistentSet (APersistentSet a)
                    IPersistentSet (IPersistentSet a)
                    IPersistentCollection (IPersistentCollection a)
                    IMeta (IMeta Any)}
                    :unchecked-ancestors
                    #{[Any -> (U a nil)]}]

Associative [[[a :variance :covariant]
              [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection Any)
              Seqable (Seqable Any)
              ILookup (ILookup a b)}]

;ITransientCollection [[[w :variance :contravariant]
;                       [r :variance :covariant]]]
;
;ITransientSet [[[w :variance :contravariant]
;                [r :variance :covariant]]
;               :replace
;               {ITransientCollection (ITransientCollection w r)}]
;
;ITransientAssociative [[[wkey :variance :contravariant]
;                        [wval :variance :contravariant]
;                        [rkey :variance :covariant]
;                        [rval :variance :covariant]]
;                       :replace
;                       {ILookup (ILookup rkey rval)
;                        ITransientCollection (ITransientCollection (IMapEntry wkey wval)
;                                                                   (IMapEntry rkey rval))}]
;
;ITransientMap [[[wkey :variance :contravariant]
;                [wval :variance :contravariant]
;                [rkey :variance :covariant]
;                [rval :variance :covariant]]
;               :replace
;               {ILookup (ILookup rkey rval)
;                ITransientAssociative (ITransientAssociative wkey wval rkey rval)
;                ITransientCollection (ITransientCollection (IMapEntry wkey wval)
;                                                           (IMapEntry rkey rval))}]
;
;ATransientMap [[[wkey :variance :contravariant]
;                [wval :variance :contravariant]
;                [rkey :variance :covariant]
;                [rval :variance :covariant]]
;               {;TODO override AFn
;                ILookup (ILookup rkey rval)
;                ITransientAssociative (ITransientAssociative wkey wval rkey rval)
;                ITransientCollection (ITransientCollection (IMapEntry wkey wval)
;                                                           (IMapEntry rkey rval))}]
;
;ITransientVector [[[w :variance :contravariant]
;                   [r :variance :covariant]]
;                  :replace
;                  {ITransientAssociative (ITransientAssociative Number wval Number rval)
;                   ITransientCollection (ITransientCollection w r)
;                   Indexed (Indexed r)}]
;
;IEditableCollection [[c :variance :covariant]]

IPersistentStack [[[a :variance :covariant]]
                  :replace
                  {IPersistentCollection (IPersistentCollection a)
                   Seqable (Seqable a)}]


;define vectors before maps, as HVector is part of map ancestors
IPersistentVector [[[a :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection a)
                    Seqable (Seqable a)
                    Reversible (Reversible a)
                    IPersistentStack (IPersistentStack a)
                    ILookup (ILookup Number a)
                    Associative (Associative Number a)
                    Indexed (Indexed a)}]

APersistentVector [[[a :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection a)
                    Seqable (Seqable a)
                    IPersistentVector (IPersistentVector a)
                    Reversible (Reversible a)
                    IPersistentStack (IPersistentStack a)
                    ILookup (ILookup Number a)
                    Associative (Associative Number a)
                    Indexed (Indexed a)}
                   :unchecked-ancestors
                   #{[Number -> a]}]

PersistentVector [[[a :variance :covariant]]
                  :replace
                  {APersistentVector (APersistentVector a)
                   IPersistentCollection (IPersistentCollection a)
                   Seqable (Seqable a)
                   IPersistentVector (IPersistentVector a)
                   Reversible (Reversible a)
                   IPersistentStack (IPersistentStack a)
                   ILookup (ILookup Number a)
                   IMeta (IMeta Any)
                   Associative (Associative Number a)
                   Indexed (Indexed a)
                   #_IEditableCollection #_(IEditableCollection (ITransientVector a))}]
                  :unchecked-ancestors
                  #{[Number -> a]}]


IPersistentMap [[[a :variance :covariant]
                 [b :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection (U '[a b] (IMapEntry a b)))
                 Seqable (Seqable (U '[a b] (IMapEntry a b)))
                 ILookup (ILookup a b)
                 Associative (Associative a b)}]

ASeq [[[a :variance :covariant]]
      :replace
      {IPersistentCollection (IPersistentCollection a)
       Seqable (Seqable a)
       ISeq (ISeq a)
       IMeta (IMeta Any)}]

APersistentMap [[[a :variance :covariant] 
                 [b :variance :covariant]]
                :replace
                {IPersistentCollection (IPersistentCollection (U '[a b] (IMapEntry a b)))
                 IPersistentMap (IPersistentMap a b)
                 Seqable (Seqable (U '[a b] (IMapEntry a b)))
                 ILookup (ILookup a b)
                 Associative (Associative a b)}
                :unchecked-ancestors
                #{(All [d]
                          (Fn [Any -> (U nil b)]
                              [Any d -> (U b d)]))}]


PersistentHashMap [[[a :variance :covariant] 
                    [b :variance :covariant]]
                   :replace
                   {IPersistentCollection (IPersistentCollection (U '[a b] (IMapEntry a b)))
                    IPersistentMap (IPersistentMap a b)
                    APersistentMap (APersistentMap a b)
                    Seqable (Seqable (U '[a b] (IMapEntry a b)))
                    ILookup (ILookup a b)
                    IMeta (IMeta Any)
                    Associative (Associative a b)
                    #_IEditableCollection #_(IEditableCollection (ITransientMap a b a b))}
                   :unchecked-ancestors
                   #{(All [d]
                             (Fn [Any -> (U nil b)]
                                 [Any d -> (U b d)]))}]

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

Keyword [[]
         :unchecked-ancestors
         #{(All [x] 
                (Fn [(U nil (IPersistentMap Any x)) -> (U nil x)]
                    [Any -> Any]))}]

IDeref [[[r :variance :covariant]]]
IBlockingDeref [[[r :variance :covariant]]]


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
       AReference (AReference Any Any)
       IDeref (IDeref r)
       IReference (IReference w r)}]

Delay [[[r :variance :covariant]]
       :replace
       {IDeref (IDeref r)}]

Var [[[w :variance :contravariant]
      [r :variance :covariant]]
     :replace
     {AReference (AReference Any Any)
      IReference (IReference Any Any)
      IRef (IRef w r)
      ARef (ARef w r)
      IDeref (IDeref r)
      IMeta (IMeta Any)}]

Atom [[[w :variance :contravariant]
       [r :variance :covariant]]
      :replace
      {IRef (IRef w r)
       IMeta (IMeta Any)
       AReference (AReference Any Any)
       ARef (ARef w r)
       IDeref (IDeref r)
       IReference (IReference w r)}]

LazySeq [[[a :variance :covariant]]
         :replace
         {Seqable (Seqable a)
          ISeq (ISeq a)
          IMeta (IMeta Any)
          IPersistentCollection (IPersistentCollection a)}]

Reduced [[[a :variance :covariant]]
         :replace
         {IDeref (IDeref a)}]

; Hack for Seqable things. Not needed if Seqable was a protocol.

java.lang.CharSequence [[]
                        :unchecked-ancestors
                        #{(Seqable Character)}]

;FIXME Need to correctly check ancestors, this shouldn't be necessary because String is a CharSequence
; CTYP-15
java.lang.String [[]
                  :unchecked-ancestors
                  #{(Seqable Character)}]

java.lang.Iterable [[]
                  :unchecked-ancestors
                  #{(Seqable Any)}]
))

(defn reset-rclass-env! []
  (rcls/reset-rclass-env! (init-altered-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial type aliases

; Note: All mappings here *must* be in set c.c.t/-base-aliases

(delay-and-cache-env init-alias-env
  (reset-rclass-env!)
  (h/alias-mappings

  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms [AnyInteger]}
clojure.core.typed/AnyInteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte)

    ^{:doc "A type that returns true for clojure.core/integer?"
      :forms [Int]}
clojure.core.typed/Int (U Integer Long clojure.lang.BigInt BigInteger Short Byte)
      ^{:doc "A type that returns true for clojure.core/number?"
        :forms [Num]}
clojure.core.typed/Num Number
      ^{:doc "A keyword"
        :forms [Keyword]}
clojure.core.typed/Keyword clojure.lang.Keyword
      ^{:doc "A symbol"
        :forms [Symbol]}
clojure.core.typed/Symbol clojure.lang.Symbol

;clojure.core.typed/AnyPrimitive (U char int short boolean byte short long float double)

    ^{:doc "An atom that can read and write type x."
      :forms [(Atom1 t)]}
clojure.core.typed/Atom1 (TFn [[x :variance :invariant]] (Atom x x))
    ^{:doc "An var that can read and write type x."
      :forms [(Var1 t)]}
clojure.core.typed/Var1 (TFn [[x :variance :invariant]] (Var x x))
    ^{:doc "A ref that can read and write type x."
      :forms [(Ref1 t)]}
clojure.core.typed/Ref1 (TFn [[x :variance :invariant]] (IRef x x))

    ^{:doc "A union of x and nil."
      :forms [(Option t)]}
clojure.core.typed/Option (TFn [[x :variance :covariant]] (U nil x))

    ^{:doc "A union of x and nil."
      :forms [(Nilable t)]}
clojure.core.typed/Nilable (TFn [[x :variance :covariant]] (U nil x))

      ^{:doc "The identity function at the type level."
        :forms [Id]}
clojure.core.typed/Id (TFn [[x :variance :covariant]] x)

      ^{:doc "A persistent collection with member type x."
        :forms [(Coll t)]}
clojure.core.typed/Coll (TFn [[x :variance :covariant]]
                             (IPersistentCollection x))
    ^{:doc "A persistent collection with member type x and count greater than 0."
      :forms [(NonEmptyColl t)]}
clojure.core.typed/NonEmptyColl (TFn [[x :variance :covariant]]
                                      (I (IPersistentCollection x) (CountRange 1)))
    ^{:doc "A persistent vector with member type x."
      :forms [(Vec t)]}
clojure.core.typed/Vec (TFn [[x :variance :covariant]]
                            (IPersistentVector x))
    ^{:doc "A persistent vector with member type x and count greater than 0."
      :forms [(NonEmptyVec t)]}
clojure.core.typed/NonEmptyVec (TFn [[x :variance :covariant]]
                                     (I (IPersistentVector x) (CountRange 1)))
    ^{:doc "A non-empty lazy sequence of type t"
      :forms [(NonEmptyLazySeq t)]}
clojure.core.typed/NonEmptyLazySeq (TFn [[t :variance :covariant]]
                                        (I (LazySeq t) (CountRange 1)))
    ^{:doc "A persistent map with keys k and vals v."
      :forms [(Map t t)]}
clojure.core.typed/Map (TFn [[k :variance :covariant]
                             [v :variance :covariant]]
                            (IPersistentMap k v))
    ^{:doc "A persistent set with member type x"
      :forms [(Set t)]}
clojure.core.typed/Set (TFn [[x :variance :covariant]]
                            (IPersistentSet x))
    ^{:doc "A sorted persistent set with member type x"
      :forms [(SortedSet t)]}
clojure.core.typed/SortedSet (TFn [[x :variance :covariant]]
                               (Extends [(IPersistentSet x) Sorted]))
    ^{:doc "A type that can be used to create a sequence of member type x."
      :forms [(Seqable t)]}
clojure.core.typed/Seqable (TFn [[x :variance :covariant]]
                                (Seqable x))
    ^{:doc "A type that can be used to create a sequence of member type x
with count greater than 0."
      :forms [(NonEmptySeqable t)]}

clojure.core.typed/NonEmptySeqable (TFn [[x :variance :covariant]]
                                         (I (Seqable x) (CountRange 1)))
    ^{:doc "A type that can be used to create a sequence of member type x
with count 0."
      :forms [(EmptySeqable t)]}
clojure.core.typed/EmptySeqable (TFn [[x :variance :covariant]]
                                  (I (Seqable x) (ExactCount 0)))
      ^{:doc "A persistent sequence of member type x."
        :forms [(Seq t)]}
clojure.core.typed/Seq (TFn [[x :variance :covariant]]
                            (ISeq x))

    ^{:doc "A persistent sequence of member type x with count greater than 0."
      :forms [(NonEmptySeq t)]}
clojure.core.typed/NonEmptySeq (TFn [[x :variance :covariant]]
                                     (I (ISeq x) (CountRange 1)))

    ^{:doc "A persistent sequence of member type x with count greater than 0, or nil."
      :forms [(NilableNonEmptySeq t)]}
clojure.core.typed/NilableNonEmptySeq (TFn [[x :variance :covariant]]
                                         (U nil (I (ISeq x) (CountRange 1))))

    ^{:doc "The type of all things with count 0. Use as part of an intersection.
eg. See EmptySeqable."
      :forms [EmptyCount]}

clojure.core.typed/EmptyCount (ExactCount 0)
    ^{:doc "The type of all things with count greater than 0. Use as part of an intersection.
eg. See NonEmptySeq"
      :forms [NonEmptyCount]}
clojure.core.typed/NonEmptyCount (CountRange 1)

    ^{:doc "A hierarchy for use with derive, isa? etc."
      :forms [Hierarchy]}
clojure.core.typed/Hierarchy '{:parents (IPersistentMap Any Any)
                               :ancestors (IPersistentMap Any Any)
                               :descendants (IPersistentMap Any Any)}

    ^{:doc "A Clojure future (see clojure.core/{future-call,future})."
      :forms [(Future x)]}
clojure.core.typed/Future 
                      (TFn [[x :variance :covariant]]
                       (Extends [(IDeref x)
                                 (IBlockingDeref x)
                                 clojure.lang.IPending
                                 java.util.concurrent.Future]))

    ^{:doc "A Clojure promise (see clojure.core/{promise,deliver})."
      :forms [(Promise x)]}
clojure.core.typed/Promise 
              (TFn [[x :variance :invariant]]
               (Rec [p]
                (I (Extends [(IDeref x)
                             (IBlockingDeref x)
                             clojure.lang.IPending])
                   [x -> (U nil p)])))
    ))

(defn reset-alias-env! []
  (let [alias-env (init-alias-env)]
    ; Ensure init-alias-env agrees with the -base-aliases
    (assert (= (set (keys alias-env))
               (set (map #(symbol "clojure.core.typed" (str %))
                         clojure.core.typed/-base-aliases)))
            (str "core.typed Bug! Base aliases do not agree with base environment."
                 " Missing from core.typed ns: "
                 (set/difference (set (keys alias-env))
                                 (set (map #(symbol "clojure.core.typed" (str %))
                                           clojure.core.typed/-base-aliases)))
                 " Missing from base-env ns "
                 (set/difference (set (map #(symbol "clojure.core.typed" (str %))
                                           clojure.core.typed/-base-aliases))
                                 (set (keys alias-env)))))
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
(let [interns '[Option AnyInteger Id Coll Seq NonEmptySeq EmptySeqable
                NonEmptySeqable Map EmptyCount NonEmptyCount SortedSet Set
                Vec NonEmptyColl NonEmptyLazySeq NilableNonEmptySeq
                Hierarchy Nilable Int Var1]]
  (when (some resolve interns)
    (doseq [i interns]
      (ns-unmap *ns* i)))
  (refer 'clojure.core.typed :only interns))

(defn ^:private count-type []
  (impl/with-clojure-impl
    (r/make-FnIntersection
      (r/make-Function 
        [(c/Un r/-nil (c/RClass-of Seqable [r/-any]) (c/RClass-of clojure.lang.Counted))]
        (prs/parse-type '(U java.lang.Integer java.lang.Long))
        nil nil
        :object (obj/->Path [(pe/->CountPE)] 0)))))

(delay-and-cache-env ^:private init-var-env
  (reset-alias-env!)
  (merge
    (h/var-mappings

clojure.core.typed/check-ns (Fn [Symbol -> Any]
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
clojure.core.typed/ann-record* [Any Any Any -> Any]
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
; should always be special cased
;clojure.core.typed/var>* [Any -> (Var Any)]

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
                        [(U Symbol String) -> Symbol])
clojure.core/intern (Fn [(U Symbol Namespace) Symbol -> (Var Nothing Any)]
                        [(U Symbol Namespace) Symbol Any -> (Var Nothing Any)])


clojure.core/doall (All [[c :< (U nil (Seqable Any))]]
                     (Fn [c -> c]
                         [AnyInteger c -> c]))
clojure.core/dorun (Fn [(U nil (Seqable Any)) -> nil]
                       [AnyInteger (U nil (Seqable Any)) -> nil])
clojure.core/iterate (All [x]
                       [[x -> x] x -> (Seq x)])
clojure.core/memoize (All [x y ...]
                            [[y ... y -> x] -> [y ... y -> x]])

clojure.core/key (All [x]
                           [(IMapEntry x Any) -> x])

;TODO flip filters
clojure.core/complement (All [x] [[x -> Any] -> [x -> boolean]])
; should preserve filters
clojure.core/boolean [Any -> boolean]

;clojure.core/filter (All [x y]
;                           (Fn
;                             [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seq (I x y))]
;                             [[x -> Any] (Option (Seqable x)) -> (Seq x)]))
clojure.core/filter (All [x y]
                           (Fn
                             [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seq y)]
                             [[x -> Any :filters {:then (! y 0)}] (Option (Seqable x)) -> (Seq (I x (Not y)))]
                             [[x -> Any] (Option (Seqable x)) -> (Seq x)]))
;clojure.core/filterv (All [x y]
;                          (Fn
;                            [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (APersistentVector (I x y))]
;                            [[x -> Any] (Option (Seqable x)) -> (APersistentVector x)]))
clojure.core/filterv (All [x y]
                          (Fn
                            [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (APersistentVector y)]
                            [[x -> Any] (Option (Seqable x)) -> (APersistentVector x)]))
;clojure.core/remove (All [x y]
;                           (Fn
;                             [[x -> Any :filters {:else (is y 0)}] (Option (Seqable x)) -> (Seq (I x y))]
;                             [[x -> Any] (Option (Seqable x)) -> (Seq x)]
;                             ))
clojure.core/remove (All [x y]
                           (Fn
                             [[x -> Any :filters {:else (is y 0)}] (Option (Seqable x)) -> (Seq y)]
                             [[x -> Any] (Option (Seqable x)) -> (Seq x)]
                             ))


clojure.core/take-while (All [x y]
                               (Fn 
                                 [[x -> Any :filters {:then (is y 0)}] (Option (Seqable x)) -> (Seq y)]
                                 [[x -> Any] (Option (Seqable x)) -> (Seq x)]))
clojure.core/drop-while (All [x]
                               [[x -> Any] (Option (Seqable x)) -> (Seq x)])

clojure.core/split-with 
     (All [x y z] 
       (Fn
         [[x -> Any :filters {:then (is y 0), :else (is z 0)}] (Option (Seqable x)) -> '[(Seq y) (Seq z)]]
         [[x -> Any] (Option (Seqable x)) -> '[(Seq x) (Seq x)]]))

clojure.core/split-at
     (All [x y z] 
          [AnyInteger (Option (Seqable x)) -> '[(Seq x) (Seq x)]])

clojure.core/partition-all (All [x] 
                             (Fn [Int (Nilable (Seqable x)) -> (Seq (Seq x))] 
                                 [Int Int (Nilable (Seqable x)) -> (Seq (Seq x))]))

clojure.core/repeatedly 
     (All [x]
          (Fn [[-> x] -> (Seq x)]
              [AnyInteger [-> x] -> (Seq x)]))


clojure.core/some (All [x y] [[x -> y] (Option (Seqable x)) -> (Option y)])

clojure.core/some-fn [[Any -> Any] [Any -> Any] * -> [Any -> Any]]
clojure.core/every-pred [[Any -> Any] [Any -> Any] * -> [Any -> Any]]

clojure.core/concat (All [x] [(Option (Seqable x)) * -> (Seq x)])

clojure.core/set (All [x] [(Option (Seqable x)) -> (PersistentHashSet x)])
clojure.core/hash-set (All [x] [x * -> (PersistentHashSet x)])
clojure.core/sorted-set (All [x] [x * -> (PersistentTreeSet x)])
clojure.core/sorted-set-by (All [x] [[x x -> AnyInteger] x * -> (PersistentTreeSet x)])
clojure.core/list (All [x] [x * -> (PersistentList x)])
clojure.core/vector (All [x y z a b c] 
                         (Fn
                           [-> '[]]
                           [x -> '[x]]
                           [x y -> '[x y]]
                           [x y z -> '[x y z]]
                           [x y z a -> '[x y z a]]
                           [x y z a b -> '[x y z a b]]
                           [x y z a b c -> '[x y z a b c]]
                           [x * -> (APersistentVector x)]))
clojure.core/vec (All [x] [(Option (Seqable x)) -> (APersistentVector x)])

clojure.core/not [Any -> boolean]
clojure.core/constantly (All [x y] [x -> [y * -> x]])

clojure.core/bound? [(Var Nothing Any) * -> Boolean]
clojure.core/thread-bound? [(Var Nothing Any) * -> Boolean]
clojure.core/bases [(Nilable Class) -> (NilableNonEmptySeq Class)]

clojure.core/make-hierarchy [-> Hierarchy]
clojure.core/isa? (Fn [Any Any -> Boolean]
                      [Hierarchy Any Any -> Boolean])

;TODO make extensible via IPersisentSet
clojure.core/disj
     (All [x]
          (Fn [(SortedSet x) Any Any * -> (SortedSet x)]
              [(Set x) Any Any * -> (Set x)]))

;TODO make extensible via IPersistentMap
clojure.core/assoc
     (All [b c d]
       (Fn [(Map b c) b c -> (Map b c)]
           [(Vec d) AnyInteger d -> (Vec d)]))

clojure.core/dissoc
     (All [k v]
       (Fn [(Map k v) Any * -> (Map k v)]))

clojure.core/zipmap
     (All [k v]
       [(U nil (Seqable k)) (U nil (Seqable v)) -> (APersistentMap k v)])

clojure.core/keys
(All [k]
     [(Map k Any) -> (Seq k) :object {:id 0 :path [Keys]}])

clojure.core/vals
(All [v]
     [(Map Any v) -> (Seq v) :object {:id 0 :path [Vals]}])

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
clojure.core/flush [-> nil]


clojure.core/format [String Any * -> String]


clojure.core/re-matcher [java.util.regex.Pattern String -> java.util.regex.Matcher]
clojure.core/re-groups [java.util.regex.Matcher -> (U nil String (Vec (Option String)))]
clojure.core/re-find (Fn [java.util.regex.Matcher -> (U nil String (Vec (Option String)))]
                              [java.util.regex.Pattern String -> (U nil String (Vec (Option String)))])
clojure.core/re-seq [java.util.regex.Pattern String -> (Seq (U nil String (Vec (Option String))))]

clojure.core/subs (Fn [String AnyInteger -> String]
                           [String AnyInteger AnyInteger -> String])

clojure.core/future-call (All [x] [[-> x] -> (Extends [(IDeref x)
                                                       java.util.concurrent.Future])])

clojure.core/atom (All [x]
                       [x & :optional {:validator (U nil [x -> Any]) :meta Any}-> (Atom x x)])

clojure.core/set-validator! (All [x]
                                 [(clojure.lang.IRef Any x) [x -> Any] -> nil])

clojure.core/deref (All [x y]
                     (Fn [(IDeref x) -> x]
                         [(U (IDeref Any) java.util.concurrent.Future) -> Any]
                         [(IBlockingDeref x) AnyInteger y -> (U x y)]
                         [(U java.util.concurrent.Future (IBlockingDeref Any)) AnyInteger Any -> Any]))

clojure.core/delay? (predicate (Delay Any))

clojure.core/force (All [x]
                        (Fn [(Delay x) -> x]
                            [x -> x]))

clojure.core/reset! (All [w r]
                              [(Atom w r) w -> w])

clojure.core/swap! (All [w r b ...] 
                             [(Atom w r) [r b ... b -> w] b ... b -> w])

clojure.core/alter-var-root (All [w r b ...] 
                              [(Var w r) [r b ... b -> w] b ... b -> w])

clojure.core/fnil (All [x y z a b ...]
                    (Fn [[x b ... b -> a] x -> [(U nil x) b ... b -> a]]
                        [[x y b ... b -> a] x y -> [(U nil x) (U nil y) b ... b -> a]]
                        [[x y z b ... b -> a] x y z -> [(U nil x) (U nil y) (U nil z) b ... b -> a]]))

clojure.core/symbol
     (Fn [(U Symbol String) -> Symbol]
         [String String -> Symbol])

clojure.core/keyword
     (Fn [(U Keyword Symbol String) -> Keyword]
         [String String -> Keyword])

clojure.core/find-keyword
     (Fn [(U Keyword Symbol String) -> (Option Keyword)]
         [String String -> (Option Keyword)])

clojure.core/derive (Fn [(U Symbol Keyword Class) (U Symbol Keyword) -> nil]
                        [Hierarchy (U Symbol Keyword Class) (U Symbol Keyword) -> Hierarchy])

clojure.core/compare [Comparable Any -> Number]

clojure.core/require [Any * -> nil]

clojure.core/seq? (predicate (Seq Any))
clojure.core/set? (predicate (Set Any))
clojure.core/vector? (predicate (Vec Any))
clojure.core/nil? (predicate nil)
clojure.core/false? (predicate false)
clojure.core/true? (predicate true)
clojure.core/zero? (predicate (Value 0))
clojure.core/symbol? (predicate Symbol)
clojure.core/keyword? (predicate Keyword)
clojure.core/map? (predicate (Map Any Any))
)
    (h/var-mappings

clojure.core/coll? (predicate (Coll Any))
clojure.core/meta (All [x]
                            (Fn [(IMeta x) -> x]
                                [Any -> nil]))
clojure.core/with-meta (All [[x :< clojure.lang.IObj] y]
                              [x y -> (I x (IMeta y))])

clojure.core/string? (predicate String)
clojure.core/char? (predicate Character)

clojure.string/split
     (Fn [String java.util.regex.Pattern -> (APersistentVector String)]
         [String java.util.regex.Pattern AnyInteger -> (APersistentVector String)])

clojure.string/join
     (Fn [(Option (Seqable Any)) -> String]
         [Any (Option (Seqable Any)) -> String])

clojure.core/interpose (All [x] (Fn [x (Option (Seqable x)) -> (Seq x)]))
clojure.core/interleave (All [x] [(Option (Seqable x)) (Option (Seqable x)) (Option (Seqable x)) * -> (Seq x)])

clojure.core/repeat (All [x] 
                         (Fn [x -> (Seq x)]
                             [AnyInteger x -> (Seq x)]))

;clojure.core/every? (All [x y] 
;                         (Fn [[x -> Any :filters {:then (is y 0)}] (Coll x) -> Boolean
;                              :filters {:then (is (Coll (I x y)) 1)}]
;                             ; argument could be nil
;                             [[x -> Any :filters {:then (is y 0)}] (U nil (Coll x)) -> Boolean
;                              :filters {:then (is (U nil (Coll (I x y))) 1)}]
;                             [[x -> Any] (U nil (Seqable x)) -> Boolean]))
clojure.core/every? (All [x y]
                         (Fn [[x -> Any :filters {:then (is y 0)}] (Coll x) -> Boolean
                              :filters {:then (is (Coll y) 1)}]
                             ; argument could be nil
                             [[x -> Any :filters {:then (is y 0)}] (U nil (Coll x)) -> Boolean
                              :filters {:then (is (U nil (Coll y)) 1)}]
                             [[x -> Any] (U nil (Seqable x)) -> Boolean]))

clojure.core/range
(Fn [-> (Seq AnyInteger)]
    [Number -> (Seq AnyInteger)]
    [AnyInteger Number -> (Seq AnyInteger)]
    [Number Number -> (Seq Number)]
    [AnyInteger Number AnyInteger -> (Seq AnyInteger)]
    [Number Number Number -> (Seq Number)])

clojure.core/class (Fn [nil -> nil :object {:id 0 :path [Class]}]
                            [Object -> Class :object {:id 0 :path [Class]}]
                            [Any -> (Option Class) :object {:id 0 :path [Class]}])

; need better metadata support if this even has a chance of working
; like class
clojure.core/type [Any -> Any]

clojure.core/seq (All [x]
                        (Fn 
                          [(NonEmptyColl x) -> (NonEmptySeq x)]
                          [(Option (Coll x)) -> (Option (NonEmptySeq x))
                           :filters {:then (& (is NonEmptyCount 0)
                                              (! nil 0))
                                     :else (| (is nil 0)
                                              (is EmptyCount 0))}]
                          [(Option (Seqable x)) -> (Option (NonEmptySeq x))]))

; Seqable [[x :variance :covariant]
;          :count [l :variance :covariant :< AnyCountRange]
;          :to-seq [sfn :kind (TFn [[x :variance :covariant]]
;                               (I IWithMeta (IMeta nil) (ISeq x) (ICollection x) 
;                                  IEmptyableCollection ISequential))]]

; clojure.core/seq (All [x
;                        [sfn :kind [* -> *]]
;                    (Fn
;                      [(Seqable x :count (CountRange 1) :to-seq sfn) -> (sfn x)]
;                      [(Seqable x :count AnyCountRange :to-seq sfn) -> (U nil (sfn x))]

clojure.core/empty? (Fn [(Option (Coll Any)) -> boolean
                          :filters {:then (| (is EmptyCount 0)
                                             (is nil 0))
                                    :else (is NonEmptyCount 0)}]
                        [(Option (Seqable Any)) -> boolean])

clojure.core/map
     (All [c a b ...]
          (Fn [[a b ... b -> c] (NonEmptySeqable a) (NonEmptySeqable b) ... b -> (NonEmptySeq c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (Seq c)]))

clojure.core/mapv
     (All [c a b ...]
          [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (APersistentVector c)])

clojure.core/mapcat
     (All [c b ...]
          [[b ... b -> (Option (Seqable c))] (Option (Seqable b)) ... b -> (Seq c)])

clojure.core/map-indexed
     (All [x y] [[AnyInteger x -> y] (Option (Seqable x)) -> (Seqable y)])

clojure.core/merge-with
     (All [k v]
          (Fn [[v v -> v] nil * -> nil]
              [[v v -> v] (Map k v) * -> (Map k v)]
              [[v v -> v] (Option (Map k v)) * -> (Option (Map k v))]))

clojure.core/reduce
     (All [a c]
          (Fn 
            ;Without accumulator
            ; default
            ; (reduce + my-coll)
            [[a c -> (U (Reduced a) a)] (NonEmptySeqable c) -> a]
            [(Fn [a c -> (U (Reduced a) a)] [-> (U (Reduced a) a)]) (Option (Seqable c)) -> a]
            ; default
            ; (reduce + 3 my-coll)
            [[a c -> (U (Reduced a) a)] a (Option (Seqable c)) -> a]))

clojure.core/reduced (All [x] [x -> (Reduced x)])
clojure.core/reduced? (predicate (Reduced Any))

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
          (Fn [(U (IMapEntry x Any) '[x Any *]) -> x]
              [(Option (EmptySeqable x)) -> nil]
              [(NonEmptySeqable x) -> x]
              [(Option (Seqable x)) -> (Option x)]))

clojure.core/second
     (All [x]
          (Fn [(U (IMapEntry Any x) '[Any x Any *]) -> x]
              [(Option (I (Seqable x) (CountRange 0 1))) -> nil]
              [(I (Seqable x) (CountRange 2)) -> x]
              [(Option (Seqable x)) -> (Option x)]))

clojure.core/ffirst
     (All [x]
          [(Option (Seqable (U nil (Seqable x)))) -> (Option x)])

clojure.core/nfirst
(All [x]
     [(Option (Seqable (Option (Seqable x)))) -> (Option (NonEmptySeq x))])

clojure.core/fnext
(All [x]
     [(Option (Seqable (Option (Seqable x)))) -> (Option x)])

clojure.core/nnext
(All [x]
     [(Option (Seqable x)) -> (Option (NonEmptySeq x))])

clojure.core/nthnext
(All [x]
     [(Option (Seqable x)) AnyInteger -> (Option (NonEmptySeq x))])

clojure.core/rest
     (All [x]
          [(Option (Seqable x)) -> (Seq x)])

clojure.core/last
     (All [x]
          [(Option (Seqable x)) -> (U nil x)])

clojure.core/butlast
     (All [x]
          [(Option (Seqable x)) -> (Seq x)])

clojure.core/next
     (All [x]
          (Fn [(Option (Coll x)) -> (Option (NonEmptySeq x))
               :filters {:then (& (is (CountRange 2) 0)
                                  (! nil 0))
                         :else (| (is (CountRange 0 1) 0)
                                  (is nil 0))}]
              [(Option (Seqable x)) -> (Option (NonEmptySeq x))]))

clojure.core/into
      (All [x y]
           (Fn [(IPersistentMap x y) (U nil (Seqable (U nil (IMapEntry x y) '[x y]))) -> (IPersistentMap x y)]
               [(IPersistentVector x) (U nil (Seqable x)) -> (IPersistentVector x)]
               [(IPersistentSet x) (U nil (Seqable x)) -> (IPersistentSet x)]))

clojure.core/conj
;     (All [e
;           [Arg :< (TFn [[x :variance :covariant]] Any)]
;           [Res :< (TFn [[x :variance :covariant]]
;                     (Coll Any))]]
;          (Fn [(clojure.lang.IPersistentCollection e Arg Res) (Arg e) (Arg e) * -> (Res e)]
;              [nil e e * -> (clojure.lang.PersistentList e)]))


     (All [x y]
          (Fn [(IPersistentVector x) x x * -> (IPersistentVector x)]
              [(APersistentMap x y)
               (U nil (IMapEntry x y) (Vector* x y))
               (U nil (IMapEntry x y) (Vector* x y)) * -> (APersistentMap x y)]
              [(IPersistentMap x y)
               (U nil (IMapEntry x y) (Vector* x y))
               (U nil (IMapEntry x y) (Vector* x y)) * -> (IPersistentMap x y)]
              [(IPersistentSet x) x x * -> (IPersistentSet x)]
              [(Seq x) x x * -> (ASeq x)]
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
          [(IPersistentMap x y) Any -> (Option (Vector* x y))])

; same as clojure.lang.RT/get
clojure.core/get
     (All [x y]
          (Fn 
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
            [String Any y -> (U y Character)]
            ))

;FIXME maps after the first can always be nil
clojure.core/merge 
     (All [k v]
          (Fn [nil * -> nil]
              [(IPersistentMap k v) (IPersistentMap k v) * -> (IPersistentMap k v)]
              [(Option (IPersistentMap k v)) * -> (Option (IPersistentMap k v))]))

;more to be said here?
clojure.core/contains? [(Option (Seqable Any)) Any -> boolean]

clojure.core/= [Any Any * -> (U true false)]


clojure.core/integer? (predicate AnyInteger)
clojure.core/number? (predicate Number)
clojure.core/var? (predicate (clojure.lang.Var Nothing Any))
clojure.core/class? (predicate Class)

clojure.core/resolve (Fn [Symbol -> (U (Var Nothing Any) Class nil)]
                         ; should &env arg be more accurate?
                         [Any Symbol -> (U (Var Nothing Any) Class nil)])

clojure.core/ns-resolve (Fn [(U Symbol Namespace) Symbol -> (U (Var Nothing Any) Class nil)]
                            ; should &env arg be more accurate?
                            [(U Symbol Namespace) Any Symbol -> (U (Var Nothing Any) Class nil)])

clojure.core/extenders [Any -> (U nil (Seqable (U Class nil)))]

clojure.core/+ (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number])
clojure.core/- (Fn [AnyInteger AnyInteger * -> AnyInteger]
                   [Number Number * -> Number])
clojure.core/* (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number])
clojure.core// [Number Number * -> Number]

clojure.core/+' (Fn [AnyInteger * -> AnyInteger]
                    [Number * -> Number])
clojure.core/-' (Fn [AnyInteger AnyInteger * -> AnyInteger]
                    [Number Number * -> Number])
clojure.core/*' (Fn [AnyInteger * -> AnyInteger]
                    [Number * -> Number])

clojure.core/inc (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number])
clojure.core/dec (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number])

clojure.core/inc' (Fn [AnyInteger -> AnyInteger]
                          [Number -> Number])

clojure.core/even? [AnyInteger -> boolean]
clojure.core/odd? [AnyInteger -> boolean]

clojure.core/take
     (All [x]
       [AnyInteger (Seqable x) -> (Seq x)])

clojure.core/cons
     (All [x]
       [x (Option (Seqable x)) -> (ASeq x)])

clojure.core/reverse
     (All [x]
       [(Option (Seqable x)) -> (Seqable x)])

clojure.core/rseq
     (All [x]
       [(Reversible x) -> (Option (NonEmptySeq x))])

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

clojure.core/char-array (Fn [(U nil Number (Seqable Character)) -> (Array char)]
                            [Number (U Number (Seqable Character)) -> (Array char)])



clojure.core/< [Number Number * -> boolean]

clojure.core/<= [Number Number * -> boolean]

clojure.core/> [Number Number * -> boolean]

clojure.core/>= [Number Number * -> boolean]

clojure.core/== [Number Number * -> boolean]

clojure.core/max [Number Number * -> Number]
clojure.core/min [Number Number * -> Number]

clojure.core/ref (All [x] [x -> (clojure.lang.ARef x x)])

clojure.core/rand (Fn [-> Number]
                      [Number -> Number])

clojure.core/rand-int [Int -> Int]

clojure.core/ex-info (Fn [String (Map Any Any) -> clojure.lang.ExceptionInfo]
                         [String (Map Any Any) (U nil Throwable) -> clojure.lang.ExceptionInfo])


;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (predicate (IChunk Any)).
clojure.core/chunked-seq? [Any -> Any]
clojure.core/chunk-first 
     (All [x]
          ;should be IChunkedSeq -> IChunk
          [(Seqable x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-rest
     (All [x]
          ;should be IChunkRest -> Seq
          [(clojure.lang.Seqable x) -> (Seq x)])
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
                     (Fn [(IPersistentVector x) AnyInteger -> (IPersistentVector x)]
                         [(IPersistentVector x) AnyInteger AnyInteger -> (IPersistentVector x)]))

clojure.core/alias [Symbol Symbol -> nil]
clojure.core/all-ns [-> (Coll Namespace)]


;; math.numeric-tower

clojure.math.numeric-tower/floor
(Fn [AnyInteger -> AnyInteger]
    [Number -> Number])

clojure.math.numeric-tower/abs
(Fn [AnyInteger -> AnyInteger]
    [Number -> Number])

;; core.match

clojure.core.match/backtrack Exception

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
    (h/method-override-mappings

clojure.lang.RT/nth (All [x y]
                         (Fn [(U (Indexed x) (Seqable x)) AnyInteger -> x]
                             [(U (Indexed x) (Seqable x) nil) AnyInteger y -> (U x y)]
                             [(U (Indexed x) (Seqable x) nil) AnyInteger -> (U x nil)]))

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

clojure.lang.RT/booleanCast [Any -> boolean]

clojure.lang.Numbers/char_array (Fn [(U nil Number (Seqable Character)) -> (Array char)]
                                    [Number (U Number (Seqable Character)) -> (Array char)])


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
clojure.lang.Numbers/add (Fn [AnyInteger AnyInteger -> AnyInteger]
                             [Number Number -> Number])
clojure.lang.Numbers/inc (Fn [AnyInteger -> AnyInteger]
                                              [Number -> Number])
clojure.lang.Numbers/dec (Fn [AnyInteger -> AnyInteger]
                             [Number -> Number])
clojure.lang.Numbers/minus (Fn 
                             [AnyInteger -> AnyInteger]
                             [Number -> Number]
                             [AnyInteger AnyInteger -> AnyInteger]
                             [Number Number -> Number])
clojure.lang.Numbers/multiply (Fn [AnyInteger AnyInteger -> AnyInteger]
                                  [Number Number -> Number])
clojure.lang.Numbers/divide [Number Number -> Number]

clojure.lang.Numbers/max [Number Number * -> Number]
clojure.lang.Numbers/min [Number Number * -> Number]


clojure.lang.Numbers/lt [Number Number -> boolean]
clojure.lang.Numbers/lte [Number Number -> boolean]
clojure.lang.Numbers/gt [Number Number -> boolean]
clojure.lang.Numbers/gte [Number Number -> boolean]

clojure.lang.Numbers/isZero (predicate (Value 0))

clojure.lang.Util/compare [Any Any -> Number]
    )
    {'clojure.lang.RT/count (count-type)}))

(comment
  clojure.lang.IFn/invoke (All [r a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 arest]
                               (Fn
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
                        [[-> x] -> (Delay x)])
    ))

(delay-and-cache-env ^:private init-protocol-env {})

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
    (reset-rclass-env!)
    ((v 'clojure.core.typed.declared-kind-env/reset-declared-kinds!) 
     (init-declared-kinds))
    ((v 'clojure.core.typed.datatype-env/reset-datatype-env!) 
     (init-datatype-env))
    ((v 'clojure.core.typed.datatype-ancestor-env/reset-datatype-ancestors!)
     (init-datatype-ancestor-env)))
  nil)
