;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.ann.clojure
  "Type annotations for the base Clojure distribution."
  (:require [#?(:clj clojure.core.typed
                :cljs cljs.core.typed)
             :refer [defalias] :as t]))

(defalias
  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms '[AnyInteger]}
  t/AnyInteger
  #?(:clj (t/U Integer
               Long
               clojure.lang.BigInt
               BigInteger
               Short
               Byte)
     :cljs t/CLJSInteger))

(defalias
  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms '[Int]}
  t/Int
  t/AnyInteger)

(defalias
  ^{:doc "A type that returns true for clojure.core/number?"
    :forms '[Num]}
  t/Num
  #?(:clj Number
     :cljs t/JSNumber))

(defalias
  ^{:doc "A keyword"
    :forms '[Keyword]}
  t/Keyword
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core/Keyword))

(defalias
  ^{:doc "A keyword"
    :forms '[Kw]}
  t/Kw
  t/Keyword)

(defalias
  ^{:doc "A symbol"
    :forms '[Symbol]}
  t/Symbol
  #?(:clj clojure.lang.Symbol
     :cljs cljs.core/Symbol))

(defalias
  ^{:doc "A symbol"
    :forms '[Sym]}
  t/Sym
  t/Symbol)

(defalias
  ^{:doc "A string"
    :forms '[Str]}
  t/Str
  #?(:clj java.lang.String
     :cljs t/JSString))

(defalias
  ^{:doc "A boolean"
    :forms '[Bool]}
  t/Bool
  #?(:clj java.lang.Boolean
     :cljs t/JSBoolean))

(defalias
  ^{:doc "A namespace"
    :forms '[Namespace]}
  t/Namespace
  #?(:clj clojure.lang.Namespace
     :cljs cljs.core/Namespace))

(defalias
  ^{:doc "An atom that can write type w and read type r."
    :forms '[(Atom2 t)]}
  t/Atom2
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]] 
         (clojure.lang.Atom w r)))

(defalias
  ^{:doc "An atom that can read and write type x."
    :forms '[(Atom1 t)]}
  t/Atom1
  (t/TFn [[x :variance :invariant]]
         (t/Atom2 x x)))

(defalias
  ^{:doc "An var that can write type w and read type r."
    :forms '[(Var2 w r)]}
  t/Var2 
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]] 
         #?(:clj (clojure.lang.Var w r)
            :cljs (cljs.core/Var w r))))

(defalias
  ^{:doc "An var that can read and write type x."
    :forms '[(Var1 t)]}
  t/Var1 
  (t/TFn [[x :variance :invariant]] 
         (t/Var2 x x)))

#?(:clj
   (defalias
     ^{:doc "A ref that can write type w and read type r."
       :forms '[(Ref2 w r)]}
     t/Ref2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Ref w r))))

#?(:clj
   (defalias
     ^{:doc "A ref that can read and write type x."
       :forms '[(Ref1 t)]}
     t/Ref1
     (t/TFn [[x :variance :invariant]]
            (t/Ref2 x x))))

#?(:clj
   (defalias
     ^{:doc "An agent that can write type w and read type r."
       :forms '[(Agent2 t t)]}
     t/Agent2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Agent w r))))

#?(:clj
   (defalias
     ^{:doc "An agent that can read and write type x."
       :forms '[(Agent1 t)]}
     t/Agent1
     (t/TFn [[x :variance :invariant]] 
            (t/Agent2 x x))))

(defalias
  ^{:doc "A union of x and nil."
    :forms '[(Option t)]}
  t/Option
  (t/TFn [[x :variance :covariant]] (t/U nil x)))

(defalias
  ^{:doc "A union of x and nil."
    :forms '[(Nilable t)]}
  t/Nilable
  t/Option)

(defalias
  ^{:doc "The identity function at the type level."
    :forms '[Id]}
  t/Id
  (t/TFn [[x :variance :covariant]] x))

(defalias
  ^{:doc "A type that can be used to create a sequence of member type x."
    :forms '[(Seqable t)]}
  t/Seqable
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Seqable x)
            :cljs (cljs.core/ISeqable x))))

(defalias
  ^{:doc "A persistent collection with member type x."
    :forms '[(Coll t)]}
  t/Coll
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentCollection x)
            :cljs (t/I (t/Seqable x)
                       (cljs.core/ICollection x)
                       cljs.core/ICounted
                       cljs.core/IEmptyableCollection
                       cljs.core/IEquiv))))

(defalias
  ^{:doc "The type of all things with count 0. Use as part of an intersection.
         eg. See EmptySeqable."
    :forms '[EmptyCount]}
  t/EmptyCount
  (t/ExactCount 0))

(defalias
  ^{:doc "The type of all things with count greater than 0. Use as part of an intersection.
         eg. See NonEmptySeq"
    :forms '[NonEmptyCount]}
  t/NonEmptyCount
  (t/CountRange 1))

(defalias
  ^{:doc "A persistent collection with member type x and count greater than 0."
    :forms '[(NonEmptyColl t)]}
  t/NonEmptyColl
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Coll x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "An associative persistent collection with members of type m
         and supporting associative operations on keys type k and values type v."
    :forms '[(Associative k v)]}
  t/Associative
  (t/TFn [[m :variance :covariant]
          [k :variance :covariant]
          [v :variance :covariant]]
         #?(:clj (clojure.lang.Associative m k v)
            :cljs (t/I (cljs.core/IAssociative k v)
                       (t/Coll m)
                       (cljs.core/ILookup k v)))))

(defalias
  ^{:doc "A Clojure reversible collection."
    :forms '[(Reversible t)]}
  t/Reversible
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Reversible x)
            :cljs (cljs.core/Reversible x))))

(defalias
  ^{:doc "A persistent vector with member type x."
    :forms '[(Vec t)]}
  t/Vec
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentVector x)
            :cljs (t/I (cljs.core/IVector x)
                       (t/Associative x t/Int x)
                       cljs.core/ISequential
                       (cljs.core/IStack x)
                       (t/Reversible x)
                       (cljs.core/IIndexed x)))))

(defalias
  ^{:doc "A persistent vector with member type x and count greater than 0."
    :forms '[(NonEmptyVec t)]}
  t/NonEmptyVec
  (t/TFn [[x :variance :covariant]]
       (t/I (t/Vec x)
            t/NonEmptyCount)))

(defalias
  ^{:doc "A persistent vector returned from clojure.core/vector (and others)"
    :forms '[(AVec t)]}
  t/AVec
  (t/TFn [[x :variance :covariant]]
         #?(:clj (t/I ; is this type useful enough? c.l.APV implements a lot more
                      (t/Vec x)
                      (java.lang.Iterable x)
                      (java.util.Collection x)
                      (java.util.List x)
                      clojure.lang.IObj)
            :cljs (t/I (t/Vec x)
                       cljs.core/APersistentVector
                       ; TODO cljs equivalent
                       ))))

;;TODO from here, add :cljs equivalents

(defalias
  ^{:doc "A persistent vector returned from clojure.core/vector (and others) and count greater than 0."
    :forms '[(NonEmptyAVec t)]}
  t/NonEmptyAVec
  (t/TFn [[x :variance :covariant]]
       (t/I (t/AVec x)
            t/NonEmptyCount)))

(defalias
  ^{:doc "A non-empty lazy sequence of type t"
    :forms '[(NonEmptyLazySeq t)]}
  t/NonEmptyLazySeq
  (t/TFn [[t :variance :covariant]]
       (t/I (clojure.lang.LazySeq t)
            t/NonEmptyCount)))

(defalias
  ^{:doc "A persistent map with keys k and vals v."
    :forms '[(Map t t)]}
  t/Map
  (t/TFn [[k :variance :covariant]
        [v :variance :covariant]]
       (clojure.lang.IPersistentMap k v)))

(defalias
  ^{:doc "A persistent set with member type x"
    :forms '[(Set t)]}
  t/Set
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IPersistentSet x)))

(defalias
  ^{:doc "A sorted persistent set with member type x"
    :forms '[(SortedSet t)]}
  t/SortedSet
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Set x)
              clojure.lang.Sorted)))

(defalias
  ^{:doc "A type that can be used to create a sequence of member type x
         with count greater than 0."
    :forms '[(NonEmptySeqable t)]}
  t/NonEmptySeqable 
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seqable x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "A type that can be used to create a sequence of member type x
         with count 0."
    :forms '[(EmptySeqable t)]}
  t/EmptySeqable
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seqable x)
              t/EmptyCount)))

(defalias
  ^{:doc "A persistent sequence of member type x."
    :forms '[(Seq t)]}
  t/Seq
  (t/TFn [[x :variance :covariant]]
         (clojure.lang.ISeq x)))

(defalias
  ^{:doc "A persistent sequence of member type x with count greater than 0."
    :forms '[(NonEmptySeq t)]}
  t/NonEmptySeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seq x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "A persistent sequence of member type x with count greater than 0, or nil."
    :forms '[(NilableNonEmptySeq t)]}
  t/NilableNonEmptySeq
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           (t/NonEmptySeq x))))

(defalias
  ^{:doc "A hierarchy for use with derive, isa? etc."
    :forms '[Hierarchy]}
  t/Hierarchy
  '{:parents (t/Map Any Any)
    :ancestors (t/Map Any Any)
    :descendants (t/Map Any Any)})

(defalias
  ^{:doc "A Clojure derefable (see clojure.core/deref)."
    :forms '[(Deref t)]}
  t/Deref
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IDeref x)))

(defalias
  ^{:doc "A Clojure future (see clojure.core/{future-call,future})."
    :forms '[(Future t)]}
  t/Future 
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Deref x)
              (clojure.lang.IBlockingDeref x)
              clojure.lang.IPending
              java.util.concurrent.Future)))

(defalias
  ^{:doc "A Clojure promise (see clojure.core/{promise,deliver})."
    :forms '[(Promise t)]}
  t/Promise 
  (t/TFn [[x :variance :invariant]]
         (t/Rec [p]
                (t/I (t/Deref x)
                     (clojure.lang.IBlockingDeref x)
                     clojure.lang.IPending
                     [x -> (t/U nil p)]))))

(defalias
  ^{:doc "A Clojure delay (see clojure.core/{delay,force})."
    :forms '[(Delay t)]}
  t/Delay
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.Delay x)))

(defalias
  ^{:doc "A Clojure blocking derefable (see clojure.core/deref)."
    :forms '[(BlockingDeref t)]}
  t/BlockingDeref
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IBlockingDeref x)))

(defalias
  ^{:doc "A Clojure persistent list."
    :forms '[(List t)]}
  t/List
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IPersistentList x)))

(defalias
  ^{:doc "A Clojure custom exception type."
    :forms '[ExInfo]}
  t/ExInfo
  (t/I clojure.lang.IExceptionInfo
     RuntimeException))

(defalias
  ^{:doc "A Clojure proxy."
    :forms '[Proxy]}
  t/Proxy
  clojure.lang.IProxy)

; Should c.l.Sorted be parameterised? Is it immutable?
;    ^{:doc "A sorted Clojure collection."
;      :forms '[Sorted]}
;Sorted
;              clojure.lang.Sorted

(defalias
  ^{:doc "A Clojure stack."
    :forms '[(Stack t)]}
  t/Stack
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IPersistentStack x)))

(defalias
  ^{:doc "A sequential collection."
    :forms '[Sequential]}
  t/Sequential
  clojure.lang.Sequential)

(defalias
  ^{:doc "A sequential, seqable collection. Seq's aren't always Sequential."
    :forms '[(SequentialSeqable t)]}
  t/SequentialSeqable
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (t/Seqable x))))

(defalias
  ^{:doc "A Clojure sequential sequence. Seq's aren't always Sequential."
    :forms '[(SequentialSeq t)]}
  t/SequentialSeq
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (clojure.lang.ISeq x))))

(defalias
  ^{:doc "A sequential seq returned from clojure.core/seq"
    :forms '[(ASeq t)]}
  t/ASeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/SequentialSeq x)
              (Iterable x)
              (java.util.Collection x)
              (java.util.List x)
              clojure.lang.IObj)))

(defalias
  ^{:doc "A sequential non-empty seq retured from clojure.core/seq"
    :forms '[(NonEmptyASeq t)]}
  t/NonEmptyASeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/ASeq x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "The result of clojure.core/seq."
    :forms '[(NilableNonEmptyASeq t)]}
  t/NilableNonEmptyASeq
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           (t/NonEmptyASeq x))))

(defalias
  ^{:doc "A type that returns true for clojure.core/fn?"
    :forms '[Fn]}
  t/Fn
  clojure.lang.Fn)

(defalias
  ^{:doc "A Clojure multimethod."
    :forms '[Multi]}
  t/Multi
  clojure.lang.MultiFn)

(defalias
  ^{:doc "A reducer function with accumulator a and reduces over collections of b"
    :forms '[(Reducer a b)]}
  t/Reducer
  (t/TFn [[a :variance :contravariant]
          [b :variance :invariant]]
         (t/IFn 
           ;init
           [:-> b]
           ;complete
           [b :-> b]
           ;step
           [b a :-> (t/U b (clojure.lang.Reduced b))])))

(defalias
  ^{:doc "A transducer function that transforms in to out."
    :forms '[(Transducer in out)]}
  t/Transducer
  (t/TFn [[in :variance :contravariant]
          [out :variance :covariant]]
         (t/All [r]
                [(t/Reducer out r) :-> (t/Reducer in r)])))

;; Predicate support for common classes

(t/rclass-preds
;  clojure.lang.Seqable 
;  {:pred (fn [this a?]
;           (cond 
;             (string? this) (every? a? this)
;             (coll? this) (every? a? this)))}
  clojure.lang.IPersistentCollection
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.ISeq
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IPersistentSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.APersistentSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentHashSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentTreeSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.Associative
  {:args #{2}
   :pred (fn [this a? b?]
           `(cond
              (vector? ~this) (and (every? ~a? (range (count ~this)))
                                   (every? ~b? ~this))
              (map? ~this) (and (every? ~a? (keys ~this))
                                (every? ~b? (vals ~this)))))}
  clojure.lang.IPersistentStack
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IPersistentVector
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.APersistentVector
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentVector
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IMapEntry
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (~a? (key ~this)) (~b? (val ~this))))}
  clojure.lang.AMapEntry
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (~a? (key ~this)) (~b? (val ~this))))}
  clojure.lang.MapEntry
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (~a? (key ~this)) (~b? (val ~this))))}
  clojure.lang.IPersistentMap
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (every? ~a? (keys ~this))
                 (every? ~b? (vals ~this))))}
  clojure.lang.ASeq
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.APersistentMap
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (every? ~a? (keys ~this))
                 (every? ~b? (vals ~this))))}
  clojure.lang.PersistentHashMap
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (every? ~a? (keys ~this))
                 (every? ~b? (vals ~this))))}
  clojure.lang.Cons
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IPersistentList
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentList
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.LazySeq
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.Reduced
  {:args #{1}
   :pred (fn [this a?] 
           `(~a? (deref ~this)))})

;; Var annotations
