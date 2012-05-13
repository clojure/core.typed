(ns typed.types
  (:require [typed.core :refer [typed-ns def-type-alias def-new-type]])
  (:import (clojure.lang IPersistentCollection IPersistentStack APersistentVector
                         IMeta IObj IDeref IReference APersistentMap)))

(def Any typed.core/Any)
(def Nothing typed.core/Nothing)

;numbers markers
(def-type-alias AnyInteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte))
(def-type-alias AnyRatio clojure.lang.Ratio)
(def-type-alias AnyDecimal BigDecimal)
(def-type-alias AnyRational (U AnyInteger AnyRatio AnyDecimal))
(def-type-alias AnyFloat (U Double Float))
(def-type-alias AnyNumber Number)

(def-type-alias Var clojure.lang.Var)

;metadata types
(def-type-alias (Meta a)
                (I IMeta
                   IObj))

;parameterised aliases
(def-type-alias (Deref a)
                IDeref)
(def-type-alias (Reference a)
                (I IReference
                   (Meta a)))

;reference types
(def-type-alias Namespace
                (I clojure.lang.Namespace
                   #_(Reference Any)))

(def-type-alias (Atom a)
                (I clojure.lang.Atom
                   (Deref a)))

(def-type-alias NonseqableSeqable
                (U Iterable
                   ;array
                   CharSequence
                   java.util.Map))

(def-type-alias (Seqable a)
                (U clojure.lang.Seqable
                   NonseqableSeqable))

(def-type-alias (NilSeqable a)
                (U (Seqable a)
                   nil))

(def-type-alias (Coll a)
                (I IPersistentCollection
                   (Seqable a)))

(def-type-alias (Seq a)
                (I clojure.lang.ASeq
                   (Seqable a)
                   (Coll a)))

(def-type-alias (NilSeq a)
                (U (Seq a)
                   nil))

(def-type-alias (MapEntry a b)
                (I clojure.lang.AMapEntry
                   (Seqable (U a b))))

(def-type-alias (Map a b)
                (I APersistentMap
                   (Seqable (MapEntry a b))
                   (Coll (MapEntry a b))
                   (All [x]
                     (Fun [Any -> (U nil b)]
                          [Any x -> (U x b)]))))

(def-type-alias (SortedMap a b)
                (I (Map a b)
                   clojure.lang.Sorted))

(def-type-alias (NilMap a b)
                (U (Map a b)
                   nil))

(def-type-alias (Associative a b)
                clojure.lang.Associative)

(def-type-alias (Vector a)
                (I APersistentVector
                   (Seqable a)
                   [AnyInteger -> a]
                   (Coll a)
                   (Associative AnyInteger a)))

(def-type-alias (NilColl a)
                (U (Coll a)
                   nil))

(def-type-alias (Stack a)
                (I IPersistentStack
                   (Seqable a)))

(def-type-alias (List a)
                (I clojure.lang.PersistentList ;no APersistentList ?
                   (Seqable a)
                   (Seq a)
                   (Coll a)
                   (Stack a)))

(def-type-alias (NilList a)
                (U (List a)
                   nil))

(def-type-alias (Set a)
                (I clojure.lang.APersistentSet
                   (Seqable a)))

(def-type-alias (SortedSet a)
                (I (Set a)
                   clojure.lang.Sorted))

(def-type-alias Symbol
                (I clojure.lang.Symbol
                   (All [k v o]
                     (Fun [(Map k v) -> (U nil v)]
                          [(Map k v) o -> (U o v)]))
                   (Meta Any)))

(def-type-alias Keyword
                (I clojure.lang.Keyword
                   (All [k v o]
                      (Fun [(Map k v) -> (U nil v)]
                           [(Map k v) o -> (U o v)]))))
