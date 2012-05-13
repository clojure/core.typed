(ns typed.class
  (:require [typed.core :refer [typed-ns def-type-alias def-new-type]]))

(typed-ns typed.class
  (:import (clojure.lang Seqable IPersistentCollection IPersistentStack
                         IPersistentList IPersistentVector APersistentVector PersistentVector
                         IMapEntry AMapEntry MapEntry ILookup Associative IPersistentMap
                         IDeref IMeta IObj IRef IReference AReference ARef Atom Ref ISeq
                         IPersistentSet Delay Agent IBlockingDeref IEditableCollection
                         IHashEq PersistentHashMap PersistentTreeSet PersistentHashSet Ratio
                         IFn Fn Symbol)))

;markers with no equivalents
(def-new-type ivar)

;numbers markers
(def-type-alias iinteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte))
(def-type-alias iratio Ratio)
(def-type-alias idecimal BigDecimal)
(def-type-alias irational (U iinteger iratio idecimal))
(def-type-alias ifloat (U Double Float))
(def-type-alias inumber Number)

;number types
(def-type-alias integer iinteger)
(def-type-alias ratio iratio)
(def-type-alias decimal idecimal)
(def-type-alias rational irational)
(def-type-alias float ifloat)
(def-type-alias number inumber)

;collection interfaces
(def-type-alias isorted Sorted)
(def-type-alias isequential Sequential)
(def-type-alias ireversible Reversible)
(def-type-alias icounted Counted)
(def-type-alias ihash IHashEq)
(def-type-alias ilookup ILookup)
(def-type-alias inamed Named)
(def-type-alias ivector IPersistentVector)
(def-type-alias imap IPersistentMap)
(def-type-alias iset IPersistentSet)
(def-type-alias iseq ISeq)
(def-type-alias iseqable Seqable)
(def-type-alias icoll IPersistentCollection)
(def-type-alias ilist IPersistentList)
(def-type-alias iset IPersistentSet)
(def-type-alias ifn IFn)
(def-type-alias fn Fn)

(def-type-alias sorted isorted)
(def-type-alias reversible ireversible)

;metadata markers
(def-type-alias iobj IObj)
(def-type-alias imeta IMeta)

;metadata types
(def-type-alias (meta a)
                (I imeta
                   iobj))

;reference markers
(def-type-alias ireference IReference) ;metadata
(def-type-alias iref IRef)             ;managed reference
(def-type-alias ideref IDeref)
(def-type-alias inamespace Namespace)
(def-type-alias iatom Atom)

;parameterised aliases
(def-type-alias (deref a)
                ideref)
(def-type-alias (reference a)
                (I ireference
                   (meta a)))

;reference types
(def-type-alias namespace 
                (I inamespace
                   (reference Any)))
(def-type-alias (atom a)
                (I iatom
                   (deref a)
                   ireference))

;markers
(def-type-alias ichar Character)
(def-type-alias isymbol Symbol)
(def-type-alias ikeyword Keyword)
(def-type-alias iboolean (U true false))
(def-type-alias istring String)

(def-type-alias boolean iboolean)
(def-type-alias char ichar)
(def-type-alias string istring)

(def-type-alias nonseqable-seqable
                (U Iterable
                   ;array
                   CharSequence
                   java.util.Map))

(def-type-alias (seqable a)
                (U iseqable
                   nonseqable-seqable))

(def-type-alias (nseqable a)
                (U (seqable a)
                   nil))

(def-type-alias (seq a)
                (I iseq
                   (seqable a)
                   (coll a)
                   sequential
                   hash))

(def-type-alias (nseq a)
                (U (seq a)
                   nil))

(def-type-alias (map a b)
                (I imap
                   (seqable (map-entry a b))
                   (coll (map-entry a b))
                   (All [x]
                     (Fun [Any -> (U nil b)]
                          [Any x -> (U x b)]))
                   hash))

(def-type-alias (sorted-map a b)
                (I (map a b)
                   sorted))

(def-type-alias (nmap a b)
                (U (map a b)
                   nil))

(def-type-alias (vector a)
                (I ivector
                   (seqable a)
                   [integer -> a]
                   (coll a)
                   reversible
                   (associative integer a)))

(def-type-alias (associative a b)
                iassociative)

(def-type-alias (coll a)
                (I icoll
                   (seqable a)))

(def-type-alias (ncoll a)
                (U (coll a)
                   nil))

(def-type-alias (list a)
                (I ilist
                   (seqable a)
                   (seq a)
                   (coll a)
                   (stack a)))

(def-type-alias (nlist a)
                (U (list a)
                   nil))

(def-type-alias (set a)
                (I iset
                   (seqable a)
                   counted))

(def-type-alias (sorted-set a)
                (I (set a)
                   sorted))

(def-type-alias (stack a)
                (I istack
                   (seqable a)
                   sequential))

(def-type-alias symbol
                (I isymbol
                   named
                   (All [k v o]
                     (Fun [(map k v) -> (U nil v)]
                          [(map k v) o -> (U o v)]))
                   (meta Any)))

(def-type-alias keyword
                (I ikeyword
                   named
                   (Fun [(map k v) -> (U nil v)]
                        [(map k v) o -> (U o v)])))
