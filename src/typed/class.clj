(ns typed.class
  (:import (clojure.lang Seqable IPersistentCollection IPersistentStack
                         IPersistentList IPersistentVector APersistentVector PersistentVector
                         IMapEntry AMapEntry MapEntry ILookup Associative IPersistentMap
                         IDeref IMeta IObj IRef IReference AReference ARef Atom Ref ISeq
                         IPersistentSet Delay Agent IBlockingDeref IEditableCollection
                         IHashEq PersistentHashMap PersistentTreeSet PersistentHashSet Ratio
                         IFn Fn)))

;markers with no equivalents
(def-type ivar)

;numbers
(def-type-alias integer (U Integer LongDouble clojure.lang.BigInt BigInteger Short Byte))
(def-type-alias ratio Ratio)
(def-type-alias decimal BigDecimal)
(def-type-alias rational (U integer ratio decimal))
(def-type-alias float (U Double Float))
(def-type-alias number Number)

;collection interfaces
(def-type-alias sorted Sorted)
(def-type-alias sequential Sequential)
(def-type-alias reversible Reversible)
(def-type-alias counted Counted)
(def-type-alias hash IHashEq)
(def-type-alias ilookup ILookup)
(def-type-alias named Named)
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

;single types
(def-type-alias char Character)

;collection types
(def-type-alias string String)

(def-type-alias nonseqable-seqable
                (U Iterable
                   ;array
                   CharSequence
                   java.util.Map))

(def-type-alias (seqable a)
                (I iseqable
                   nonseqable-seqable))

(def-type-alias (seq a)
                (I iseq
                   (seqable a)
                   (coll a)
                   sequential
                   hash))

(def-type-alias (map a b)
                (I imap
                   (seqable (map-entry a b))
                   (coll (map-entry a b))
                   (All [x]
                     (Fun [Any -> (U nil b)]
                          [Any x -> (U x b)]))
                   hash))

(def-type-alias (vector a)
                (I ivector
                   (seqable a)
                   [integer -> a]
                   (coll a)
                   (associative integer a)))

(def-type-alias (coll a)
                (I icoll
                   (seqable a)))


(def-type-alias (list a)
                (I ilist
                   (seqable a)
                   (seq a)
                   (coll a)
                   (stack a)))

(def-type-alias (set a)
                (I iset
                   (seqable a)
                   counted))

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
                   meta))

(def-type-alias keyword
                (I ikeyword
                   named
                   (Fun [(map k v) -> (U nil v)]
                        [(map k v) o -> (U o v)])))

fn
future
