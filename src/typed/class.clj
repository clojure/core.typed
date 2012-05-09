(ns typed.class
  (:import (clojure.lang Seqable IPersistentCollection IPersistentStack
                         IPersistentList IPersistentVector APersistentVector PersistentVector
                         IMapEntry AMapEntry MapEntry ILookup Associative IPersistentMap
                         IDeref IMeta IObj IRef IReference AReference ARef Atom Ref ISeq
                         IPersistentSet Delay Agent IBlockingDeref IEditableCollection
                         IHashEq PersistentHashMap PersistentTreeSet PersistentHashSet))
  (:require [typed.core :refer [annotate-class]]))


;interfaces, like protocols here. Mix and match, but none imply others.

(parameterise-interface Seqable [a])

(parameterise-interface IPersistentCollection [a])

(parameterise-interface ISeq [a])

(parameterise-interface IPersistentStack [a])

(parameterise-interface IPersistentSet [a])

(parameterise-interface IPersistentList [a])

(parameterise-interface IPersistentVector [a])

(parameterise-interface IMapEntry [a b])

(parameterise-interface ILookup [a b])

(parameterise-interface Associative [a b])

(parameterise-interface IPersistentMap [a b])

(parameterise-interface IDeref [a])

(parameterise-interface IBlockingDeref [a])

(parameterise-interface IMeta [(a <! (U nil (IPersistentMap Any Any)))])

;abstract classes, more like type aliases here

(parameterise-abstract-class APersistentMap [a b]
  (I Iterable 
     (IPersistentCollection (MapEntryT a b))
     IHashEq 
     [Any -> (U nil b)] 
     (Associative a b)
     MapEquivalence 
     java.util.concurrent.Callable 
     (clojure.lang.IPersistentMap a b)
     (ILookup a b)
     java.io.Serializable
     java.lang.Runnable 
     java.util.Map 
     Counted 
     (Seqable (MapEntryT a b))
     java.lang.Object))

(parameterise-abstract-class APersistentVector [a]
  (I Reversible 
     Indexed 
     Iterable 
     (IPersistentCollection a) 
     IHashEq 
     [Long -> a] 
     (Associative Long a) 
     Comparable 
     RandomAccess 
     java.util.concurrent.Callable 
     (IPersistentVector a) 
     java.util.List 
     java.util.Collection 
     (ILookup Long a) 
     Serializable 
     Runnable 
     Counted 
     (Seqable a)
     Object 
     (IPersistentStack a)
     Sequential 
     IEditableCollection))

(parameterise-abstract-class AMapEntry [a b]
  (I Reversible 
     Indexed 
     Iterable 
     (IPersistentCollection (U a b))
     IHashEq 
     [Long -> (U a b)]
     (Associative Long (U a b))
     java.lang.Comparable 
     java.util.RandomAccess 
     java.util.concurrent.Callable 
     (IPersistentVector (U a b))
     java.util.List 
     java.util.Collection 
     (ILookup Long (U a b))
     java.io.Serializable 
     java.util.Map$Entry 
     (IMapEntry a b) 
     java.lang.Runnable 
     Counted 
     (Seqable (U a b)) 
     Object 
     (IPersistentStack (U a b))
     Sequential))

(parameterise-abstract-class APersistentSet [a]
  (I (IPersistentSet a)
     [Any -> (U nil a)]
     Iterable 
     (IPersistentCollection a) 
     IHashEq 
     java.util.Set 
     java.util.concurrent.Callable 
     java.util.Collection 
     java.io.Serializable 
     java.lang.Runnable 
     Counted 
     (Seqable a) 
     java.lang.Object))

(parameterise-abstract-class Obj [a]
  (I IObj
     (IMeta a)
     java.io.Serializable))

(parameterise-abstract-class ASeq [a]
  (I java.lang.Iterable 
     (IPersistentCollection a) 
     IHashEq 
     java.util.List 
     java.util.Collection 
     java.io.Serializable 
     (ISeq a)
     (Seqable a)
     java.lang.Object 
     Sequential 
     (Obj (APersistentMap Any Any))))

;concrete classes, again more like type aliases. Abstract classes useful for abstraction

(parameterise-class PersistentVector [a]
  (I IObj
     (IMeta (Map Any Any))
     (APersistentVector a)))

(parameterise-class PersistentHashMap [a b]
  (I (Map a b)
     IObj
     (IMeta (Map Any Any))
     IEditableCollection))

(parameterise-class PersistentTreeMap [a b]
  (I (Map a b) 
     IObj
     (IMeta (Map Any Any))
     Reversible 
     Sorted))

(parameterise-class PersistentHashSet [a]
  (I IEditableCollection
     (IMeta (Map Any Any))
     IObj))

(parameterise-class PersistentTreeSet [a]
  (I Reversible
     Sorted
     (IMeta (Map Any Any))
     IObj))

(parameterise-class MapEntry [a b]
  (MapEntryT a b))

(parameterise-class Keyword []
  (I (All [k v o] 
       (Fun [(Map k v) -> (U nil v)]
            [(Map k v) o -> (U o v)]))
     java.lang.Comparable 
     java.util.concurrent.Callable 
     java.io.Serializable 
     java.lang.Runnable 
     Named 
     java.lang.Object))

(parameterise-class Symbol []
  (I (All [k v o] 
       (Fun [(Map k v) -> (U nil v)]
            [(Map k v) o -> (U o v)]))
     (IMeta (Map Any Any))
     IObj
     java.lang.Comparable 
     java.util.concurrent.Callable 
     java.io.Serializable 
     java.lang.Runnable 
     Named 
     java.lang.Object)
