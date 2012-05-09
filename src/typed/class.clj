(ns typed.class
  (:import (clojure.lang Seqable IPersistentCollection IPersistentStack
                         IPersistentList IPersistentVector APersistentVector PersistentVector
                         IMapEntry AMapEntry MapEntry ILookup Associative IPersistentMap
                         IDeref IMeta IObj IRef IReference AReference ARef Atom Ref ISeq
                         IPersistentSet Delay Agent IBlockingDeref IEditableCollection
                         IHashEq PersistentHashMap))
  (:require [typed.core :refer [annotate-class]]))

;all classes/interfaces know how to directly coerce to each parameterised ancestor
; Note: not very useful just doing this for concrete classes, as most return types
; in the core library are interfaces. eg. IPersistentVector instead of PersistentVector

(parameterise-interface Seqable [a])

(parameterise-interface IPersistentCollection [a]
  (Seqable a))

(parameterise-interface ISeq [a]
  (IPersistentCollection a)
  (Seqable a))

(parameterise-interface IPersistentStack [a]
  (IPersistentCollection a)
  (Seqable a))

(parameterise-interface IEditableCollection [])

(parameterise-interface IPersistentSet [a]
  (IPersistentCollection a)
  (Seqable a))

(parameterise-interface IPersistentList [a]
  (IPersistentCollection a)
  (Seqable a)
  (IPersistentStack a))

(parameterise-interface IPersistentVector [a]
  (IPersistentCollection a)
  (Associative Number a)
  (ILookup Number a)
  (Seqable a)
  (IPersistentStack a))

(parameterise-interface IMapEntry [a b])

(parameterise-interface ILookup [a b])

(parameterise-interface IHashEq [])

(parameterise-interface Associative [a b]
  (IPersistentCollection Any)
  (ILookup a b)
  (Seqable Any))

(parameterise-interface IPersistentMap [a b]
  (IPersistentCollection (IMapEntry a b))
  (Associative a b)
  (ILookup a b)
  (Seqable (IMapEntry a b)))

(parameterise-interface IDeref [a])

(parameterise-interface IBlockingDeref [a])

(parameterise-interface IMeta [(a <! (U nil (IPersistentMap Any Any)))])

(parameterise-interface IObj [(a <! (U nil (IPersistentMap Any Any)))])

;TODO
;(parameterise-interface IFn [])

; ignore abstract classes

(parameterise-class PersistentVector [a]
  (clojure.lang.Associative Number a)
  ;clojure.lang.IFn 
  (clojure.lang.ILookup Long a)
  (clojure.lang.IMeta (U nil (IPersistentMap Any Any)))
  (clojure.lang.IObj (U nil (IPersistentMap Any Any)))
  (clojure.lang.IPersistentCollection a)
  (clojure.lang.IPersistentStack a)
  (clojure.lang.IPersistentVector a)
  (clojure.lang.Seqable a))

(annotate-class PersistentHashMap [a b]
  (clojure.lang.Associative a b)
  ;clojure.lang.IFn 
  (clojure.lang.ILookup a b)
  (clojure.lang.IMeta (U nil (IPersistentMap Any Any)))
  (clojure.lang.IObj (U nil (IPersistentMap Any Any)))
  (clojure.lang.IPersistentCollection (IMapEntry a b))
  (clojure.lang.IPersistentMap a b)
  (clojure.lang.Seqable (IMapEntry a b)))

(parameterise-class PersistentTreeMap [a b]
  (clojure.lang.Associative a b)
  ;clojure.lang.IFn 
  (clojure.lang.ILookup a b)
  (clojure.lang.IMeta (U nil (IPersistentMap Any Any)))
  (clojure.lang.IObj (U nil (IPersistentMap Any Any)))
  (clojure.lang.IPersistentCollection (IMapEntry a b))
  (clojure.lang.IPersistentMap a b)
  (clojure.lang.Seqable (IMapEntry a b)))

(parameterise-class PersistentHashSet [a]
  ;clojure.lang.IFn 
  (clojure.lang.IMeta (U nil (IPersistentMap Any Any)))
  (clojure.lang.IObj (U nil (IPersistentMap Any Any)))
  (clojure.lang.IPersistentCollection a)
  (clojure.lang.IPersistentSet a)
  (clojure.lang.Seqable a))

(parameterise-class PersistentTreeSet [a]
  ;clojure.lang.AFn 
  ;clojure.lang.APersistentSet 
  ;clojure.lang.IFn 
  (clojure.lang.IMeta (U nil (IPersistentMap Any Any)))
  (clojure.lang.IObj (U nil (IPersistentMap Any Any)))
  (clojure.lang.IPersistentCollection a)
  (clojure.lang.IPersistentSet a)
  (clojure.lang.Seqable a))

(parameterise-class MapEntry [a b]
  (clojure.lang.Associative Long (U a b))
  ;clojure.lang.IFn 
  (clojure.lang.ILookup Long (U a b))
  (clojure.lang.IMapEntry a b)
  (clojure.lang.IPersistentCollection (U a b))
  (clojure.lang.IPersistentStack (U a b))
  (clojure.lang.IPersistentVector (U a b))
  (clojure.lang.Seqable (U a b)))

(parameterise-class Keyword []
  ;clojure.lang.IFn    ; [Keyword (IPersistentMap Any Any) -> Any]
  )

(parameterise-class Symbol []
  ;clojure.lang.IFn  ; [Symbol (IPersistentMap Any Any) -> Any]
  (clojure.lang.IMeta (U nil (IPersistentMap Any Any)))
  (clojure.lang.IObj (U nil (IPersistentMap Any Any))))
