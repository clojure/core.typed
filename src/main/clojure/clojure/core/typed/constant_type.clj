(ns clojure.core.typed.constant-type
  (:require [clojure.core.typed
             [type-rep :as r]
             [type-ctors :as c]])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef AReference ARef IDeref IReference APersistentSet PersistentHashSet Sorted
                         LazySeq APersistentMap Indexed)))

;[Any -> Type]
(defprotocol ConstantType 
  (constant-type [this]))

(extend-protocol ConstantType
  nil
  (constant-type [_] r/-nil)

  Class
  (constant-type [v] (r/-val v))

  Symbol
  (constant-type [v] (r/-val v))

  Long
  (constant-type [v] (r/-val v))

  Double
  (constant-type [v] (r/-val v))

  Integer
  (constant-type [v] (r/-val v))

  java.math.BigDecimal
  (constant-type [v] (r/-val v))

  clojure.lang.BigInt
  (constant-type [v] (r/-val v))

  String
  (constant-type [v] (r/-val v))

  Character
  (constant-type [v] (r/-val v))

  clojure.lang.Keyword
  (constant-type  [v] (r/-val v))

  java.util.regex.Pattern
  (constant-type [v] (c/RClass-of java.util.regex.Pattern))

  Boolean
  (constant-type [v] (if v r/-true r/-false))
  PersistentHashSet
  (constant-type [v] (c/RClass-of PersistentHashSet [(apply c/Un (map constant-type v))]))

  ;nothing specific, Cons seems like an implementation detail
  Cons
  (constant-type [v] (c/RClass-of Seqable [(apply c/Un (map constant-type v))]))

  IPersistentList
  (constant-type [clist] (r/->HeterogeneousList (apply list (map constant-type clist))))

  ;Make sure lists hit these cases instead of ISeq
  PersistentList
  (constant-type [clist] (r/->HeterogeneousList (apply list (map constant-type clist))))
;  PersistentList$EmptyList
;  (constant-type [clist] (r/->HeterogeneousList (apply list (map constant-type clist))))

  ;default for ISeqs
  ISeq
  (constant-type [iseq] 
    (cond
      ;handle empty list?
      (list? iseq) (r/->HeterogeneousList (apply list (map constant-type iseq)))
      :else (c/RClass-of ISeq [(apply c/Un (map constant-type iseq))])))

  IPersistentVector
  (constant-type  [cvec] (r/-hvec (mapv constant-type cvec)))

  IPersistentMap
  (constant-type [cmap]
    (let [kts (map constant-type (keys cmap))
          vts (map constant-type (vals cmap))]
      (if (every? r/Value? kts)
        (c/-complete-hmap (zipmap kts vts))
        (c/RClass-of IPersistentMap 
                     [(apply c/Un kts)
                      (apply c/Un vts)])))))
