(ns clojure.core.typed.constant-type
  (:require [clojure.core.typed.type-rep :as r :refer [ret]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.hset-utils :as hset])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable ISeq
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef ARef IDeref IReference PersistentHashSet Sorted
                         LazySeq Indexed)))

(defprotocol ConstantType 
  (constant-ret [this]))

(defn constant-type [s]
  (r/ret-t (constant-ret s)))

;[Any -> Type]

(defmacro constant-type->val
  [& cls]
  (let [method `(constant-ret [v#] (ret (r/-val v#)))]
    `(extend-protocol ConstantType
       ~@(apply concat (zipmap cls (repeat method))))))

(constant-type->val
  nil Class Symbol Long Double Integer java.math.BigDecimal
  clojure.lang.BigInt String Character clojure.lang.Keyword
  Boolean clojure.lang.Namespace)

(extend-protocol ConstantType
  java.util.regex.Pattern
  (constant-ret [v] (ret (c/RClass-of java.util.regex.Pattern)))

  PersistentHashSet
  (constant-ret [v] 
    (ret
      (if (every? hset/valid-fixed? v)
        (r/-hset (r/sorted-type-set (map r/-val v)))
        (c/RClass-of PersistentHashSet [(apply c/Un (map constant-type v))]))))

  ;nothing specific, Cons seems like an implementation detail
  Cons
  (constant-ret [v] (ret (c/RClass-of Seqable [(apply c/Un (map constant-type v))])))

  IPersistentList
  (constant-ret [clist] (ret (r/HeterogeneousList-maker (apply list (map constant-type clist)))))

  ;Make sure lists hit these cases instead of ISeq
  PersistentList
  (constant-ret [clist] (ret (r/HeterogeneousList-maker (apply list (map constant-type clist)))))
;  PersistentList$EmptyList
;  (constant-ret [clist] (ret (r/HeterogeneousList-maker (apply list (map constant-type clist)))))

  ;default for ISeqs
  ISeq
  (constant-ret [iseq] 
    (cond
      ;handle empty list?
      (list? iseq) (ret (r/HeterogeneousList-maker (apply list (map constant-type iseq))))
      :else (ret (c/RClass-of ISeq [(apply c/Un (map constant-type iseq))]))))

  IPersistentVector
  (constant-ret [cvec] (ret (r/-hvec (mapv constant-type cvec))))

  IPersistentMap
  (constant-ret [cmap]
    (let [kts (map constant-type (keys cmap))
          vts (map constant-type (vals cmap))]
      (if (every? r/Value? kts)
        (ret (c/-complete-hmap (zipmap kts vts)))
        (ret (c/RClass-of IPersistentMap 
                          [(apply c/Un kts)
                           (apply c/Un vts)])))))
  
  ;base case
  Object
  (constant-ret [bse]
    (ret (c/RClass-of-with-unknown-params (class bse)))))
