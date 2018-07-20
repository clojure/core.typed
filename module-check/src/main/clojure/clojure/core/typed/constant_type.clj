(ns clojure.core.typed.constant-type
  (:require [clojure.core.typed.type-rep :as r :refer [ret]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.hset-utils :as hset]
            [clojure.core.typed.filter-ops :as fo])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons ISeq
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef ARef IDeref IReference Sorted
                         LazySeq Indexed)))

(defprotocol ConstantType 
  (constant-ret [this]))

(def ^:dynamic *quoted?* false)

(defn constant-type [s & [quoted?]]
  (binding [*quoted?* (or quoted? *quoted?*)]
    (if (and (not *quoted?*)
             (seq? s)
             (= 'quote (first s))
             (= 2 (count s)))
      (binding [*quoted?* true]
        (r/ret-t (constant-ret (second s))))
      (r/ret-t (constant-ret s)))))

;[Any -> Type]

(defmacro constant-type->val
  [& cls]
  (let [method `(constant-ret [v#] (ret (r/-val v#)))]
    `(extend-protocol ConstantType
       ~@(apply concat (zipmap cls (repeat method))))))

(constant-type->val
  Class Symbol Long Double Integer java.math.BigDecimal
  clojure.lang.BigInt String Character clojure.lang.Keyword
  Boolean clojure.lang.Namespace)

(extend-protocol ConstantType
  nil
  (constant-ret [v]
    (impl/impl-case
      :clojure (ret (r/-val nil))
      :cljs (ret (r/JSNull-maker))))

  java.util.regex.Pattern
  (constant-ret [v]
    (impl/impl-case
      :clojure (ret (c/RClass-of java.util.regex.Pattern))
      :cljs (assert nil "TODO: CLJS pattern in ConstantType")))

  IPersistentSet
  (constant-ret [v] 
    (ret
      (if (every? hset/valid-fixed? v)
        (r/-hset (r/sorted-type-set (map r/-val v)))
        (c/-name `t/Set (apply c/Un (map constant-type v))))))

  ;default for ISeqs
  ISeq
  (constant-ret [iseq] (ret (r/-hsequential
                              (mapv constant-type iseq)
                              :kind (cond
                                      (list? iseq) :list
                                      (seq? iseq) :seq
                                      :else :sequential))))

  IPersistentVector
  (constant-ret [cvec] (ret (r/-hvec (mapv constant-type cvec))))

  IPersistentMap
  (constant-ret [cmap]
    (let [kts (map constant-type (keys cmap))
          vts (map constant-type (vals cmap))]
      (if (every? r/Value? kts)
        (ret (c/-complete-hmap (zipmap kts vts)))
        (ret (c/-name `t/Map
                      (apply c/Un kts)
                      (apply c/Un vts))))))
  
  ;base case
  Object
  (constant-ret [bse]
    (impl/impl-case
      :clojure (ret (c/RClass-of-with-unknown-params (class bse)))
      :cljs (cond
              (number? bse) (ret (r/JSNumber-maker))
              :else (assert nil "TODO: base case of constant-type")))))
