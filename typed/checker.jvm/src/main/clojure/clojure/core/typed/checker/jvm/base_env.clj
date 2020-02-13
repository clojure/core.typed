;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.base-env
  (:import (clojure.lang Named IMapEntry AMapEntry Seqable
                         LazySeq PersistentHashSet PersistentTreeSet PersistentList
                         IPersistentSet IPersistentMap IPersistentVector
                         APersistentMap ISeq IPersistentCollection
                         ILookup Indexed Associative
                         IRef Reduced)
           (java.util Comparator Collection))
  (:require [clojure.core.typed.checker.base-env-helper :as h]
            [clojure.core.typed.checker.jvm.base-env-clj-rclass :as base-rclass]
            [clojure.core.typed.checker.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.path-rep :as pe]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.fold-default]
            [clojure.core.typed.checker.name-env :as nme-env]
            [clojure.core.typed.checker.subst]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed :refer [Any Nothing TFn Rec
                                        Pred U I All IFn
                                        HVec HSequential]
             :as t]))

;; Dev notes
;; ---------
;;
;; To reload these type annotations *without* restarting the repl,
;; you should reload this file then run `(reset-clojure-envs!)`.
;;
;; There is some abuse of interning to get the type resolving correctly
;; in the annotations. The goal is to simulate we're inside `clojure.core.typed`.

(defn- aset-*-type [t]
  (impl/with-clojure-impl
    (let [arr-t (prs/parse-type `(~'Array ~t))
          rtn-type (prs/parse-type t)
          num-t (prs/parse-type 'Number)]
      (apply r/make-FnIntersection
             (map r/make-Function
                  (loop [num 1
                         result []
                         dom [arr-t num-t]]
                    (if (> num 10)
                      result
                      (recur (inc num)
                             (conj result (conj dom rtn-type))
                             (conj dom num-t))))
                  (repeat rtn-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial type aliases

;(base-rclass/reset-rclass-env!)

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

(defn ^:private count-type []
  (impl/with-clojure-impl
    (r/make-FnIntersection
      (r/make-Function
        [(prs/parse-type '(U nil (clojure.core.typed/Seqable Any) clojure.lang.Counted))]
        (prs/parse-type '(U java.lang.Integer java.lang.Long))
        :object (obj/-path [(pe/CountPE-maker)] 0)))))

(defn ^:private nth-type []
  (impl/with-clojure-impl
    (prs/parse-type
      '(All [x y]
            (IFn 
              [(U (Indexed x) (t/SequentialSeqable x)) t/AnyInteger -> x]
              [(U (Indexed x) (t/SequentialSeqable x) nil) t/AnyInteger y -> (U x y)]
              [(U (Indexed x) (t/SequentialSeqable x) nil) t/AnyInteger -> (U x nil)])))))

(def this-ns *ns*)

(delay-and-cache-env ^:private init-var-env
  ;(reset-alias-env!)
  (merge
   (common/parse-clj-ann-map @common/common-var-annotations)
   (h/var-mappings
     this-ns

clojure.core.typed/check-ns (IFn [t/Symbol -> Any]
                                [-> Any])
;; Internal annotations

;clojure.core.typed.current-impl/*current-impl* Any
clojure.core.typed.current-impl/clojure Any
clojure.core.typed.current-impl/clojurescript Any
clojure.core.typed/ann* [Any Any Any -> Any]
clojure.core.typed/untyped-var* [Any Any -> Any]
clojure.core.typed/declare-names* [Any -> Any]
clojure.core.typed/typed-deps* [Any -> Any]
clojure.core.typed/warn-on-unannotated-vars* [-> Any]
clojure.core.typed/ann-datatype* [Any Any Any Any -> Any]
clojure.core.typed/ann-protocol* [Any Any Any -> Any]
clojure.core.typed/ann-record* [Any Any Any Any -> Any]
clojure.core.typed/ann-pdatatype* [Any Any Any Any -> Any]
clojure.core.typed/declare-datatypes* [Any -> Any]
clojure.core.typed/declare-protocols* [Any -> Any]
clojure.core.typed/non-nil-return* [Any Any -> Any]
clojure.core.typed/nilable-param* [Any Any -> Any]
clojure.core.typed/override-constructor* [Any Any -> Any]
clojure.core.typed/override-method* [Any Any -> Any]
clojure.core.typed/typed-deps* [Any -> Any]
clojure.core.typed/load-if-needed [-> Any]
clojure.core.typed/*collect-on-eval* Any
; should always be special cased
;clojure.core.typed/var>* [Any -> (t/Var2 Nothing Any)]

;; core annotations

clojure.core/*ns* t/Namespace
clojure.core/pop-thread-bindings [-> Any]
clojure.core/load [String * -> Any]
clojure.core/read-string [String -> Any]
clojure.core/read (IFn [-> Any]
                      [java.io.Reader -> Any]
                      [java.io.Reader Boolean Any -> Any]
                      [java.io.Reader Boolean Any Boolean -> Any])
clojure.core/read-line [-> (U nil String)]

clojure.core/add-classpath [(U String java.net.URL) -> nil]

clojure.core/*1 Any
clojure.core/*2 Any
clojure.core/*3 Any
clojure.core/*e (U nil Throwable)
clojure.core/*agent* (U nil (t/Agent2 Nothing Any))
clojure.core/*allow-unresolved-vars* Any
clojure.core/*assert* Any
clojure.core/*data-readers* (t/Map t/Symbol (t/Var2 Nothing Any))
clojure.core/*default-data-reader-fn* (U nil [Any Any -> Any])
clojure.core/*fn-loader* Any
clojure.core/*math-context* Any
clojure.core/*source-path* String
clojure.core/*use-context-classloader* Any

clojure.core/alength [(ReadOnlyArray Any) -> t/AnyInteger]
clojure.core/aclone (All [x] [(ReadOnlyArray x) -> (Array x)])
clojure.core/aget (All [x] (IFn [(ReadOnlyArray x) 
                                t/AnyInteger -> x]
                               [(ReadOnlyArray (ReadOnlyArray x)) 
                                t/AnyInteger t/AnyInteger -> x]
                               [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x))) 
                                t/AnyInteger t/AnyInteger t/AnyInteger -> x]
                               [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))) 
                                t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger -> x]
                               ; don't support unsound cases
                               [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))))
                                t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger -> x]))

clojure.core/aset
(All [x]
  (IFn
    [(Array x) t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]))

clojure.core/macroexpand-1 [Any -> Any]
clojure.core/macroexpand [Any -> Any]

clojure.core/create-struct [Any * -> (t/Map Any Any)]

clojure.core/find-ns [t/Symbol -> t/Namespace]
clojure.core/create-ns [t/Symbol -> t/Namespace]
clojure.core/remove-ns [t/Symbol -> t/Namespace]

clojure.core/namespace [(U t/Symbol String t/Keyword) -> (t/Option String)]
clojure.core/ns-name [(U t/Symbol t/Namespace) -> t/Symbol]
clojure.core/ns-map [(U t/Symbol t/Namespace) -> t/Symbol]
clojure.core/ns-aliases [(U t/Symbol t/Namespace) -> (t/Map t/Symbol t/Namespace)]
clojure.core/name [(U String Named) -> String]
clojure.core/the-ns [(U t/Symbol t/Namespace) -> t/Namespace]
clojure.core/in-ns [t/Symbol -> nil]
clojure.core/import [Any * -> nil]
clojure.core/identity (All [x] [x -> x
                                :filters {:then (! (U nil false) 0)
                                          :else (is (U nil false) 0)}
                                :object {:id 0}])
clojure.core/gensym (IFn [-> t/Symbol]
                        [(U t/Symbol String) -> t/Symbol])
clojure.core/intern (IFn [(U t/Symbol t/Namespace) t/Symbol -> (t/Var2 Nothing Any)]
                        [(U t/Symbol t/Namespace) t/Symbol Any -> (t/Var2 Nothing Any)])


clojure.core/doall (All [[c :< (U nil (Seqable Any))]]
                     (IFn [c -> c]
                         [t/AnyInteger c -> c]))
clojure.core/dorun (IFn [(U nil (Seqable Any)) -> nil]
                       [t/AnyInteger (U nil (Seqable Any)) -> nil])
clojure.core/iterate (All [x]
                       [[x -> x] x -> (t/ASeq x)])
clojure.core/memoize (All [x y ...]
                            [[y ... y -> x] -> [y ... y -> x]])

clojure.core/key (All [x]
                           [(IMapEntry x Any) -> x])
clojure.core/val (All [x]
                           [(IMapEntry Any x) -> x])

;clojure.core/juxt
;(All [b1 ...]
;(All [x r b2 ...]
;     (Fn [[b1 ... b1 -> b2] ... b2 -> [b1 ... b1 -> '[b2 ... b2]]]
;         [[b1 ... b1 -> r] * -> [b1 ... b1 -> (t/Vec r)]]
;         [[x * -> b2] ... b2 -> [x * -> '[b2 ... b2]]]
;         [[x * -> r] * -> [x * -> (t/Vec r)]])))


;TODO flip filters
clojure.core/complement (All [x] [[x -> Any] -> [x -> Boolean]])
; should preserve filters
clojure.core/boolean [Any -> Boolean]

clojure.core/filter (All [x y]
                           (IFn
                             [[x -> Any :filters {:then (is y 0)}] (t/Option (Seqable x)) -> (t/ASeq y)]
                             [[x -> Any :filters {:then (! y 0)}] (t/Option (Seqable x)) -> (t/ASeq (I x (Not y)))]
                             [[x -> Any] (t/Option (Seqable x)) -> (t/ASeq x)]))
clojure.core/filterv (All [x y]
                          (IFn
                            [[x -> Any :filters {:then (is y 0)}] (t/Option (Seqable x)) -> (t/AVec y)]
                            [[x -> Any] (t/Option (Seqable x)) -> (t/AVec x)]))
clojure.core/remove (All [x y]
                           (IFn
                             [[x -> Any :filters {:else (is y 0)}] (t/Option (Seqable x)) -> (t/ASeq y)]
                             [[x -> Any :filters {:else (! y 0)}] (t/Option (Seqable x)) -> (t/ASeq (I x (Not y)))]
                             [[x -> Any] (t/Option (Seqable x)) -> (t/ASeq x)]
                             ))


clojure.core/take-while (All [x y]
                               [[x -> Any] (t/Option (Seqable x)) -> (t/ASeq x)])
clojure.core/drop-while (All [x]
                               [[x -> Any] (t/Option (Seqable x)) -> (t/ASeq x)])

clojure.core/split-with
     (All [x y z] 
       (IFn
         [[x -> Any :filters {:then (is y 0), :else (is z 0)}] (t/Option (Seqable x)) -> '[(t/ASeq y) (t/ASeq z)]]
         [[x -> Any] (t/Option (Seqable x)) -> '[(t/ASeq x) (t/ASeq x)]]))

clojure.core/split-at
     (All [x y z] 
          [t/AnyInteger (t/Option (Seqable x)) -> '[(t/ASeq x) (t/ASeq x)]])

clojure.core/partition-all (All [x] 
                             (IFn [t/Int (t/Nilable (Seqable x)) -> (t/ASeq (t/ASeq x))] 
                                 [t/Int t/Int (t/Nilable (Seqable x)) -> (t/ASeq (t/ASeq x))]))

clojure.core/repeatedly
     (All [x]
          (IFn [[-> x] -> (t/ASeq x)]
              [t/AnyInteger [-> x] -> (t/ASeq x)]))


clojure.core/some (All [x y] [[x -> y] (t/Option (Seqable x)) -> (t/Option y)])

; Unions need to support dots for this to work:
;
; (All [t0 b ...]
;    (IFn [[Any -> Any :filters {:then (is t0 0) :else (! t0 0)}] 
;         [Any -> Any :filters {:then (is b 0) :else (! b 0)}] ... b
;         -> (IFn [Any -> Any :filters {:then (is (U t0 b ... b) 0) :else (! (U t0 b ... b) 0)}]
;                [Any * -> Any])]))
clojure.core/some-fn 
  (All [t0 t1 t2 t3 t4 t5]
    (IFn [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         -> (IFn [Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1) 0) :else (! (U t0 t1) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2) 0) :else (! (U t0 t1 t2) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2 t3) 0) :else (! (U t0 t1 t2 t3) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         [Any -> Boolean :filters {:then (is t4 0) :else (! t4 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2 t3 t4) 0) :else (! (U t0 t1 t2 t3 t4) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         [Any -> Boolean :filters {:then (is t4 0) :else (! t4 0)}]
         [Any -> Boolean :filters {:then (is t5 0) :else (! t5 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (U t0 t1 t2 t3 t4 t5) 0) :else (! (U t0 t1 t2 t3 t4 t5) 0)}]
                [Any * -> Any])]
        [[Any -> Any] [Any -> Any] * -> [Any * -> Any]]))
clojure.core/every-pred
  (All [t0 t1 t2 t3 t4 t5]
    (IFn [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         -> (IFn [Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1) 0) :else (! (I t0 t1) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2) 0) :else (! (I t0 t1 t2) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2 t3) 0) :else (! (I t0 t1 t2 t3) 0)}]
                [Any * -> Any])]
        [[Any -> Boolean :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Boolean :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Boolean :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Boolean :filters {:then (is t3 0) :else (! t3 0)}]
         [Any -> Boolean :filters {:then (is t4 0) :else (! t4 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2 t3 t4) 0) :else (! (I t0 t1 t2 t3 t4) 0)}]
                [Any * -> Any])]
        [[Any -> Any :filters {:then (is t0 0) :else (! t0 0)}] 
         [Any -> Any :filters {:then (is t1 0) :else (! t1 0)}]
         [Any -> Any :filters {:then (is t2 0) :else (! t2 0)}]
         [Any -> Any :filters {:then (is t3 0) :else (! t3 0)}]
         [Any -> Any :filters {:then (is t4 0) :else (! t4 0)}]
         [Any -> Any :filters {:then (is t5 0) :else (! t5 0)}]
         -> (IFn [Any -> Boolean :filters {:then (is (I t0 t1 t2 t3 t4 t5) 0) :else (! (I t0 t1 t2 t3 t4 t5) 0)}]
                [Any * -> Any])]
        [[Any -> Any] [Any -> Any] * -> [Any * -> Any]]))

clojure.core/concat (All [x] [(t/Option (Seqable x)) * -> (t/ASeq x)])

clojure.core/set (All [x] [(t/Option (Seqable x)) -> (PersistentHashSet x)])
clojure.core/hash-set (All [x] [x * -> (PersistentHashSet x)])
clojure.core/hash-map (All [x y] [(HSequential [x y] :repeat true) <* -> (t/Map x y)])
clojure.core/sorted-map (All [x y] [(HSequential [x y] :repeat true) <* -> (t/Map x y)])
clojure.core/sorted-set (All [x] [x * -> (PersistentTreeSet x)])
clojure.core/sorted-set-by (All [x] [[x x -> t/AnyInteger] x * -> (PersistentTreeSet x)])
clojure.core/list (All [x] [x * -> (PersistentList x)])
clojure.core/list* (All [x] 
                        (IFn [(U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x x x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]
                            [x x x x x x x x x x (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]))

clojure.core/list? (Pred (t/List Any))

clojure.core/load-reader [java.io.Reader -> Any]

clojure.core/methods [t/Multi -> (t/Map Any Any)]

clojure.core/munge (IFn [t/Symbol -> t/Symbol]
                       [Any -> Any])

clojure.core/pos? (IFn [Number -> Boolean])
clojure.core/neg? (IFn [Number -> Boolean])

clojure.core/nthrest (All [x] [(U nil (Seqable x)) t/AnyInteger 
                               -> (t/ASeq x)])

clojure.core/vector (All [r b ...]
                         (IFn [b ... b -> '[b ... b]]
                             [r * -> (t/AVec r)]))
clojure.core/vec (All [x] [(t/Option (Seqable x)) -> (t/AVec x)])

clojure.core/not [Any -> Boolean]
clojure.core/constantly (All [x] [x -> [Any * -> x]])

clojure.core/bound? [(t/Var2 Nothing Any) * -> Boolean]
clojure.core/thread-bound? [(t/Var2 Nothing Any) * -> Boolean]
clojure.core/bases [(t/Nilable Class) -> (t/NilableNonEmptyASeq Class)]

clojure.core/make-hierarchy [-> t/Hierarchy]
clojure.core/isa? (IFn [Any Any -> Boolean]
                      [t/Hierarchy Any Any -> Boolean])

clojure.core/disj
     (All [x]
          (IFn [(t/SortedSet x) Any Any * -> (t/SortedSet x)]
              [(t/Set x) Any Any * -> (t/Set x)]))

clojure.core/assoc
     (All [m k v c ...]
          (IFn [m k v (t/HSeq [c c] :repeat true) <... c
                -> (Assoc m k v c ... c)]
;               [m k v (t/HSeq [k v] :repeat true) <*
;                -> (Assoc m k v)]
               [nil k v (t/HSeq [c c] :repeat true) <... c
                -> (Assoc nil k v c ... c)]
               [nil k v (t/HSeq [k v] :repeat true) <*
                -> (t/Map k v)]))
;     (All [b c d]
;       (Fn [(t/Map b c) b c -> (t/Map b c)]
;           [(t/Vec d) t/AnyInteger d -> (t/Vec d)]
;           [d b c (HSequential [b c] :repeat true) <* -> (Assoc d b c)]))

clojure.core/dissoc
     (All [k v]
       (IFn [(t/Map k v) Any * -> (t/Map k v)]))
)
    (h/var-mappings
      this-ns

clojure.core/zipmap
     (All [k v]
       [(U nil (Seqable k)) (U nil (Seqable v)) -> (APersistentMap k v)])

clojure.core/keys
(All [k]
     [(t/Map k Any) -> (t/ASeq k) :object {:id 0 :path [Keys]}])

clojure.core/vals
(All [v]
     [(t/Map Any v) -> (t/ASeq v) :object {:id 0 :path [Vals]}])

;most useful case
clojure.core/comp
     (All [x y b ...]
          [[x -> y] [b ... b -> x] -> [b ... b -> y]])


;apply: wishful thinking
;     (All [b1 ...]
;     (All [y b2 ...]
;          (IFn [[b1 ... b1 b2 ... b2 -> y] b1 ... b1 (HSequential [b2 ... b2]) -> y]
;              [[b1 ... b1 r * -> y] b1 ... b1 (U nil (Seqable r)) -> y])))

clojure.core/apply
     (All [y a b c d r z ...]
          (IFn [[z ... z -> y] (U nil (HSequential [z ... z])) -> y]
              [[a z ... z -> y] a (U nil (HSequential [z ... z])) -> y]
              [[a b z ... z -> y] a b (U nil (HSequential [z ... z])) -> y]
              [[a b c z ... z -> y] a b c (U nil (HSequential [z ... z])) -> y]
              [[a b c d z ... z -> y] a b c d (U nil (HSequential [z ... z])) -> y]
              [[r * -> y] (U nil (Seqable r)) -> y]
              [[a r * -> y] a (U nil (Seqable r)) -> y]
              [[a b r * -> y] a b (U nil (Seqable r)) -> y]
              [[a b c r * -> y] a b c (U nil (Seqable r)) -> y]
              [[a b c d r * -> y] a b c d (U nil (Seqable r)) -> y]
              ))

;partial: wishful thinking (replaces the first 4 arities)
; (All [b1 ...]
; (All [r b2 ...]
;    [[b1 ... b1 b2 ... b2 -> r] b1 ... b1 -> [b2 ... b2 -> r]]))

clojure.core/partial
     (All [y a b c d z ...]
          (IFn [[z ... z -> y] -> [z ... z -> y]]
              [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a * -> y] a * -> [a * -> y]]))

clojure.core/str [Any * -> String]
clojure.core/prn-str [Any * -> String]
clojure.core/pr-str [Any * -> String]
clojure.core/newline [-> nil]

clojure.core/print [Any * -> nil]
clojure.core/println [Any * -> nil]
clojure.core/print-str [Any * -> String]
clojure.core/println-str [Any * -> String]
clojure.core/printf [String Any * -> nil]
clojure.core/format [String Any  * -> String]
clojure.core/pr [Any * -> nil]
clojure.core/prn [Any * -> nil]
clojure.core/flush [-> nil]
clojure.core/*print-length* (U nil false t/AnyInteger)
clojure.core/*print-level* (U nil false t/AnyInteger)
clojure.core/*verbose-defrecords* Boolean
clojure.core/print-ctor [Object [Object java.io.Writer -> Any] java.io.Writer -> nil]

clojure.core/prefer-method [t/Multi Any Any -> Any]
clojure.core/print-simple [Any java.io.Writer -> nil]
clojure.core/char-escape-string (t/Map Character String)
clojure.core/char-name-string (t/Map Character String)
clojure.core/primitives-classnames (t/Map Class String)

clojure.core/namespace-munge [(U t/Symbol t/Namespace) -> String]

;clojure.core/find-protocol-impl ['{:on-interface Class
;                                   :impls ?}]


clojure.core/re-matcher [java.util.regex.Pattern String -> java.util.regex.Matcher]
clojure.core/re-groups [java.util.regex.Matcher -> (U nil String (t/Vec (t/Option String)))]
clojure.core/re-find (IFn [java.util.regex.Matcher -> (U nil String (t/Vec (t/Option String)))]
                              [java.util.regex.Pattern String -> (U nil String (t/Vec (t/Option String)))])
clojure.core/re-seq [java.util.regex.Pattern String -> (t/ASeq (U nil String (t/Vec (t/Option String))))]

clojure.core/subs (IFn [String t/AnyInteger -> String]
                           [String t/AnyInteger t/AnyInteger -> String])

;TODO
;clojure.core/spit [java.io.Writer Any]

clojure.core/future-call (All [x] [[-> x] -> (t/Future x)])

clojure.core/atom (All [x]
                       [x & :optional {:validator (U nil [x -> Any]) :meta Any} -> (t/Atom2 x x)])

clojure.core/set-validator! (All [x]
                                 [(clojure.lang.IRef Any x) [x -> Any] -> nil])

clojure.core/deref (All [x y]
                     (IFn 
                         [(t/Deref x) -> x]
                         [(U (t/Deref Any) java.util.concurrent.Future) -> Any]
                         [(t/BlockingDeref x) t/AnyInteger y -> (U x y)]
                         [(U java.util.concurrent.Future (t/BlockingDeref Any)) t/AnyInteger Any -> Any]))

clojure.core/delay? (Pred (t/Delay Any))

clojure.core/future-cancelled? [java.util.concurrent.Future -> Boolean]

clojure.core/future-cancel [java.util.concurrent.Future -> Any]

clojure.core/future? (Pred java.util.concurrent.Future)

clojure.core/future-done? [java.util.concurrent.Future -> Boolean]

clojure.core/force (All [x]
                        (IFn [(t/Delay x) -> x]
                            [Any -> Any]))

clojure.core/realized? [clojure.lang.IPending -> Boolean]

clojure.core/select-keys (All [k v] [(t/Map k v) (U nil (Seqable Any))
                                     -> (t/Map k v)])

clojure.core/sort (All [x] 
                       (IFn [(U nil (Seqable x)) -> (t/ASeq x)]
                           [(I Comparator [x x -> t/AnyInteger]) 
                            (U nil (Seqable x)) -> (t/ASeq x)]))

clojure.core/reset! (All [w r]
                              [(t/Atom2 w r) w -> w])

clojure.core/swap! (All [w r b ...] 
                             [(t/Atom2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/compare-and-set!
                   (All [w]
                     [(t/Atom2 w Any) Any w -> Boolean])

clojure.core/set-validator!
                   (All [w]
                     [(clojure.lang.IRef w Any) (U nil [w -> Any]) -> Any])

clojure.core/get-validator
                   (All [w]
                     [(clojure.lang.IRef w Any) -> (U nil [w -> Any])])

clojure.core/alter-var-root (All [w r b ...] 
                              [(t/Var2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/method-sig [java.lang.reflect.Method -> '[Any (U nil (t/NonEmptySeqable Any)) Any]]
clojure.core/proxy-name [Class (U nil (Seqable Class)) -> String]
clojure.core/get-proxy-class [Class * -> Class]
clojure.core/construct-proxy [Class Any * -> Any]
clojure.core/init-proxy [t/Proxy (t/Map String Any) -> t/Proxy]
clojure.core/update-proxy [t/Proxy (t/Map String Any) -> t/Proxy]
clojure.core/proxy-mappings [t/Proxy -> (t/Map String Any)]
clojure.core/proxy-call-with-super (All [x] [[-> x] t/Proxy String -> x])
clojure.core/bean [Object -> (t/Map Any Any)]

clojure.core/fnil (All [x y z a b ...]
                    (IFn [[x b ... b -> a] x -> [(U nil x) b ... b -> a]]
                        [[x y b ... b -> a] x y -> [(U nil x) (U nil y) b ... b -> a]]
                        [[x y z b ... b -> a] x y z -> [(U nil x) (U nil y) (U nil z) b ... b -> a]]))

clojure.core/symbol
     (IFn [(U t/Symbol String) -> t/Symbol]
         [(U nil String) String -> t/Symbol])

clojure.core/keyword
     (IFn [(U t/Keyword t/Symbol String) -> t/Keyword 
           :object {:id 0 :path [Keyword]}
           :filters {:then tt
                     :else ff}]
          [nil -> nil 
           :object {:id 0 :path [Keyword]}
           :filters {:then ff
                     :else tt}]
          [Any -> (U nil t/Keyword) 
           :object {:id 0 :path [Keyword]}
           :filters {:then (is (U t/Keyword t/Symbol String) 0)
                     :else (! (U t/Keyword t/Symbol String) 0)}]
          [String String -> t/Keyword
           :filters {:then tt
                     :else ff}])

clojure.core/find-keyword
     (IFn [(U t/Keyword t/Symbol String) -> (t/Option t/Keyword)]
         [String String -> (t/Option t/Keyword)])

clojure.core/derive (IFn [(U t/Symbol t/Keyword Class) (U t/Symbol t/Keyword) -> nil]
                        [t/Hierarchy (U t/Symbol t/Keyword Class) (U t/Symbol t/Keyword) -> t/Hierarchy])

clojure.core/compare [Any Any -> Number]

clojure.core/require [Any * -> nil]
clojure.core/use [Any * -> nil]
clojure.core/refer [t/Symbol & :optional {:exclude (Seqable t/Symbol)
                                        :only (Seqable t/Symbol)
                                        :rename (t/Map t/Symbol t/Symbol)}
                    -> nil]

clojure.core/*loaded-libs* (t/Ref1 (t/Set t/Symbol))

clojure.core/seq? (Pred (t/Seq Any))
clojure.core/set? (Pred (t/Set Any))
clojure.core/vector? (Pred (t/Vec Any))
clojure.core/nil? (Pred nil)
clojure.core/false? (Pred false)
clojure.core/true? (Pred true)
clojure.core/zero?  [Number -> Boolean
                     :filters {:then (is (Value 0) 0)
                               :else (!  (Value 0) 0)}]
clojure.core/symbol? (Pred t/Symbol)
clojure.core/keyword? (Pred t/Keyword)
clojure.core/map? (Pred (t/Map Any Any))

; would be nice
; (Pred (Not nil))
clojure.core/some? [Any -> Boolean :filters {:then (! nil 0)
                                             :else (is nil 0)}]
)
    (h/var-mappings
      this-ns

clojure.core/cast (All [x] [Class x -> x])

clojure.core/associative? (Pred (clojure.lang.Associative Any Any Any))
clojure.core/coll? (Pred (t/Coll Any))
      ;TODO should these be parameterised?
clojure.core/sequential? (Pred t/Sequential)
;clojure.core/sorted? (Pred Sorted)
clojure.core/meta [Any -> (U nil (t/Map Any Any))]
clojure.core/with-meta (All [[x :< clojure.lang.IObj]]
                            [x (U nil (t/Map Any Any)) -> x])
clojure.core/vary-meta (All [[x :< clojure.lang.IObj] b ...]
                            [x [(U nil (t/Map Any Any)) b ... b -> (U nil (t/Map Any Any))] b ... b -> x])

clojure.core/reset-meta! [clojure.lang.IReference (U nil (t/Map Any Any)) -> (U nil (t/Map Any Any))]
clojure.core/alter-meta! 
      (All [b ...]
      [clojure.lang.IReference [(U nil (t/Map Any Any)) b ... b -> (U nil (t/Map Any Any))] b ... b -> (U nil (t/Map Any Any))])

clojure.core/commute
      (All [w r b ...] 
           [(t/Ref2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/alter
      (All [w r b ...] 
           [(t/Ref2 w r) [r b ... b -> w] b ... b -> w])

clojure.core/cycle
      (All [x]
           [(U nil (Seqable x)) -> (t/ASeq x)])

clojure.core/compile [t/Symbol -> t/Symbol]

clojure.core/comparator
      (All [x y]
           [[x y -> Any] -> (I Comparator [x y -> t/AnyInteger])])

clojure.core/destructure [Any -> Any]

clojure.core/distinct (All [x] [(U nil (Seqable x)) -> (t/ASeq x)])

clojure.core/string? (Pred String)
clojure.core/char? (Pred Character)

clojure.string/split
     (IFn [String java.util.regex.Pattern -> (t/AVec String)]
         [String java.util.regex.Pattern t/AnyInteger -> (t/AVec String)])

clojure.string/join
     (IFn [(t/Option (Seqable Any)) -> String]
         [Any (t/Option (Seqable Any)) -> String])

clojure.string/upper-case
      [CharSequence -> String]

clojure.string/blank? [(U nil String) -> Boolean]
clojure.string/capitalize [String -> String]
clojure.string/lower-case [String -> String]
clojure.string/replace (IFn [String String String -> String]  [String Character Character -> String]  [String java.util.regex.Pattern (U String [String -> String]) -> String] )
clojure.string/replace-first (IFn [String String String -> String]  [String Character Character -> String]  [String java.util.regex.Pattern (U String [String -> String]) -> String] )
clojure.string/reverse [String -> String]
clojure.string/trim [String -> String]
clojure.string/trimr [String -> String]
clojure.string/triml [String -> String]

 clojure.data/diff [Any Any -> '[Any Any Any]]
 clojure.instant/read-instant-date [String -> java.util.Date]
 clojure.instant/read-instant-calendar [String -> java.util.GregorianCalendar]
 clojure.instant/read-instant-timestamp [String -> java.sql.Timestamp]
 clojure.repl/apropos [(U String java.util.regex.Pattern) -> (t/Seq t/Symbol)]
 clojure.repl/demunge [String -> String]
 clojure.repl/source-fn [t/Symbol -> (U String nil)]
 clojure.template/apply-template [(t/Vec Any) Any (U nil (Seqable Any)) -> Any]
 clojure.set/difference (All [x] [(t/Set x) (t/Set Any) * -> (t/Set x)])
 clojure.set/subset? [(t/Set Any) (t/Set Any) -> Boolean]
 clojure.set/superset? [(t/Set Any) (t/Set Any) -> Boolean]
 clojure.set/join (IFn [(t/Set (t/Map Any Any)) (t/Set (t/Map Any Any)) -> (t/Set (t/Map Any Any))]
                       [(t/Set (t/Map Any Any)) (t/Set (t/Map Any Any)) (t/Map Any Any) -> (t/Set (t/Map Any Any))])

 ; would be nice
;(All [[m :> (t/Map Any Any)] k]
;     [(t/Set m) (U nil (Seqable k)) -> (t/Map (t/Map k (Get m k)) (t/Set m))]
;     )
 clojure.set/index (All [x y]
                    [(t/Set (t/Map x y)) (U nil (Seqable Any)) -> (t/Map (t/Map Any Any) (t/Set (t/Map x y)))]
                    )
 clojure.set/map-invert (All [a b] [(t/Map a b) -> (t/Map b a)])

 ;would be nice, not quite correct though
; (All [x y [m :< (t/Map x y)] k]
;    [(t/Set m) (t/Vec k) -> (t/Set (t/Map k (Get m k)))])
 clojure.set/project (All [x y]
                      [(t/Set (t/Map x y)) (t/Vec Any) -> (t/Set (t/Map x y))])
 clojure.set/rename (All [x y]
                      [(t/Set (t/Map x y)) (t/Map Any x) -> (t/Set (t/Map x y))])
 clojure.set/rename-keys (All [x y]
                          [(t/Map x y) (t/Map Any x) -> (t/Map x y)])
 ;like filter
clojure.set/select (All [x y]
                           (IFn
                             [[x -> Any :filters {:then (is y 0)}] (t/Set x) -> (t/Set y)]
                             [[x -> Any :filters {:then (! y 0)}] (t/Set x) -> (t/Set (I x (Not y)))]
                             [[x -> Any] (t/Set x) -> (t/Set x)]))
 
; FIXME should be [String [Any -> Any] -> String]
clojure.string/escape [String (U (t/Map Any Any) [Any -> Any]) -> String]
clojure.string/split-lines [String -> (t/Vec String)]

clojure.test/function? [Any -> Boolean]
clojure.test/assert-any [Any Any -> Any]
clojure.test/do-report [Any -> Any]
clojure.test/run-tests [t/Symbol * -> (t/Map Any Any)]
clojure.test/run-all-tests (IFn [-> (t/Map Any Any)]
                                [java.util.regex.Pattern * -> (t/Map Any Any)])
clojure.test/successful? [(U nil (t/Map Any Any)) -> Boolean]
clojure.test/compose-fixtures [[[-> Any] -> Any] [[-> Any] -> Any] -> [[-> Any] -> Any]]
clojure.test/testing-vars-str [(t/Map Any Any) -> String]
clojure.test/testing-contexts-str [-> String]
clojure.test/test-ns [(U t/Namespace t/Symbol) -> (t/Map Any Any)]

clojure.test.tap/print-tap-plan [Any -> Any]
clojure.test.tap/print-tap-diagnostic [String -> Any]
clojure.test.tap/print-tap-pass [Any -> Any]
clojure.test.tap/print-tap-fail [Any -> Any]

clojure.java.javadoc/add-local-javadoc [Any -> (t/List Any)]
clojure.java.javadoc/add-remote-javadoc [String Any -> (t/Map Any Any)]
clojure.java.javadoc/javadoc [Any -> Any]

clojure.edn/read-string [(U String nil) -> Any]

clojure.java.shell/sh [Any *
                       ;would be nice (combine * and kw args)
                       ; String *
                       ;& :optional {:in Any  ;; any valid input to clojure.java.io/copy
                       ;             :inc-enc String :out-env (U ':bytes String)
                       ;             :env (U (Array String) (t/Map Any Any))
                       ;             :dir (U String java.io.File)}
                       -> '{:exit String
                            :out (U (Array byte) String)
                            :err String}]

clojure.java.browse/browse-url [Any -> Any]

clojure.java.io/delete-file (IFn [Any
                                  ;; FIXME any arg that c.j.io/file accepts
                                  #_String 
                                  -> Any]
                                 [Any Any -> Any])

clojure.stacktrace/e [-> Any]
clojure.stacktrace/print-cause-trace [Throwable -> Any]
clojure.stacktrace/print-stack-trace [Throwable -> Any]
clojure.stacktrace/print-throwable [Throwable -> Any]
clojure.stacktrace/root-cause [Throwable -> Throwable]

;; FIXME keyword arguments
clojure.reflect/reflect [Any Any * -> (t/Map Any Any)]

clojure.inspector/atom? [Any -> Boolean]
clojure.inspector/collection-tag [Any -> t/Keyword]
clojure.inspector/tree-model [Any -> Any]
clojure.inspector/old-table-model [(U nil (Seqable Any)) -> Any]
clojure.inspector/inspect [Any -> javax.swing.JFrame]
clojure.inspector/inspect-tree [Any -> javax.swing.JFrame]
clojure.inspector/inspect-table [(U nil (Seqable Any)) -> javax.swing.JFrame]

clojure.pprint/cl-format [(U java.io.Writer nil Boolean) String Any * -> (U nil String)]
clojure.pprint/fresh-line [-> Any]
clojure.pprint/get-pretty-writer [java.io.Writer -> java.io.Writer]

clojure.main/demunge [String -> String]
clojure.main/repl-prompt [-> Any]
clojure.main/repl-read [Any Any -> Any]
clojure.main/repl-caught [Throwable -> Any]
clojure.main/repl-exception [Throwable -> Any]
clojure.main/root-cause [Throwable -> Exception]
clojure.main/repl [& :optional {:init [-> Any]
                                :need-prompt [-> Any]
                                :prompt [-> Any]
                                :flush [-> Any]
                                :read [Any Any -> Any]
                                :eval [Any -> Any]
                                :print [Any -> Any]
                                :caught [Throwable -> Any]}
                   -> Any]
clojure.main/main [Any * -> Any]
clojure.main/load-script [String -> Any]

clojure.walk/keywordize-keys [Any -> Any]
clojure.walk/macroexpand-all [Any -> Any]
clojure.walk/postwalk [[Any -> Any] Any -> Any]
clojure.walk/postwalk-demo [Any -> Any]
clojure.walk/postwalk-replace [(t/Map Any Any) Any -> Any]
clojure.walk/prewalk [[Any -> Any] Any -> Any]
clojure.walk/prewalk-demo [Any -> Any]
clojure.walk/prewalk-replace [(t/Map Any Any) Any -> Any]
clojure.walk/stringify-keys [Any -> Any]
clojure.walk/walk [[Any -> Any] [Any -> Any] Any -> Any]

clojure.zip/zipper [[Any -> Any] [(U nil (Seqable Any)) -> (U nil (t/Seq Any))] 
                    [Any (U nil (t/Seq Any)) -> Any]
                    Any 
                    -> (t/Vec Any)]
clojure.zip/seq-zip [Any -> (t/Vec Any)]
clojure.zip/vector-zip [Any -> (t/Vec Any)]
clojure.zip/xml-zip [Any -> (t/Vec Any)]
clojure.zip/node [(t/Vec Any) -> Any]
clojure.zip/branch? [(t/Vec Any) -> Boolean]
clojure.zip/children [(t/Vec Any) -> (U nil (t/Seq Any))]
clojure.zip/root [(t/Vec Any) -> Any]
clojure.zip/rightmost [(t/Vec Any) -> (t/Vec Any)]
clojure.zip/right [(t/Vec Any) -> Any]
clojure.zip/up [(t/Vec Any) -> (U nil (t/Vec Any))]
clojure.zip/rights [(t/Vec Any) -> Any]
clojure.zip/replace [(t/Vec Any) Any -> (t/Vec Any)]
clojure.zip/down [(t/Vec Any) -> (U (t/Vec Any) nil)]
clojure.zip/left [(t/Vec Any) -> (U (t/Vec Any) nil)]
clojure.zip/lefts [(t/Vec Any) -> (U (t/Vec Any) nil)]
clojure.zip/leftmost [(t/Vec Any) -> (U (t/Vec Any) nil)]
clojure.zip/append-child [(t/Vec Any) Any -> (t/Vec Any)]
clojure.zip/branch? [(t/Vec Any) -> Boolean]
clojure.zip/end? [(t/Vec Any) -> Boolean]
clojure.zip/insert-child [(t/Vec Any) Any -> (t/Vec Any)]
clojure.zip/insert-left [(t/Vec Any) Any -> (t/Vec Any)]
clojure.zip/insert-right [(t/Vec Any) Any -> (t/Vec Any)]
clojure.zip/next [(t/Vec Any) -> (t/Vec Any)]
clojure.zip/prev [(t/Vec Any) -> (U (t/Vec Any) nil)]

;; more to say here
clojure.zip/path [(t/Vec Any) -> Any]

clojure.zip/remove [(t/Vec Any) -> (t/Vec Any)]

clojure.core/interpose (All [x] (IFn [x (t/Option (Seqable x)) -> (t/ASeq x)]))
clojure.core/interleave (All [x] [(t/Option (Seqable x)) (t/Option (Seqable x)) (t/Option (Seqable x)) * -> (t/ASeq x)])

clojure.core/repeat (All [x] 
                         (IFn [x -> (t/ASeq x)]
                             [t/AnyInteger x -> (t/ASeq x)]))

;clojure.core/every? (All [x y] 
;                         (IFn [[x -> Any :filters {:then (is y 0)}] (t/Coll x) -> Boolean
;                              :filters {:then (is (t/Coll (I x y)) 1)}]
;                             ; argument could be nil
;                             [[x -> Any :filters {:then (is y 0)}] (U nil (t/Coll x)) -> Boolean
;                              :filters {:then (is (U nil (t/Coll (I x y))) 1)}]
;                             [[x -> Any] (U nil (Seqable x)) -> Boolean]))
clojure.core/every? (All [x y]
                         (IFn [[x -> Any :filters {:then (is y 0)}] (t/Coll x) -> Boolean
                              :filters {:then (is (t/Coll y) 1)}]
                             ; argument could be nil
                             [[x -> Any :filters {:then (is y 0)}] (U nil (t/Coll x)) -> Boolean
                              :filters {:then (is (U nil (t/Coll y)) 1)}]
                             [[x -> Any] (U nil (Seqable x)) -> Boolean]))

clojure.core/range
(IFn [-> (t/ASeq t/AnyInteger)]
    [Number -> (t/ASeq t/AnyInteger)]
    [t/AnyInteger Number -> (t/ASeq t/AnyInteger)]
    [Number Number -> (t/ASeq Number)]
    [t/AnyInteger Number t/AnyInteger -> (t/ASeq t/AnyInteger)]
    [Number Number Number -> (t/ASeq Number)])

clojure.core/class (IFn [nil -> nil :object {:id 0 :path [Class]}]
                            [Object -> Class :object {:id 0 :path [Class]}]
                            [Any -> (t/Option Class) :object {:id 0 :path [Class]}])

; need better metadata support if this even has a chance of working
; like class
clojure.core/type [Any -> Any]

clojure.core/seq (All [x]
                        (IFn 
                          [(t/NonEmptyColl x) -> (t/NonEmptyASeq x)
                           :filters {:then tt
                                     :else ff}]
                          [(t/Option (t/Coll x)) -> (t/Option (t/NonEmptyASeq x))
                           :filters {:then (& (is t/NonEmptyCount 0)
                                              (! nil 0))
                                     :else (| (is nil 0)
                                              (is t/EmptyCount 0))}]
                          [(t/Option (Seqable x)) -> (t/Option (t/NonEmptyASeq x))]))

; Seqable [[x :variance :covariant]
;          :count [l :variance :covariant :< AnyCountRange]
;          :to-seq [sfn :kind (TFn [[x :variance :covariant]]
;                               (I IWithMeta (IMeta nil) (ISeq x) (ICollection x) 
;                                  IEmptyableCollection ISequential))]]

; clojure.core/seq (All [x
;                        [sfn :kind [* -> *]]
;                    (IFn
;                      [(Seqable x :count (CountRange 1) :to-seq sfn) -> (sfn x)]
;                      [(Seqable x :count AnyCountRange :to-seq sfn) -> (U nil (sfn x))]))

clojure.core/empty? (IFn [(t/Option (HSequential [Any *])) -> Boolean
                          :filters {:then (| (is t/EmptyCount 0)
                                             (is nil 0))
                                    :else (is t/NonEmptyCount 0)}]
                        [(t/Option (t/Coll Any)) -> Boolean
                          :filters {:then (| (is t/EmptyCount 0)
                                             (is nil 0))
                                    :else (is t/NonEmptyCount 0)}]
                        [(t/Option (Seqable Any)) -> Boolean])

clojure.core/map
     (All [c a b ...]
          (IFn [[a :-> c] :-> (t/Transducer a c)]
               [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
               [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (t/ASeq c)]))

clojure.core/mapv
     (All [c a b ...]
          (IFn [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyAVec c)]
               [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (t/AVec c)]))

clojure.core/mapcat
     (All [c a b ...]
       (IFn
          [[a :-> (t/Option (Seqable c))] :-> (t/Transducer a c)]
          [[a b ... b -> (t/Option (Seqable c))] (t/Option (Seqable a)) (t/Option (Seqable b)) ... b -> (t/ASeq c)]))

clojure.core/pmap
     (All [c a b ...]
          (IFn [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
              [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (t/ASeq c)]))

clojure.core/pcalls
      (All [r]
           [[-> r] * -> (t/ASeq r)])

#_#_
clojure.core/halt-when
(All [a d]
  [[a :-> t/Any] :-> (t/Transducer a a)]
  [[a :-> t/Any] (U nil [t/Any a :-> a]) :-> (t/Transducer a a)])

clojure.core/*clojure-version* '{:major Any
                                 :minor Any
                                 :incremental Any
                                 :qualifier Any}

clojure.core/clojure-version [-> String]

clojure.core/promise
        (All [x]
           [-> (t/Promise x)])

clojure.core/deliver (All [x] [(t/Promise x) x -> (U nil (t/Promise x))])

clojure.core/flatten [(U nil (Seqable Any)) -> (t/Seq Any)]

clojure.core/map-indexed
     (All [x y] [[t/AnyInteger x -> y] (t/Option (Seqable x)) -> (Seqable y)])

clojure.core/keep-indexed
     (All [a c] [[Number a -> (U nil c)] (Seqable a) -> (t/Seq c)])

clojure.core/merge-with
     (All [k v]
          (IFn [[v v -> v] nil * -> nil]
              [[v v -> v] (t/Map k v) * -> (t/Map k v)]
              [[v v -> v] (t/Option (t/Map k v)) * -> (t/Option (t/Map k v))]))

clojure.core/reduce
     (All [a c]
          (IFn 
            ;Without accumulator
            ; default
            ; (reduce + my-coll)
            [[a c -> (U (Reduced a) a)] (t/NonEmptySeqable c) -> a]
            [(IFn [a c -> (U (Reduced a) a)] [-> (U (Reduced a) a)]) (t/Option (Seqable c)) -> a]
            ; default
            ; (reduce + 3 my-coll)
            ; (reduce (fn [a b] a) (reduced 1) nil) 
            ; ;=> (reduced 1)
            [[a c -> (U (Reduced a) a)] a (t/Option (Seqable c)) -> a]))

clojure.core/reduce-kv
    (All [a c k v]
      [[a k v -> (U (Reduced a) a)] a (t/Option (Associative Any k v)) -> a])

clojure.core/reduced (All [x] [x -> (Reduced x)])
clojure.core/reduced? (Pred (Reduced Any))

#_(comment
  clojure.core/reduce
       (All [a c d]
            (IFn 
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
              [(IFn [c c -> c] [-> c]) (U nil (Seqable c)) -> c]
              ;With accumulator
              ; empty coll, f not called, returns accumulator
              ; (reduce + 3 []) => 3
              [Any a (U nil (I (ExactCount 0) (Seqable Any))) -> a]
              ; default
              ; (reduce + 3 my-coll)
              [[a c -> a] a (U nil (Seqable c)) -> a]))
  )

;should be special cased
clojure.core/not= [Any Any * -> Boolean]

clojure.core/first
     (All [x]
          (IFn [(HSequential [x Any *]) -> x
                :object {:id 0 :path [(Nth 0)]}]
               [(t/Option (t/EmptySeqable x)) -> nil]
               [(t/NonEmptySeqable x) -> x]
               [(t/Option (Seqable x)) -> (t/Option x)]))

clojure.core/second
     (All [x]
          (IFn [(HSequential [Any x Any *]) -> x
                :object {:id 0 :path [(Nth 1)]}]
               [(t/Option (I (Seqable x) (CountRange 0 1))) -> nil]
               [(I (Seqable x) (CountRange 2)) -> x]
               [(t/Option (Seqable x)) -> (t/Option x)]))

clojure.core/ffirst
     (All [x]
          [(t/Option (Seqable (U nil (Seqable x)))) -> (t/Option x)])

clojure.core/nfirst
(All [x]
     [(t/Option (Seqable (t/Option (Seqable x)))) -> (t/Option (t/NonEmptyASeq x))])

clojure.core/group-by
(All [x y]
     [[x -> y] (U nil (Seqable x)) -> (t/Map y (t/Vec x))])

clojure.core/fnext
(All [x]
     [(t/Option (Seqable (t/Option (Seqable x)))) -> (t/Option x)])

clojure.core/nnext
(All [x]
     [(t/Option (Seqable x)) -> (t/Option (t/NonEmptyASeq x))])

clojure.core/nthnext
(All [x]
     (IFn [nil t/AnyInteger -> nil]
          [(t/Option (Seqable x)) t/AnyInteger -> (t/Option (t/NonEmptyASeq x))]))

clojure.core/rest
     (All [x]
          [(t/Option (Seqable x)) -> (t/ASeq x)])

clojure.core/last
     (All [x]
          (IFn [(t/NonEmptySeqable x) -> x]
               [(t/Option (Seqable x)) -> (U nil x)]))

clojure.core/butlast
     (All [x]
          [(t/Option (Seqable x)) -> (t/ASeq x)])

clojure.core/next
     (All [x]
          (IFn [(t/Option (t/Coll x)) -> (t/Option (t/NonEmptyASeq x))
               :filters {:then (& (is (CountRange 2) 0)
                                  (! nil 0))
                         :else (| (is (CountRange 0 1) 0)
                                  (is nil 0))}]
              [(t/Option (Seqable x)) -> (t/Option (t/NonEmptyASeq x))]))

clojure.core/into
      (All [x y :named [a]]
           (IFn [(t/Map x y) (U nil (Seqable (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]))) -> (t/Map x y)]
               [(t/Vec x) (U nil (Seqable x)) -> (t/Vec x)]
               [(t/Set x) (U nil (Seqable x)) -> (t/Set x)]
               [(t/Coll Any) (U nil (Seqable Any)) -> (t/Coll Any)]
               ; transducer arities
               [(t/Map x y) (t/Transducer a (U nil '[x y])) (U nil (Seqable a)) -> (t/Map x y)]
               [(t/Vec x) (t/Transducer y x) (U nil (Seqable y)) -> (t/Vec x)]
               [(t/Set x) (t/Transducer y x) (U nil (Seqable y)) -> (t/Set x)]
               [(t/Coll Any) (t/Transducer y Any) (U nil (Seqable y)) -> (t/Coll Any)]))

clojure.core/conj
;     (All [e
;           [Arg :< (TFn [[x :variance :covariant]] Any)]
;           [Res :< (TFn [[x :variance :covariant]]
;                     (t/Coll Any))]]
;          (IFn [(clojure.lang.IPersistentCollection e Arg Res) (Arg e) (Arg e) * -> (Res e)]
;              [nil e e * -> (clojure.lang.PersistentList e)]))


     (All [x y]
          (IFn [(IPersistentVector x) x x * -> (IPersistentVector x)]
              [(APersistentMap x y)
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) *
               -> (APersistentMap x y)]
              [(IPersistentMap x y)
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
               (U nil (Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) * -> (IPersistentMap x y)]
              [(IPersistentSet x) x x * -> (IPersistentSet x)]
              [(t/ASeq x) x x * -> (t/ASeq x)]
              [nil x x * -> (clojure.lang.PersistentList x)]
              [(t/Coll Any) Any Any * -> (t/Coll Any)]
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

clojure.core/sequence
       (All [a b]
         (IFn [(U nil (Seqable a)) -> (t/Seq a)]
              [(t/Transducer a b) (Seqable a) :-> (Seqable b)]))
clojure.core/find
     (All [x y]
          [(U nil (clojure.lang.Associative Any x y)) Any -> (U nil (HVec [x y]))])

; same as clojure.lang.RT/get
clojure.core/get
     (All [x y]
          (IFn 
            ;no default
            [(U nil (t/Set x) (ILookup Any x)) Any -> (t/Option x)]
            [(t/Option java.util.Map) Any -> Any]
            [(t/Option String) Any -> (t/Option Character)]
            ;default
            [(U nil (t/Set x) (ILookup Any x)) Any y -> (U y x)]
            [(t/Option java.util.Map) Any y -> (U y Any)]
            [(t/Option String) Any y -> (U y Character)]
            ))
)
    (h/var-mappings
      this-ns

clojure.core/get-in
    (IFn [Any (U nil (Seqable Any)) -> Any]
        [Any (U nil (Seqable Any)) Any -> Any])

clojure.core/assoc-in
    [(U nil (Associative Any Any Any)) (Seqable Any) Any -> Any]

;FIXME maps after the first can always be nil
clojure.core/merge 
     (All [k v]
          (IFn [nil * -> nil]
              [(IPersistentMap k v) (IPersistentMap k v) * -> (IPersistentMap k v)]
              [(t/Option (IPersistentMap k v)) * -> (t/Option (IPersistentMap k v))]))

;more to be said here?
clojure.core/contains? [(t/Option (Seqable Any)) Any -> Boolean]

clojure.core/= [Any Any * -> (U true false)]
clojure.core/identical? [Any Any -> Boolean]
clojure.core/distinct? [Any Any * -> Boolean]

clojure.core/decimal? (Pred BigDecimal)

clojure.core/denominator [clojure.lang.Ratio -> Number]

clojure.core/mod (IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                      [Number Number -> Number])

clojure.core/var-get (All [r] [(t/Var2 Nothing r) -> r])
clojure.core/var-set (All [w] [(t/Var2 w Any) w -> w])

clojure.core/supers [Class -> (U nil (I t/NonEmptyCount (t/Set Class)))]

clojure.core/take-nth (All [x] [t/AnyInteger (U nil (Seqable x)) -> (t/ASeq x)])

clojure.core/shuffle (All [x] 
                          (IFn [(I (Collection x) (Seqable x)) -> (t/Vec x)]
                               [(Collection x) -> (t/Vec x)]))

clojure.core/special-symbol? [Any -> Boolean]

clojure.core/integer? (Pred t/AnyInteger)
clojure.core/number? (Pred Number)
clojure.core/var? (Pred (t/Var2 Nothing Any))
clojure.core/class? (Pred Class)

clojure.core/resolve (IFn [t/Symbol -> (U (t/Var2 Nothing Any) Class nil)]
                         ; should &env arg be more accurate?
                         [Any t/Symbol -> (U (t/Var2 Nothing Any) Class nil)])

clojure.core/ns-resolve (IFn [(U t/Symbol t/Namespace) t/Symbol -> (U (t/Var2 Nothing Any) Class nil)]
                            ; should &env arg be more accurate?
                            [(U t/Symbol t/Namespace) Any t/Symbol -> (U (t/Var2 Nothing Any) Class nil)])

clojure.core/extenders [Any -> (U nil (Seqable (U Class nil)))]

clojure.core/+ (IFn [Long * -> Long]
                    [(U Long Double) * -> Double]
                    [t/AnyInteger * -> t/AnyInteger]
                    [Number * -> Number])
clojure.core/- (IFn [Long Long * -> Long]
                    [(U Long Double) (U Long Double) * -> Double]
                    [t/AnyInteger t/AnyInteger * -> t/AnyInteger]
                    [Number Number * -> Number])
clojure.core/* (IFn [Long * -> Long]
                    [(U Long Double) * -> Double]
                    [t/AnyInteger * -> t/AnyInteger]
                    [Number * -> Number])
clojure.core// (IFn [Double Double * -> Double]
                    [Number Number * -> Number])

clojure.core/+' (IFn [t/AnyInteger * -> t/AnyInteger]
                     [Number * -> Number])
clojure.core/-' (IFn [t/AnyInteger t/AnyInteger * -> t/AnyInteger]
                     [Number Number * -> Number])
clojure.core/*' (IFn [t/AnyInteger * -> t/AnyInteger]
                    [Number * -> Number])
clojure.core/quot (IFn [Long Long -> Long]
                       [(U Long Double) (U Long Double) -> Double]
                       [t/AnyInteger t/AnyInteger -> t/AnyInteger] 
                       [Number Number -> Number])

clojure.core/unchecked-inc (IFn [t/AnyInteger -> t/AnyInteger]
                                [Number -> Number])
clojure.core/unchecked-inc-int [Number -> t/AnyInteger]
clojure.core/unchecked-dec (IFn [t/AnyInteger -> t/AnyInteger]
                                [Number -> Number])
clojure.core/unchecked-dec-int [Number -> t/AnyInteger]
clojure.core/unchecked-subtract (IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                     [Number Number -> Number])
clojure.core/unchecked-subtract-int [Number Number -> t/AnyInteger]
clojure.core/unchecked-negate (IFn [t/AnyInteger -> t/AnyInteger]
                                   [Number -> Number])
clojure.core/unchecked-negate-int [Number -> t/AnyInteger]
clojure.core/unchecked-add (IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                [Number Number -> Number])
clojure.core/unchecked-add-int [Number Number -> t/AnyInteger]
clojure.core/unchecked-multiply (IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                     [Number Number -> Number])
clojure.core/unchecked-multiply-int [Number Number -> t/AnyInteger]
clojure.core/unchecked-divide-int [Number Number -> t/AnyInteger]
clojure.core/unchecked-remainder-int [Number Number -> t/AnyInteger]
clojure.core/inc (IFn [Long -> Long]
                      [Double -> Double]
                      [t/AnyInteger -> t/AnyInteger]
                      [Number -> Number])
clojure.core/dec (IFn [Long -> Long]
                      [Double -> Double]
                      [t/AnyInteger -> t/AnyInteger]
                      [Number -> Number])

clojure.core/inc' (IFn [t/AnyInteger -> t/AnyInteger]
                       [Number -> Number])
clojure.core/dec' (IFn [t/AnyInteger -> t/AnyInteger]
                          [Number -> Number])

clojure.core/rationalize [Number -> Number]

clojure.core/bit-not [t/AnyInteger -> t/AnyInteger]
clojure.core/bit-and [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-or [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-xor [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-and-not [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
clojure.core/bit-clear [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-set [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-flip [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-test [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-shift-left [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/bit-shift-right [t/AnyInteger t/AnyInteger -> t/AnyInteger]
clojure.core/unsigned-bit-shift-right [t/AnyInteger t/AnyInteger -> t/AnyInteger]

clojure.core/even? [t/AnyInteger -> Boolean]
clojure.core/odd? [t/AnyInteger -> Boolean]

clojure.core/peek (All [x]
                       (IFn [(I t/NonEmptyCount (t/Stack x)) -> x]
                           [(t/Stack x) -> x]))
clojure.core/pop (All [x]
                      (IFn
                        [(t/List x) -> (t/List x)]
                        [(t/Vec x) -> (t/Vec x)]
                        [(t/Stack x) -> (t/Stack x)]))

clojure.core/get-thread-bindings
    [-> (t/Map (t/Var2 Nothing Any) Any)]
clojure.core/bound-fn*
    (All [r b ...]
         [[b ... b -> r] -> [b ... b -> r]])

clojure.core/find-var
    [t/Symbol -> (U nil (t/Var2 Nothing Any))]

clojure.core/agent
    (All [x] [x & :optional {:validator (U nil [x -> Any]) :meta Any
                             :error-handler (U nil [(t/Agent1 x) Throwable -> Any])
                             :error-mode (U ':continue ':fail)} 
              -> (t/Agent1 x)])

clojure.core/set-agent-send-executor!
    [java.util.concurrent.ExecutorService -> Any]

clojure.core/set-agent-send-off-executor!
    [java.util.concurrent.ExecutorService -> Any]

clojure.core/send-via (All [w r b ...] 
                           [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])

clojure.core/send (All [w r b ...] 
                           [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])

clojure.core/send-off (All [w r b ...] 
                           [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])

clojure.core/await [(t/Agent2 Nothing Any) * -> nil]
clojure.core/await-for [t/AnyInteger (t/Agent2 Nothing Any) * -> Boolean]
clojure.core/await1 (All [w r] [(t/Agent2 w r) -> (t/Agent2 w r)])

clojure.core/release-pending-sends [-> t/AnyInteger]

clojure.core/add-watch
        (All [x [a :< (IRef Nothing x)]]
             (IFn 
               ; this arity remembers the type of reference we pass to the function
               [a Any [Any a x x -> Any] -> Any]
               ; if the above cannot be inferred, 
               [(IRef Nothing x) Any [Any (IRef Nothing x) x x -> Any] -> Any]))

clojure.core/remove-watch [(IRef Nothing Any) Any -> Any]

clojure.core/agent-error [(t/Agent2 Nothing Any) -> (U nil Throwable)]

clojure.core/restart-agent
(All [w]
     ; w is invariant
     [(t/Agent2 w Any) w & :optional {:clear-actions Any} -> Any])

clojure.core/set-error-handler!
(All [w r]
    [(t/Agent2 w r) [(t/Agent2 w r) Throwable -> Any] -> Any])

clojure.core/error-handler
(All [w r]
    [(t/Agent2 w r) -> (U nil [(t/Agent2 w r) Throwable -> Any])])

clojure.core/set-error-mode!
    [(t/Agent2 Nothing Any) (U ':fail ':continue) -> Any]

clojure.core/error-mode
    [(t/Agent2 Nothing Any) -> Any]

clojure.core/agent-errors
    [(t/Agent2 Nothing Any) -> (U nil (t/ASeq Throwable))]
clojure.core/clear-agent-errors
    [(t/Agent2 Nothing Any) -> Any]

clojure.core/shutdown-agents [-> Any]

clojure.core/take
     (All [x]
       (IFn [t/Int :-> (t/Transducer x x)]
            [t/AnyInteger (U nil (Seqable x)) -> (t/ASeq x)]))

clojure.core/drop
     (All [x]
       (IFn [t/Int :-> (t/Transducer x x)]
            [t/AnyInteger (U nil (Seqable x)) -> (t/ASeq x)]))

clojure.core/take-last
     (All [x]
       [t/AnyInteger (U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)])

clojure.core/drop-last
     (All [x]
       [t/AnyInteger (U nil (Seqable x)) -> (t/ASeq x)])

clojure.core/hash [Any -> t/AnyInteger]
clojure.core/hash-combine [t/AnyInteger Any -> t/AnyInteger]

clojure.core/ifn? (Pred clojure.lang.IFn)
clojure.core/fn? (Pred t/Fn)

clojure.core/instance? [Class Any -> Boolean]

clojure.core/cons
     (All [x]
       [x (t/Option (Seqable x)) -> (t/ASeq x)])

clojure.core/reverse
     (All [x]
       [(t/Option (Seqable x)) -> (t/ASeq x)])

clojure.core/rseq
     (All [x]
       [(clojure.core.typed/Reversible x) -> (t/Option (t/NonEmptyASeq x))])

;coercions
clojure.core/bigdec [(U String Number) -> BigDecimal]
clojure.core/bigint [(U String Number) -> clojure.lang.BigInt]
clojure.core/biginteger [(U String Number) -> java.math.BigInteger]
clojure.core/boolean [Any -> Boolean]
clojure.core/byte [(U Character Number) -> Byte]
clojure.core/char [(U Character Number) -> Character]
clojure.core/double [Number -> Double]
clojure.core/float [Number -> Float]
clojure.core/int [(U Character Number) -> Integer]
clojure.core/long [(U Character Number) -> Long]
clojure.core/num [Number -> Number]
clojure.core/short [(U Character Number) -> Short]

;array ctors
clojure.core/boolean-array (IFn [(U nil Number (Seqable Boolean)) -> (Array boolean)]
                                    [Number (U nil Boolean (Seqable Boolean)) -> (Array boolean)])
clojure.core/byte-array (IFn [(U nil Number (Seqable Byte)) -> (Array byte)]
                                 [Number (U nil Byte (Seqable Byte)) -> (Array byte)])
clojure.core/char-array (IFn [(U nil Number (Seqable Character)) -> (Array char)]
                            [Number (U nil Number (Seqable Character)) -> (Array char)])
clojure.core/short-array (IFn [(U nil Number (Seqable Short)) -> (Array short)]
                                  [Number (U nil Short (Seqable Short)) -> (Array short)])
clojure.core/int-array (IFn [(U nil Number (Seqable Number)) -> (Array int)]
                                [Number (U nil Number (Seqable Number)) -> (Array int)])
clojure.core/double-array (IFn [(U nil Number (Seqable Number)) -> (Array double)]
                                   [Number (U nil Number (Seqable Number)) -> (Array double)])

;cast to java array
clojure.core/booleans [Any -> (Array boolean)]
clojure.core/bytes [Any -> (Array byte)]
clojure.core/chars [Any -> (Array char)]
clojure.core/shorts [Any -> (Array short)]
clojure.core/ints [Any -> (Array int)]
clojure.core/longs [Any -> (Array long)]
clojure.core/floats [Any -> (Array float)]
clojure.core/doubles [Any -> (Array double)]

clojure.core/max-key (All [x] 
                          [[x -> Number] x x x * -> x])
clojure.core/min-key (All [x] 
                          [[x -> Number] x x x * -> x])

clojure.core/< [Number Number * -> Boolean]

clojure.core/<= [Number Number * -> Boolean]

clojure.core/> [Number Number * -> Boolean]

clojure.core/>= [Number Number * -> Boolean]

clojure.core/== [Number Number * -> Boolean]

clojure.core/max (IFn [Long Long * -> Long]
                      [Double Double * -> Double]
                      [Number Number * -> Number])
clojure.core/min (IFn [Long Long * -> Long]
                      [Double Double * -> Double]
                      [Number Number * -> Number])

clojure.core/ref (All [x] [x & :optional {:validator (U nil [x -> Any]) :meta (U nil (t/Map Any Any))
                                          :min-history (U nil t/AnyInteger)
                                          :max-history (U nil t/AnyInteger)}
                           -> (clojure.lang.Ref x x)])

clojure.core/rand (IFn [-> Number]
                      [Number -> Number])

clojure.core/rand-int [t/Int -> t/Int]

clojure.core/ex-info (IFn [(U nil String) (t/Map Any Any) -> t/ExInfo]
                         [(U nil String) (t/Map Any Any) (U nil Throwable) -> t/ExInfo])

clojure.core/ex-data (IFn [t/ExInfo -> (t/Map Any Any)]
                         [Any -> (U nil (t/Map Any Any))])


;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (Pred (IChunk Any)).
clojure.core/chunked-seq? [Any -> Any]
clojure.core/chunk-first 
     (All [x]
          ;should be IChunkedSeq -> IChunk
          [(Seqable x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-rest
     (All [x]
          ;should be IChunkRest -> t/Seq
          [(clojure.lang.Seqable x) -> (t/ASeq x)])
clojure.core/chunk-buffer
     (All [x]
          [(U Integer Long) -> (clojure.lang.ChunkBuffer x)])
clojure.core/chunk
     (All [x]
          [(clojure.lang.ChunkBuffer x) -> (clojure.lang.IChunk x)])
clojure.core/chunk-cons
     (All [x]
          [(clojure.lang.IChunk x) (t/Option (Seqable x)) -> (t/Option (Seqable x))])
clojure.core/chunk-append
     (All [x]
          [(clojure.lang.ChunkBuffer x) x -> Any])
;;END CHUNK HACKS


clojure.core/subvec (All [x] 
                     (IFn [(t/Vec x) t/AnyInteger -> (t/Vec x)]
                         [(t/Vec x) t/AnyInteger t/AnyInteger -> (t/Vec x)]))

clojure.core/alias [t/Symbol t/Symbol -> nil]
clojure.core/all-ns [-> (t/Nilable (t/NonEmptyASeq t/Namespace))]

clojure.core/*file* String
clojure.core/*command-line-args* (U nil (t/NonEmptyASeq String))
clojure.core/*warn-on-reflection* Boolean
clojure.core/*compile-path* String
clojure.core/*compile-files* Boolean
clojure.core/*unchecked-math* Boolean
clojure.core/*compiler-options* (t/Map Any Any)
clojure.core/*in* java.io.Reader
clojure.core/*out* java.io.Writer
clojure.core/*err* java.io.Writer
clojure.core/*flush-on-newline* Boolean
clojure.core/*print-meta* Boolean
clojure.core/*print-dup* Boolean
clojure.core/*print-readably* Boolean
clojure.core/*read-eval* (U ':unknown Boolean)

clojure.core/trampoline 
       (All [r b ...]
         [[b ... b -> (Rec [f] (U r [-> (U f r)]))]
          b ... b -> r])


;; math.numeric-tower

clojure.math.numeric-tower/floor
(IFn [t/AnyInteger -> t/AnyInteger]
    [Number -> Number])

clojure.math.numeric-tower/abs
(IFn [t/AnyInteger -> t/AnyInteger]
    [Number -> Number])

;; core.match

clojure.core.match/backtrack Exception

clojure.core/eval [Any -> Any]
clojure.core/rand-nth (All [x] [(U (Indexed x) (t/SequentialSeqable x)) -> x])

clojure.pprint/pprint (IFn [Any -> nil]
                           [Any java.io.Writer -> nil])

      )
(h/var-mappings
  this-ns
clojure.set/union (All [x] [(t/Set x) * -> (t/Set x)])
clojure.set/intersection (All [x] [(t/Set x) (t/Set x) * -> (t/Set x)])
clojure.set/difference (All [x] [(t/Set x) (t/Set Any) * -> (t/Set x)])


clojure.repl/pst (IFn [-> nil]
                      [(U t/Int Throwable) -> nil]
                      [Throwable t/Int -> nil])
clojure.repl/print-doc [t/Symbol -> Any]
clojure.repl/find-doc [(U String java.util.regex.Pattern) -> Any]
clojure.repl/source-fn [Any -> (U nil String)]
clojure.java.javadoc/javadoc [Object -> Any]
complete.core/completions
(IFn [Any -> Any]
     [Any Any -> Any])
  )
    {'clojure.core/count (count-type)
     'clojure.core/aset-boolean (aset-*-type 'boolean)
     'clojure.core/aset-byte (aset-*-type 'byte)
     'clojure.core/aset-char (aset-*-type 'char)
     'clojure.core/aset-short (aset-*-type 'short)
     'clojure.core/aset-int (aset-*-type 'int)
     'clojure.core/aset-long (aset-*-type 'long)
     'clojure.core/aset-float (aset-*-type 'float)
     'clojure.core/aset-double (aset-*-type 'double)
     'clojure.core/nth (nth-type)
     }
))


;(comment
;  (aget my-array 0 1 2)
;  (aget (aget my-array 0) 1 2)
;  (aget (aget (aget my-array 0) 1) 2)
;
;  (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
;       (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
;            (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
;                 (Associative t/Keyword Number)
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
;                   [(Array w _) t/AnyInteger v -> v]
;                   [(Array _ r) t/AnyInteger b ... b
;                    :recur (rec r b ... b)]))
;
;  clojure.core/aget 
;       (Label [rec]
;              (All [x :dotted [b]] 
;                   (IFn [(Array _ x) t/AnyInteger -> x]
;                       [(Array _ x) t/AnyInteger b ... b
;                        :recur 
;                        (rec x b ... b)])))
;
;  clojure.core/assoc 
;       (All [[h <: (IPersistentMap Any Any)]
;             a b e ...2]
;         [h k ...2 a b -> (Assoc h k ...2 a b)])
;
;       (Label [rec]
;              (All [[h :< (HMap {})] x y [k :< (I AnyValue t/Keyword)] [e :< k] :dotted [b]]
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
;              (IFn [(HMap {l v}) (Vector* k) [v a ... a -> r] a ... a -> (I x (HMap {l r}))]
;                  [(HMap {l r}) (Vector* k b ... b) [v a ... a -> e] a ... a
;                   :recur
;                   [r (Vector* b ... b) [v a ... a -> e] a ... a]])))
;
;  ;clojure.core/get-in 
;  ;     (Label [rec]
;  ;       (All [[x :< (U nil (Associative Any Any))] k :dotted [b]]
;  ;            (IFn [x (Vector*) -> x]
;  ;                [x (Vector*) _ -> x]
;  ;                [(U nil (Associative _ y) (Vector* k b ... b) a -> x
;  ;                ;TODO
;  ;                [(U nil (Associative Any y)) (Vector* k) -> (U nil x)]
;  ;                    ))))
;
;  clojure.core/partial 
;       (Label [rec]
;              (All [x [a :< x] r :dotted [b c]]
;                   (IFn [[x c ... c -> r] a -> [c ... c -> r]]
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
  ;(reset-alias-env!)
  (merge
    {'clojure.lang.RT/nth (nth-type)}
    (h/method-override-mappings

clojure.lang.RT/isReduced (Pred (Reduced Any))

clojure.lang.Indexed/nth
  (All [x y]
       (IFn [(Indexed x) t/AnyInteger -> x]
           [(Indexed x) t/AnyInteger y -> (U x y)]))


;what about combinations of references and primitives?
clojure.lang.RT/box
(All [x]
     (IFn [Character -> Character]
          [Integer -> Integer]
          [Short -> Short]
          [Boolean -> Boolean]
          [Byte -> Byte]
          [Long -> Long]
          [Float -> Float]
          [Double -> Double]
          [(U Byte Short Integer Long) -> t/AnyInteger]
          [(U Float Double) -> Number]
          [nil -> nil]
          [x -> x]))

clojure.lang.RT/booleanCast [Any -> Boolean]

clojure.lang.Numbers/char_array (IFn [(U nil Number (Seqable Character)) -> (Array char)]
                                    [Number (U Number (Seqable Character)) -> (Array char)])


clojure.lang.LockingTransaction/runInTransaction
                 (All [x]
                   [[-> x] -> x])

;array ops
clojure.lang.RT/alength [(ReadOnlyArray Any) -> Integer]

clojure.lang.RT/aget (All [o]
                        [(ReadOnlyArray o) Integer -> o])

clojure.lang.RT/aset (All [i o]
                          [(Array2 i o) t/AnyInteger i -> o])

;get
;same as clojure.core/get
clojure.lang.RT/get (All [x y]
                         (IFn 
                           ;no default
                           [(IPersistentSet x) Any -> (t/Option x)]
                           [nil Any -> nil]
                           [(t/Option (ILookup Any x)) Any -> (t/Option x)]
                           [java.util.Map Any -> (t/Option Any)]
                           [String Any -> (t/Option Character)]
                           ;default
                           [(IPersistentSet x) Any y -> (U y x)]
                           [nil Any y -> y]
                           [(t/Option (ILookup Any x)) Any y -> (U y x)]
                           [java.util.Map Any y -> (U y Any)]
                           [String Any y -> (U y Character)]))

;numbers
clojure.lang.Numbers/add (IFn [Long Long -> Long]
                              [Double Double -> Double]
                              [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                              [Number Number -> Number])
clojure.lang.Numbers/inc (IFn [Long -> Long]
                              [Double -> Double]
                              [t/AnyInteger -> t/AnyInteger]
                              [Number -> Number])
clojure.lang.Numbers/dec (IFn [Long -> Long]
                              [Double -> Double]
                              [t/AnyInteger -> t/AnyInteger]
                              [Number -> Number])
clojure.lang.Numbers/quotient (IFn [Long Long -> Long]
                                   [(U Long Double) (U Long Double) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [Number Number -> Number])
clojure.lang.Numbers/incP (IFn [Long -> (U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [t/AnyInteger -> t/AnyInteger]
                               [Number -> Number])
clojure.lang.Numbers/decP (IFn [Long -> (U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [t/AnyInteger -> t/AnyInteger]
                               [Number -> Number])
clojure.lang.Numbers/unchecked_inc (IFn [Long -> Long]
                                        [Double -> Double]
                                        [t/AnyInteger -> t/AnyInteger]
                                        [Number -> Number])
clojure.lang.Numbers/unchecked_dec (IFn [Long -> Long]
                                        [Double -> Double]
                                        [t/AnyInteger -> t/AnyInteger]
                                        [Number -> Number])
clojure.lang.Numbers/unchecked_int_inc [Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_dec [Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_negate [Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_subtract [Number Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_add [Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_minus (IFn 
                                       ; negate
                                       [Long -> Long]
                                       [Double -> Double]
                                       [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                       [Number Number -> Number]
                                       ; subtract
                                       [Long Long -> Long]
                                       [(U Long Double) (U Long Double) -> Double]
                                       [t/AnyInteger -> t/AnyInteger]
                                       [Number -> Number])
clojure.lang.Numbers/minus (IFn
                             ; negate
                             [Long -> Long]
                             [Double -> Double]
                             [t/AnyInteger -> t/AnyInteger]
                             [Number -> Number]
                             ;minus
                             [Long Long -> Long]
                             [(U Double Long) (U Double Long) -> Long]
                             [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                             [Number Number -> Number])
clojure.lang.Numbers/unchecked_multiply (IFn [Long Long -> Long]
                                             [(U Long Double) (U Long Double) -> Double]
                                             [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                             [Number Number -> Number])
clojure.lang.Numbers/unchecked_int_multiply [Number Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_divide [Number Number -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_remainder [Number Number -> t/AnyInteger]
clojure.lang.Numbers/remainder [Number Number -> t/AnyInteger]
clojure.lang.Numbers/multiply (IFn [Long Long -> Long]
                                   [(U Double Long) (U Double Long) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [Number Number -> Number])
clojure.lang.Numbers/divide (IFn [Long Long -> Long]
                                   [(U Double Long) (U Double Long) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [Number Number -> Number])
      ;bit-not
clojure.lang.Numbers/not [t/AnyInteger -> Long]
;bit-and
clojure.lang.Numbers/and [t/AnyInteger t/AnyInteger -> Long]
;bit-or
clojure.lang.Numbers/or [t/AnyInteger t/AnyInteger -> Long]
;bit-xor
clojure.lang.Numbers/xor [t/AnyInteger t/AnyInteger -> Long]
;bit-and-not
clojure.lang.Numbers/andNot [t/AnyInteger t/AnyInteger -> Long]
; unsigned-bit-shift-right 
clojure.lang.Numbers/unsignedShiftRight [t/AnyInteger t/AnyInteger -> Long]

clojure.lang.Numbers/max (IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                           [Number Number -> Number])
clojure.lang.Numbers/min (IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                           [Number Number -> Number])

clojure.lang.Numbers/lt [Number Number -> Boolean]
clojure.lang.Numbers/lte [Number Number -> Boolean]
clojure.lang.Numbers/gt [Number Number -> Boolean]
clojure.lang.Numbers/gte [Number Number -> Boolean]

clojure.lang.Numbers/isZero [Number -> Boolean
                             :filters {:then (is (Value 0) 0)
                                       :else (!  (Value 0) 0)}]
clojure.lang.Numbers/isNeg [Number -> Boolean]
clojure.lang.Numbers/isPos [Number -> Boolean]

clojure.lang.Util/compare [Any Any -> Number]

; this is overloaded in interesting ways, but this is good enough for destructuring purposes
clojure.lang.PersistentHashMap/create [(U nil (ISeq Any) java.util.Map (ReadOnlyArray Object)) -> (t/Map Any Any)]

clojure.lang.RT/floatCast  [Number -> Float]
clojure.lang.RT/byteCast   [(U Character Number) -> Byte]
clojure.lang.RT/charCast   [(U Character Number) -> Character]
clojure.lang.RT/doubleCast [Number -> Double]
clojure.lang.RT/intCast    [(U Character Number) -> Integer]
clojure.lang.RT/longCast   [(U Character Number) -> Long]
clojure.lang.RT/shortCast  [(U Character Number) -> Short]

clojure.lang.RT/uncheckedFloatCast  [Number -> Float]
clojure.lang.RT/uncheckedByteCast   [(U Character Number) -> Byte]
clojure.lang.RT/uncheckedCharCast   [(U Character Number) -> Character]
clojure.lang.RT/uncheckedIntCast    [(U Character Number) -> Integer]
clojure.lang.RT/uncheckedLongCast   [(U Character Number) -> Long]
clojure.lang.RT/uncheckedShortCast  [(U Character Number) -> Short]

clojure.lang.Numbers/num   [Number -> Number]
    )
    {'clojure.lang.RT/count (count-type)
     }))

(comment
  clojure.lang.IFn/invoke (All [r a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 arest]
                               (IFn
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
  ;(reset-alias-env!)
  (h/ctor-override-mappings

clojure.lang.LazySeq (All [x]
                          [[-> (t/Option (Seqable x))] -> (LazySeq x)])
clojure.lang.Delay (All [x]
                        [[-> x] -> (clojure.lang.Delay x)])
    ))

;; not added in refresh
(delay-and-cache-env ^:private init-declared-kinds {})

;; not added in refresh
(delay-and-cache-env ^:private init-datatype-env {})

;; not added in refresh
(delay-and-cache-env ^:private init-datatype-ancestor-env {})

(let [reset-var-type-env! (delay (impl/dynaload 'clojure.core.typed.checker.var-env/reset-var-type-env!))
      reset-nonnilable-method-return-env!
      (delay (impl/dynaload 'clojure.core.typed.checker.jvm.method-return-nilables/reset-nonnilable-method-return-env!))
      reset-method-nilable-param-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.method-param-nilables/reset-method-nilable-param-env!))
      reset-method-override-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.method-override-env/reset-method-override-env!))
      reset-constructor-override-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.ctor-override-env/reset-constructor-override-env!))
      reset-protocol-env! (delay (impl/dynaload 'clojure.core.typed.checker.protocol-env/reset-protocol-env!))
      reset-declared-kinds! (delay (impl/dynaload 'clojure.core.typed.checker.declared-kind-env/reset-declared-kinds!))
      reset-datatype-env! (delay (impl/dynaload 'clojure.core.typed.checker.datatype-env/reset-datatype-env!))
      reset-datatype-ancestors! (delay (impl/dynaload 'clojure.core.typed.checker.datatype-ancestor-env/reset-datatype-ancestors!))]
  (defn reset-clojure-envs! []
    (impl/with-clojure-impl
      ;(reset-alias-env!)
      (base-rclass/reset-rclass-env!)
      (@reset-var-type-env! (init-var-env) (init-var-nochecks))
      (@reset-nonnilable-method-return-env! (init-method-nonnilable-return-env))
      (@reset-method-nilable-param-env! (init-method-nilable-param-env))
      (@reset-method-override-env! (init-method-override-env))
      (@reset-constructor-override-env! (init-ctor-override-env))
      (@reset-protocol-env! (init-protocol-env))
      (@reset-declared-kinds! (init-declared-kinds))
      (@reset-datatype-env! (init-datatype-env))
      (@reset-datatype-ancestors! (init-datatype-ancestor-env)))
    nil))

(let [merge-protocol-env! (delay (impl/dynaload 'clojure.core.typed.checker.protocol-env/merge-protocol-env!))
      refresh-var-type-env! (delay (impl/dynaload 'clojure.core.typed.checker.var-env/refresh-var-type-env!))
      merge-method-nilable-param-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.method-param-nilables/merge-method-nilable-param-env!))
      merge-nonnilable-method-return-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.method-return-nilables/merge-nonnilable-method-return-env!))
      merge-method-override-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.method-override-env/merge-method-override-env!))
      merge-constructor-override-env! (delay (impl/dynaload 'clojure.core.typed.checker.jvm.ctor-override-env/merge-constructor-override-env!))]
  (defn refresh-core-clojure-envs! []
    (impl/with-clojure-impl
      ;(refresh-core-alias-env!)
      (base-rclass/reset-rclass-env!)
      (@merge-protocol-env! (init-protocol-env))
      (@refresh-var-type-env! (init-var-env) (init-var-nochecks))
      (@merge-method-nilable-param-env! (init-method-nilable-param-env))
      (@merge-nonnilable-method-return-env! (init-method-nonnilable-return-env))
      (@merge-method-override-env! (init-method-override-env))
      (@merge-constructor-override-env! (init-ctor-override-env)))
    nil))
