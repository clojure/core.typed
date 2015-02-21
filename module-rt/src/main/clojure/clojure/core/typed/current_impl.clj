; untyped, clojure.core.typed depends on this namespace
(ns clojure.core.typed.current-impl
  (:require [clojure.core.typed.profiling :as p]
            [clojure.set :as set]))

(defonce var-env (atom {}))
(defonce alias-env (atom {}))
(defonce protocol-env (atom {}))
(defonce rclass-env (atom {}))
(defonce datatype-env (atom {}))
(defonce jsnominal-env (atom {}))

(defn v [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    @var))

(defn the-var [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]
   :post [(var? %)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    var))

(def clojure ::clojure)
(def clojurescript ::clojurescript)

(def any-impl ::any-impl)

(derive clojure any-impl)
(derive clojurescript any-impl)

(defonce ^:dynamic *current-impl* nil)
(set-validator! #'*current-impl* (some-fn nil? keyword?))

(defmacro with-impl [impl & body]
  `(do (assert ((some-fn #{~impl} nil?) *current-impl*) 
               (str "Cannot overlay different core.typed implementations: " (pr-str *current-impl*)
                    ", expected " (pr-str ~impl)))
     (binding [*current-impl* ~impl]
       ~@body)))

(defn clj-bindings []
  {(the-var 'clojure.core.typed.name-env/*current-name-env*)
   (v 'clojure.core.typed.name-env/CLJ-TYPE-NAME-ENV)
   (the-var 'clojure.core.typed.protocol-env/*current-protocol-env*)
   (v 'clojure.core.typed.protocol-env/CLJ-PROTOCOL-ENV)
   (the-var 'clojure.core.typed.ns-deps/*current-deps*)
   (v 'clojure.core.typed.ns-deps/CLJ-TYPED-DEPS)
   ; var env
   (the-var 'clojure.core.typed.var-env/*current-var-annotations*)
   (v 'clojure.core.typed.var-env/CLJ-VAR-ANNOTATIONS)
   (the-var 'clojure.core.typed.var-env/*current-nocheck-var?*)
   (v 'clojure.core.typed.var-env/CLJ-NOCHECK-VAR?)
   (the-var 'clojure.core.typed.var-env/*current-used-vars*)
   (v 'clojure.core.typed.var-env/CLJ-USED-VARS)
   (the-var 'clojure.core.typed.var-env/*current-checked-var-defs*)
   (v 'clojure.core.typed.var-env/CLJ-CHECKED-VAR-DEFS) 

   (the-var 'clojure.core.typed.declared-kind-env/*current-declared-kinds*)
   (v 'clojure.core.typed.declared-kind-env/CLJ-DECLARED-KIND-ENV) 
   (the-var 'clojure.core.typed.datatype-env/*current-datatype-env*)
   (v 'clojure.core.typed.datatype-env/CLJ-DATATYPE-ENV) 
   (the-var 'clojure.core.typed.datatype-ancestor-env/*current-dt-ancestors*)
   (v 'clojure.core.typed.datatype-ancestor-env/CLJ-DT-ANCESTOR-ENV)})

(defmacro with-clojure-impl [& body]
  `(with-impl clojure
     (with-bindings (clj-bindings)
       ~@body)))

(defn cljs-bindings []
  {(the-var 'clojure.core.typed.name-env/*current-name-env*)
   (v 'clojure.core.typed.name-env/CLJS-TYPE-NAME-ENV)
   (the-var 'clojure.core.typed.protocol-env/*current-protocol-env*)
   (v 'clojure.core.typed.protocol-env/CLJS-PROTOCOL-ENV)
   (the-var 'clojure.core.typed.ns-deps/*current-deps*)
   (v 'clojure.core.typed.ns-deps/CLJS-TYPED-DEPS)
   ; var env
   (the-var 'clojure.core.typed.var-env/*current-var-annotations*)
   (v 'clojure.core.typed.var-env/CLJS-VAR-ANNOTATIONS)
   (the-var 'clojure.core.typed.var-env/*current-nocheck-var?*)
   (v 'clojure.core.typed.var-env/CLJS-NOCHECK-VAR?)
   (the-var 'clojure.core.typed.var-env/*current-used-vars*)
   (v 'clojure.core.typed.var-env/CLJS-USED-VARS)
   (the-var 'clojure.core.typed.var-env/*current-checked-var-defs*)
   (v 'clojure.core.typed.var-env/CLJS-CHECKED-VAR-DEFS) 

   (the-var 'clojure.core.typed.declared-kind-env/*current-declared-kinds*)
   (v 'clojure.core.typed.declared-kind-env/CLJS-DECLARED-KIND-ENV) 
   (the-var 'clojure.core.typed.datatype-env/*current-datatype-env*)
   (v 'clojure.core.typed.datatype-env/CLJS-DATATYPE-ENV)
   (the-var 'clojure.core.typed.jsnominal-env/*current-jsnominal-env*)
   (v 'clojure.core.typed.jsnominal-env/JSNOMINAL-ENV)})

(defmacro with-cljs-impl [& body]
  `(with-impl clojurescript
     (with-bindings (cljs-bindings)
       ~@body)))

(defmacro with-full-impl [impl & body]
  `(with-impl ~impl
     (with-bindings (impl-case
                      :clojure (clj-bindings)
                      :cljs (cljs-bindings))
       ~@body)))


(defn implementation-specified? []
  (boolean *current-impl*))

(defn ensure-impl-specified []
  (assert (implementation-specified?) "No implementation specified"))

(defn current-impl []
  (ensure-impl-specified)
  *current-impl*)

(defn checking-clojure? []
  (ensure-impl-specified)
  (= clojure *current-impl*))

(defn checking-clojurescript? []
  (ensure-impl-specified)
  (= clojurescript *current-impl*))

(defn assert-clojure 
  ([] (assert-clojure nil))
  ([msg] (assert (= clojure *current-impl*) (str "Clojure implementation only"
                                                 (when (seq msg)
                                                   (str ": " msg))))))

(defn assert-cljs []
  (assert (= clojurescript *current-impl*) "Clojurescript implementation only"))

(defmacro impl-case [& {clj-case :clojure cljs-case :cljs :as opts}]
  (assert (= #{:clojure :cljs} (set (keys opts)))
          "Incorrect cases to impl-case")
  `(case (current-impl)
     ~clojure ~clj-case
     ~clojurescript ~cljs-case
     (assert nil (str "No case matched for impl-case" (current-impl)))))

(defn var->symbol [^clojure.lang.Var var]
  {:pre [(var? var)]
   :post [((every-pred symbol? namespace) %)]}
  (symbol (str (ns-name (.ns var)))
          (str (.sym var))))

(defn Class->symbol [^Class cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

; for type-contract
(defn hmap-c? [& {:keys [mandatory optional absent-keys complete?]}]
  (every-pred map?
              #(cond
                 complete? (set/subset? (set (keys %))
                                        (set (mapcat keys [mandatory optional])))
                 :else
                 (let [actual-ks (set (keys %))]
                   (and 
                     ;required keys is a subset of actual keys
                     (set/subset? 
                       (set (keys mandatory))
                       actual-ks)
                     ;no absent-keys are present
                     (empty?
                       (set/intersection
                         absent-keys
                         actual-ks)))))
              #(every? identity 
                       (for [[k vc] mandatory]
                         (and (contains? % k)
                              (vc (get % k)))))
              #(every? identity 
                       (for [[k vc] optional]
                         (or (not (contains? % k))
                             (vc (get % k)))))))

(def init-aliases
  '[
  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms '[AnyInteger]}
AnyInteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte)

    ^{:doc "A type that returns true for clojure.core/integer?"
      :forms '[Int]}
Int (U Integer Long clojure.lang.BigInt BigInteger Short Byte)
      ^{:doc "A type that returns true for clojure.core/number?"
        :forms '[Num]}
Num Number
      ^{:doc "A keyword"
        :forms '[Keyword]}
Keyword clojure.lang.Keyword
      ^{:doc "A keyword"
        :forms '[Kw]}
Kw clojure.lang.Keyword
      ^{:doc "A symbol"
        :forms '[Symbol]}
Symbol clojure.lang.Symbol
      ^{:doc "A symbol"
        :forms '[Sym]}
Sym clojure.lang.Symbol

      ^{:doc "A string"
        :forms '[Str]}
Str java.lang.String

      ^{:doc "A boolean"
        :forms '[Bool]}
Bool java.lang.Boolean

; TODO: IMapEntry

      ^{:doc "A namespace"
        :forms '[Namespace]}
Namespace clojure.lang.Namespace

    ^{:doc "An atom that can read and write type x."
      :forms '[(Atom1 t)]}
Atom1 (TFn [[x :variance :invariant]] 
                              (clojure.lang.Atom x x))
    ^{:doc "An atom that can write type w and read type r."
      :forms '[(Atom2 t)]}
Atom2 (TFn [[w :variance :contravariant]
                               [r :variance :covariant]] 
                              (clojure.lang.Atom w r))
    ^{:doc "An var that can read and write type x."
      :forms '[(Var1 t)]}
Var1 
    (TFn [[x :variance :invariant]] 
         (clojure.lang.Var x x))
    ^{:doc "An var that can write type w and read type r."
      :forms '[(Var2 w r)]}
Var2 
    (TFn [[w :variance :contravariant]
          [r :variance :covariant]] 
         (clojure.lang.Var w r))
    ^{:doc "A ref that can read and write type x."
      :forms '[(Ref1 t)]}
Ref1 (TFn [[x :variance :invariant]] (clojure.lang.Ref x x))
    ^{:doc "A ref that can write type w and read type r."
      :forms '[(Ref2 w r)]}
Ref2 (TFn [[w :variance :contravariant]
                              [r :variance :covariant]] 
                             (clojure.lang.Ref w r))
    ^{:doc "An agent that can read and write type x."
      :forms '[(Agent1 t)]}
Agent1 (TFn [[x :variance :invariant]] 
                               (clojure.lang.Agent x x))
    ^{:doc "An agent that can write type w and read type r."
      :forms '[(Agent2 t t)]}
Agent2 (TFn [[w :variance :contravariant]
                                [r :variance :covariant]] 
                               (clojure.lang.Agent w r))

    ^{:doc "A union of x and nil."
      :forms '[(Option t)]}
Option (TFn [[x :variance :covariant]] (U nil x))

    ^{:doc "A union of x and nil."
      :forms '[(Nilable t)]}
Nilable (TFn [[x :variance :covariant]] (U nil x))

      ^{:doc "The identity function at the type level."
        :forms '[Id]}
Id (TFn [[x :variance :covariant]] x)

      ^{:doc "A persistent collection with member type x."
        :forms '[(Coll t)]}
Coll (TFn [[x :variance :covariant]]
                             (clojure.lang.IPersistentCollection x))
    ^{:doc "A persistent collection with member type x and count greater than 0."
      :forms '[(NonEmptyColl t)]}
NonEmptyColl (TFn [[x :variance :covariant]]
                                      (I (clojure.lang.IPersistentCollection x) (CountRange 1)))
    ^{:doc "A persistent vector with member type x."
      :forms '[(Vec t)]}
Vec (TFn [[x :variance :covariant]]
                            (clojure.lang.IPersistentVector x))
    ^{:doc "A persistent vector with member type x and count greater than 0."
      :forms '[(NonEmptyVec t)]}
NonEmptyVec (TFn [[x :variance :covariant]]
                                     (I (clojure.lang.IPersistentVector x) (CountRange 1)))
    ^{:doc "A persistent vector returned from clojure.core/vector (and others)"
      :forms '[(AVec t)]}
AVec (TFn [[x :variance :covariant]]
                             (I (clojure.lang.IPersistentVector x)
                                (java.lang.Iterable x)
                                (java.util.Collection x)
                                (java.util.List x)
                                clojure.lang.IObj))
    ^{:doc "A persistent vector returned from clojure.core/vector (and others) and count greater than 0."
      :forms '[(NonEmptyAVec t)]}
NonEmptyAVec (TFn [[x :variance :covariant]]
                                     (I (clojure.lang.IPersistentVector x)
                                        (java.lang.Iterable x)
                                        (java.util.Collection x)
                                        (java.util.List x)
                                        clojure.lang.IObj
                                        (CountRange 1)))
    ^{:doc "A non-empty lazy sequence of type t"
      :forms '[(NonEmptyLazySeq t)]}
NonEmptyLazySeq (TFn [[t :variance :covariant]]
                                        (I (clojure.lang.LazySeq t) (CountRange 1)))
    ^{:doc "A persistent map with keys k and vals v."
      :forms '[(Map t t)]}
Map (TFn [[k :variance :covariant]
                             [v :variance :covariant]]
                            (clojure.lang.IPersistentMap k v))
    ^{:doc "A persistent set with member type x"
      :forms '[(Set t)]}
Set (TFn [[x :variance :covariant]]
                            (clojure.lang.IPersistentSet x))
    ^{:doc "A sorted persistent set with member type x"
      :forms '[(SortedSet t)]}
SortedSet (TFn [[x :variance :covariant]]
                               (Extends [(clojure.lang.IPersistentSet x) clojure.lang.Sorted]))
    ^{:doc "A type that can be used to create a sequence of member type x."
      :forms '[(Seqable t)]}
Seqable (TFn [[x :variance :covariant]]
                                (clojure.lang.Seqable x))
    ^{:doc "A type that can be used to create a sequence of member type x
with count greater than 0."
      :forms '[(NonEmptySeqable t)]}

NonEmptySeqable (TFn [[x :variance :covariant]]
                                         (I (clojure.lang.Seqable x) (CountRange 1)))
    ^{:doc "A type that can be used to create a sequence of member type x
with count 0."
      :forms '[(EmptySeqable t)]}
EmptySeqable (TFn [[x :variance :covariant]]
                                  (I (clojure.lang.Seqable x) (ExactCount 0)))
      ^{:doc "A persistent sequence of member type x."
        :forms '[(Seq t)]}
Seq (TFn [[x :variance :covariant]]
                            (clojure.lang.ISeq x))

    ^{:doc "A persistent sequence of member type x with count greater than 0."
      :forms '[(NonEmptySeq t)]}
NonEmptySeq (TFn [[x :variance :covariant]]
                                     (I (clojure.lang.ISeq x) (CountRange 1)))

    ^{:doc "A persistent sequence of member type x with count greater than 0, or nil."
      :forms '[(NilableNonEmptySeq t)]}
NilableNonEmptySeq (TFn [[x :variance :covariant]]
                                         (U nil (I (clojure.lang.ISeq x) (CountRange 1))))

    ^{:doc "The type of all things with count 0. Use as part of an intersection.
eg. See EmptySeqable."
      :forms '[EmptyCount]}

EmptyCount (ExactCount 0)
    ^{:doc "The type of all things with count greater than 0. Use as part of an intersection.
eg. See NonEmptySeq"
      :forms '[NonEmptyCount]}
NonEmptyCount (CountRange 1)

    ^{:doc "A hierarchy for use with derive, isa? etc."
      :forms '[Hierarchy]}
Hierarchy '{:parents (clojure.lang.IPersistentMap Any Any)
                               :ancestors (clojure.lang.IPersistentMap Any Any)
                               :descendants (clojure.lang.IPersistentMap Any Any)}

    ^{:doc "A Clojure future (see clojure.core/{future-call,future})."
      :forms '[(Future t)]}
Future 
                      (TFn [[x :variance :covariant]]
                       (Extends [(clojure.lang.IDeref x)
                                 (clojure.lang.IBlockingDeref x)
                                 clojure.lang.IPending
                                 java.util.concurrent.Future]))

    ^{:doc "A Clojure promise (see clojure.core/{promise,deliver})."
      :forms '[(Promise t)]}
Promise 
              (TFn [[x :variance :invariant]]
               (Rec [p]
                (I (Extends [(clojure.lang.IDeref x)
                             (clojure.lang.IBlockingDeref x)
                             clojure.lang.IPending])
                   [x -> (U nil p)])))

    ^{:doc "A Clojure delay (see clojure.core/{delay,force})."
      :forms '[(Delay t)]}
Delay
              (TFn [[x :variance :covariant]]
                   (clojure.lang.Delay x))

    ^{:doc "A Clojure derefable (see clojure.core/deref)."
      :forms '[(Deref t)]}
Deref
              (TFn [[x :variance :covariant]]
                   (clojure.lang.IDeref x))

    ^{:doc "A Clojure blocking derefable (see clojure.core/deref)."
      :forms '[(BlockingDeref t)]}
BlockingDeref
              (TFn [[x :variance :covariant]]
                   (clojure.lang.IBlockingDeref x))

    ^{:doc "A Clojure persistent list."
      :forms '[(List t)]}
List
              (TFn [[x :variance :covariant]]
                   (clojure.lang.IPersistentList x))

    ^{:doc "A Clojure custom exception type."
      :forms '[ExInfo]}
ExInfo
              (I clojure.lang.IExceptionInfo
                 RuntimeException)

    ^{:doc "A Clojure proxy."
      :forms '[Proxy]}
Proxy
              clojure.lang.IProxy

; Should c.l.Sorted be parameterised? Is it immutable?
;    ^{:doc "A sorted Clojure collection."
;      :forms '[Sorted]}
;Sorted
;              clojure.lang.Sorted

    ^{:doc "A Clojure stack."
      :forms '[(Stack t)]}
Stack
              (TFn [[x :variance :covariant]]
                   (clojure.lang.IPersistentStack x))

    ^{:doc "A Clojure reversible collection."
      :forms '[(Reversible t)]}
Reversible
              (TFn [[x :variance :covariant]]
                   (clojure.lang.Reversible x))

    ^{:doc "A sequential collection."
      :forms '[Sequential]}
Sequential
             clojure.lang.Sequential

    ^{:doc "A sequential, seqable collection. Seq's aren't always Sequential."
      :forms '[(SequentialSeqable t)]}
SequentialSeqable
      (TFn [[x :variance :covariant]]
             (I clojure.lang.Sequential
                (clojure.lang.Seqable x)))

    ^{:doc "A Clojure sequential sequence. Seq's aren't always Sequential."
      :forms '[(SequentialSeq t)]}
SequentialSeq
      (TFn [[x :variance :covariant]]
             (I clojure.lang.Sequential
                (clojure.lang.ISeq x)))

    ^{:doc "A sequential seq returned from clojure.core/seq"
      :forms '[(ASeq t)]}
ASeq
      (TFn [[x :variance :covariant]]
           (I (clojure.lang.ISeq x)
              clojure.lang.Sequential
              (Iterable x)
              (java.util.Collection x)
              (java.util.List x)
              clojure.lang.IObj))

    ^{:doc "A sequential non-empty seq retured from clojure.core/seq"
      :forms '[(NonEmptyASeq t)]}
NonEmptyASeq
      (TFn [[x :variance :covariant]]
           (I (clojure.lang.ISeq x)
              clojure.lang.Sequential
              (Iterable x)
              (java.util.Collection x)
              (java.util.List x)
              clojure.lang.IObj
              (CountRange 1)))

    ^{:doc "The result of clojure.core/seq."
      :forms '[(NilableNonEmptyASeq t)]}
NilableNonEmptyASeq
      (TFn [[x :variance :covariant]]
           (U nil
              (I (clojure.lang.ISeq x)
                 clojure.lang.Sequential
                 (Iterable x)
                 (java.util.Collection x)
                 (java.util.List x)
                 clojure.lang.IObj
                 (CountRange 1))))

    ^{:doc "A type that returns true for clojure.core/fn?"
      :forms '[Fn]}
Fn
              clojure.lang.Fn

    ^{:doc "A Clojure multimethod."
      :forms '[Multi]}
Multi
              clojure.lang.MultiFn
])

(assert (even? (count init-aliases)))
(assert (apply distinct? (map first (partition 2 init-aliases))))
