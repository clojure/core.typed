; untyped, clojure.core.typed depends on this namespace
(ns clojure.core.typed.current-impl
  (:require #?(:clj [clojure.core.typed.profiling :as p])
            [clojure.set :as set]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.util-vars :as vs]))

(def current-var-annotations-kw ::current-var-annotations)
(def current-nocheck-var?-kw ::current-nocheck-var?)
(def current-used-vars-kw ::current-used-vars)
(def current-checked-var-defs-kw ::current-checked-var-defs)
(def cljs-jsvar-annotations-kw ::cljs-jsvar-annotations)
(def untyped-var-annotations-kw ::untyped-var-annotations)
(def current-name-env-kw ::current-name-env)
(def method-return-nonnilable-env-kw ::method-return-nonnilable-env)
(def method-param-nilable-env-kw ::method-param-nilable-env)
(def method-override-env-kw ::method-override-env)
(def constructor-override-env-kw ::constructor-override-env)
(def protocol-name-type ::protocol-name)
(def current-protocol-env-kw ::current-protocol-env)
(def current-datatype-env-kw ::current-datatype-env)
(def current-dt-ancestors-kw ::current-dt-ancestors)
(def current-deps-kw ::current-deps)
(def datatype-name-type ::datatype-name)
(def ns-opts-kw ::ns-options)

(defn add-tc-var-type [sym type]
  (env/swap-checker! assoc-in [current-var-annotations-kw sym] type)
  nil)

(defn add-tc-alias [sym type]
  (env/swap-checker! assoc-in [current-var-annotations-kw sym] type)
  nil)

(defn add-nocheck-var [sym]
  (env/swap-checker! update current-nocheck-var?-kw (fnil conj #{}) sym)
  nil)

(defn remove-nocheck-var [sym]
  (env/swap-checker! update current-nocheck-var?-kw (fnil disj #{}) sym)
  nil)

(defn var-no-checks []
  {:post [(set? %)]}
  (get (env/deref-checker) current-nocheck-var?-kw #{}))

(defn check-var? [sym]
  (not (contains? (var-no-checks) sym)))

(defn add-tc-type-name [sym ty]
  (env/swap-checker! assoc-in
                     [current-name-env-kw sym]
                     ty
                     #_(if (r/Type? ty)
                       (vary-meta ty assoc :from-name sym)
                       ty))
  nil)

(def declared-name-type ::declared-name)

(defn declare-name* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-tc-type-name sym declared-name-type)
  nil)

(defn declare-protocol* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-tc-type-name sym protocol-name-type)
  nil)

(defn declare-datatype* [sym]
  (add-tc-type-name sym datatype-name-type)
  nil)

(defn add-untyped-var [nsym sym t]
  {:pre [(symbol? nsym)
         (symbol? sym)
         ; enforced in var-env/get-untyped-var,
         ; not worth loading/importing r/Type? for this
         ; assertion.
         #_(or (r/Type? t)
               (delay? t))]
   :post [(nil? %)]}
  (env/swap-checker! assoc-in [untyped-var-annotations-kw nsym sym] t)
  nil)

(defn add-nonnilable-method-return [sym m]
  {:pre [((every-pred namespace symbol?) sym)
         ((some-fn #(= :all %)
                   (con/set-c? con/znat?))
          m)]}
  (env/swap-checker! assoc-in [method-return-nonnilable-env-kw sym] m)
  nil)

(defn add-method-nilable-param [sym a]
  {:pre [((every-pred namespace symbol?) sym)
         ((con/hash-c? (some-fn #{:all} con/znat?)
                       (some-fn #{:all} (con/set-c? con/znat?)))
          a)]}
  (env/swap-checker! assoc-in [method-param-nilable-env-kw sym] a)
  nil)

(defn add-method-override [sym t]
  {:pre [((every-pred symbol? namespace) sym)
         ;; checked at `get-method-override`
         #_
         ((some-fn delay? r/Poly? r/FnIntersection?)
          t)]}
  (env/swap-checker! assoc-in [method-override-env-kw sym] t)
  nil)

(defn add-constructor-override [sym t]
  {:pre [(symbol? sym)
         ;; checked at `get-constructor-override`
         #_((some-fn delay? r/Type?) t)]}
  (env/swap-checker! assoc-in [constructor-override-env-kw sym] t)
  nil)

(defn add-protocol [sym t]
  {:pre [(symbol? sym)
         ;; checked in get-protocol
         #_
         ((some-fn delay? r/Type?) t)]}
  (env/swap-checker! assoc-in [current-protocol-env-kw sym] t)
  nil)

(defn add-datatype [sym t]
  {:pre [((every-pred symbol?
                      (fn [k] (some #{\.} (str k))))
          sym)
         ;; checked in get-datatype
         #_
         ((some-fn delay? r/Type?) t)]
   :post [(nil? %)]}
  (env/swap-checker! assoc-in [current-datatype-env-kw sym] t)
  nil)

(defn add-ns-deps [nsym deps]
  {:pre [(symbol? nsym)
         ((con/set-c? symbol?) deps)]
   :post [(nil? %)]}
  (env/swap-checker! update-in [current-deps-kw nsym] (fnil set/union #{}) deps)
  nil)

(defn register-warn-on-unannotated-vars [nsym]
  (env/swap-checker! assoc-in [ns-opts-kw nsym :warn-on-unannotated-vars] true)
  nil)

#?(:clj
(defmacro create-env
  "For name n, creates defs for {n}, {n}-kw, add-{n},
  and reset-{n}!"
  [n]
  {:pre [(symbol? n)
         (not (namespace n))]}
  (let [kw-def (symbol (str n "-kw"))
        add-def (symbol (str "add-" n))
        reset-def (symbol (str "reset-" n "!"))]
    `(do (def ~kw-def ~(keyword (str (ns-name *ns*)) (str n)))
         (defn ~n []
           {:post [(map? ~'%)]}
           (get (env/deref-checker) ~kw-def {}))
         (defn ~add-def [sym# t#]
           {:pre [(symbol? sym#)]
            :post [(nil? ~'%)]}
           (env/swap-checker! assoc-in [~kw-def sym#] t#)
           nil)
         (defn ~reset-def [m#]
           (env/swap-checker! assoc ~kw-def m#)
           nil)
         nil))))

;; runtime environments
#?(:clj
(create-env var-env))
#?(:clj
(create-env alias-env))
#?(:clj 
(create-env protocol-env))
#?(:clj 
(create-env rclass-env))
#?(:clj 
(create-env datatype-env))
#?(:clj 
(create-env jsnominal-env))

#?(:clj
(defn v [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    @var)))

#?(:clj
(defn the-var [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]
   :post [(var? %)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    var)))

(def clojure ::clojure)
(def clojurescript ::clojurescript)

(def unknown ::unknown)

(derive clojure unknown)
(derive clojurescript unknown)

(def current-impl-kw ::current-impl)

(defn current-impl []
  {:post [(keyword? %)]}
  (get (some-> (env/checker-or-nil) deref)
       current-impl-kw unknown))

(declare bindings-for-impl)

#?(:clj
(defmacro with-impl [impl & body]
  `(with-bindings (get (bindings-for-impl) ~impl {})
     ~@body)))

(defonce clj-checker-atom 
  (doto (env/init-checker)
    (swap! assoc current-impl-kw clojure)))

(defn clj-checker []
  clj-checker-atom)

(defn clj-bindings []
  {#'env/*checker* (clj-checker)})

#?(:clj
(defmacro with-clojure-impl [& body]
  `(with-impl clojure
     ~@body)))

(defonce cljs-checker-atom 
  (doto (env/init-checker)
    (swap! assoc current-impl-kw clojurescript)))

(defn cljs-checker []
  {:post [#?(:clj (instance? clojure.lang.IAtom %)
             :cljs (instance? Atom %))]}
  cljs-checker-atom)

(defn cljs-bindings []
  {#'env/*checker* (cljs-checker)})

#?(:clj
(defmacro with-cljs-impl [& body]
  `(with-impl clojurescript
     ~@body)))

(defn impl-for []
  {:clojure (cljs-checker)
   :cljs (clj-checker)})

(defn bindings-for-impl []
  {clojure (clj-bindings)
   clojurescript (cljs-bindings)})

#?(:clj
(defmacro with-full-impl [impl & body]
  `(with-impl ~impl
     ~@body)))

(defn implementation-specified? []
  ((complement #{unknown}) (current-impl)))

(defn ensure-impl-specified []
  (assert (implementation-specified?) "No implementation specified"))

(defn checking-clojure? []
  (ensure-impl-specified)
  (= clojure (current-impl)))

(defn checking-clojurescript? []
  (ensure-impl-specified)
  (= clojurescript (current-impl)))

(defn assert-clojure 
  ([] (assert-clojure nil))
  ([msg] (assert (= clojure (current-impl)) (str "Clojure implementation only"
                                                 (when (seq msg)
                                                   (str ": " msg))))))

(defn assert-cljs []
  (assert (= clojurescript (current-impl)) "Clojurescript implementation only"))

;; :clojure = ::clojure
;; :cljs = ::clojurescript
;; :unknown = ::unknown
#?(:clj
(defmacro impl-case [& {clj-case :clojure cljs-case :cljs unknown :unknown :as opts}]
  (assert (empty? (set/difference (set (keys opts)) #{:clojure :cljs :unknown}))
          "Incorrect cases to impl-case")
  `(case (current-impl)
     ~clojure ~clj-case
     ~clojurescript ~cljs-case
     ~(if (contains? opts :unknown)
        unknown
        `(assert nil (str "No case matched for impl-case " (current-impl)))))))

#?(:clj
(defn var->symbol [^clojure.lang.Var var]
  {:pre [(var? var)]
   :post [((every-pred symbol? namespace) %)]}
  (symbol (str (ns-name (.ns var)))
          (str (.sym var)))))

#?(:clj
(defn Class->symbol [^Class cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls))))

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
                (I (clojure.lang.IDeref x)
                   (clojure.lang.IBlockingDeref x)
                   clojure.lang.IPending
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

  ^{:doc "A reducer function with accumulator a and reduces over collections of b"
    :forms '[(Reducer a b)]}
Reducer
  (TFn [[a :variance :contravariant]
        [b :variance :invariant]]
    (IFn 
      ;init
      [:-> b]
      ;complete
      [b :-> b]
      ;step
      [b a :-> (U b (clojure.lang.Reduced b))]))

  ^{:doc "A transducer function that transforms in to out."
    :forms '[(Transducer in out)]}
Transducer
  (TFn [[in :variance :contravariant]
        [out :variance :covariant]]
    (All [r]
      [(clojure.core.typed/Reducer out r) :-> (clojure.core.typed/Reducer in r)]))
])

(assert (even? (count init-aliases)))
(assert (apply distinct? (map first (partition 2 init-aliases))))

#?(:clj
(defn gen-protocol* [current-env current-ns vsym binder mths]
  {:pre [(symbol? current-ns)
         ((some-fn nil? map?) mths)]}
  (let [_ (require 'clojure.core.typed.errors)
        int-error (v 'clojure.core.typed.errors/int-error)
        _ (when-not (symbol? vsym)
            (int-error
              (str "First argument to ann-protocol must be a symbol: " vsym)))
        s (if (namespace vsym)
            (symbol vsym)
            (symbol (str current-ns) (name vsym)))
        protocol-defined-in-nstr (namespace s)
        _ (when-let [[m] (seq (remove symbol? (keys mths)))]
            (int-error (str "Method names to ann-protocol must be symbols, found: " (pr-str m))))
        _ (doseq [n1 (keys mths)
                  n2 (keys mths)]
            (when (and (not= n1 n2)
                       (= (munge n1) (munge n2)))
              (int-error 
                (str "Protocol methods for " vsym " must have distinct representations: "
                     "both " n1 " and " n2 " compile to " (munge n1)))))
        ; add a Name so the methods can be parsed
        _ (declare-protocol* s)
        parsed-binder (when binder 
                        (delay
                          (let [_ (require 'clojure.core.typed.parse-unparse)
                                parse-free-binder-with-variance 
                                (v 'clojure.core.typed.parse-unparse/parse-free-binder-with-variance)
                                with-parse-ns* (v 'clojure.core.typed.parse-unparse/with-parse-ns*)]
                            (with-parse-ns* current-ns
                              #(parse-free-binder-with-variance binder)))))
        fs (when parsed-binder
             (delay 
               (let [_ (require 'clojure.core.typed.type-rep)
                     make-F (v 'clojure.core.typed.type-rep/make-F)]
                 (map (comp make-F :fname) (force parsed-binder)))))
        bnds (when parsed-binder
               (delay (map :bnd (force parsed-binder))))
        ms (into {} (for [[knq v*] mths]
                      (let [_ (when (namespace knq)
                                (int-error "Protocol method should be unqualified"))
                            mtype 
                            (delay
                              (let [_ (require 'clojure.core.typed.free-ops
                                               'clojure.core.typed.parse-unparse)
                                    with-bounded-frees* (v 'clojure.core.typed.free-ops/with-bounded-frees*)
                                    with-parse-ns* (v 'clojure.core.typed.parse-unparse/with-parse-ns*)
                                    unparse-type (v 'clojure.core.typed.parse-unparse/unparse-type)
                                    parse-type (v 'clojure.core.typed.parse-unparse/parse-type)
                                    mtype (with-bounded-frees* (zipmap (force fs) (force bnds))
                                            #(binding [vs/*current-env* current-env]
                                               (with-parse-ns* current-ns
                                                 (fn []
                                                   (parse-type v*)))))
                                    _ (let [_ (require 'clojure.core.typed.type-ctors
                                                       'clojure.core.typed.type-rep)
                                            fully-resolve-type (v 'clojure.core.typed.type-ctors/fully-resolve-type)
                                            Poly? (v 'clojure.core.typed.type-rep/Poly?)
                                            Poly-fresh-symbols* (v 'clojure.core.typed.type-ctors/Poly-fresh-symbols*)
                                            Poly-body* (v 'clojure.core.typed.type-ctors/Poly-body*)
                                            PolyDots? (v 'clojure.core.typed.type-rep/PolyDots?)
                                            PolyDots-fresh-symbols* (v 'clojure.core.typed.type-ctors/PolyDots-fresh-symbols*)
                                            PolyDots-body* (v 'clojure.core.typed.type-ctors/PolyDots-body*)
                                            FnIntersection? (v 'clojure.core.typed.type-rep/FnIntersection?)
                                            rt (fully-resolve-type mtype)
                                            fin? (fn [f]
                                                   (let [f (fully-resolve-type f)]
                                                     (boolean
                                                       (when (FnIntersection? f)
                                                         (every? seq (map :dom (:types f)))))))]
                                        (when-not 
                                          (or
                                            (fin? rt)
                                            (when (Poly? rt) 
                                              (let [names (Poly-fresh-symbols* rt)]
                                                (fin? (Poly-body* names rt))))
                                            (when (PolyDots? rt) 
                                              (let [names (PolyDots-fresh-symbols* rt)]
                                                (fin? (PolyDots-body* names rt)))))
                                          ;(prn "throwing method type")
                                          (int-error (str "Protocol method " knq " should be a possibly-polymorphic function intersection"
                                                              " taking at least one fixed argument: "
                                                              (unparse-type mtype)))))]
                                mtype))]
                         [knq mtype])))
        ;_ (prn "collect protocol methods" (into {} ms))
        t (delay
            (let [_ (require 'clojure.core.typed.type-ctors)
                  Protocol* (v 'clojure.core.typed.type-ctors/Protocol*)
                  Protocol-var->on-class (v 'clojure.core.typed.type-ctors/Protocol-var->on-class)]
              (Protocol* (map :name (force fs)) (map :variance (force parsed-binder) )
                         (force fs) s (Protocol-var->on-class s) 
                         (into {} (map (fn [[k v]] [k (force v)])) ms) 
                         (map :bnd (force parsed-binder)))))]
    ;(prn "Adding protocol" s t)
    (add-protocol s t)
    ; annotate protocol var as Any
    (add-nocheck-var s)
    (add-tc-var-type s (delay 
                              (let [_ (require 'clojure.core.typed.type-rep)
                                    -any (v 'clojure.core.typed.type-rep/-any)]
                                -any)))
    (doseq [[kuq mt] ms]
      (assert (not (namespace kuq))
              "Protocol method names should be unqualified")
      ;qualify method names when adding methods as vars
      (let [kq (symbol protocol-defined-in-nstr (name kuq))
            mt-ann (delay 
                     (let [_ (require 'clojure.core.typed.collect-utils)
                           protocol-method-var-ann (v 'clojure.core.typed.collect-utils/protocol-method-var-ann)]
                       (protocol-method-var-ann (force mt) (map :name (force fs)) (force bnds))))]
        (add-nocheck-var kq)
        (add-tc-var-type kq mt-ann)))
    ;(prn "end gen-protocol" s)
    nil)))

(defn add-datatype-ancestors
  "Add a mapping of ancestor overrides (from the type syntax of the override
  to the actual parsed type) for the datatype named sym."
  [sym tmap]
  {:pre [(symbol? sym)
         (map? tmap)]
   :post [(nil? %)]}
  (env/swap-checker! update-in [current-dt-ancestors-kw sym] merge tmap)
  nil)

#?(:clj
(defn gen-datatype* [current-env current-ns provided-name fields vbnd opt record?]
  {:pre [(symbol? current-ns)]}
  (with-clojure-impl
    (let [{ancests :unchecked-ancestors} opt
          ancests (or ancests (:extends opt))
          parsed-binders (when vbnd
                           (delay
                             (let [_ (require 'clojure.core.typed.parse-unparse)
                                   parse-free-binder-with-variance 
                                   (v 'clojure.core.typed.parse-unparse/parse-free-binder-with-variance)
                                   with-parse-ns* (v 'clojure.core.typed.parse-unparse/with-parse-ns*)]
                               (with-parse-ns* current-ns
                                 #(parse-free-binder-with-variance vbnd)))))
          ;variances
          vs (when parsed-binders
               (delay (seq (map :variance (force parsed-binders)))))
          args (when parsed-binders
                 (delay (seq (map :fname (force parsed-binders)))))
          bnds (when parsed-binders
                 (delay (seq (map :bnd (force parsed-binders)))))]
      (let [provided-name-str (str provided-name)
            ;_ (prn "provided-name-str" provided-name-str)
            munged-ns-str (if (some #(= \. %) provided-name-str)
                            (apply str (butlast (apply concat (butlast (partition-by #(= \. %) provided-name-str)))))
                            (str (munge current-ns)))
            ;_ (prn "munged-ns-str" munged-ns-str)
            _ (require 'clojure.repl)
            demunge (v 'clojure.repl/demunge)
            demunged-ns-str (str (demunge munged-ns-str))
            ;_ (prn "demunged-ns-str" demunged-ns-str)
            local-name (if (some #(= \. %) provided-name-str)
                         (symbol (apply str (last (partition-by #(= \. %) (str provided-name-str)))))
                         provided-name-str)
            ;_ (prn "local-name" local-name)
            s (symbol (str munged-ns-str \. local-name))
            fs (delay
                 (let [_ (require 'clojure.core.typed.parse-unparse
                                  'clojure.core.typed.type-rep
                                  'clojure.core.typed.type-ctors
                                  'clojure.core.typed.free-ops)
                       with-parse-ns* (v 'clojure.core.typed.parse-unparse/with-parse-ns*)
                       parse-type (v 'clojure.core.typed.parse-unparse/parse-type)
                       make-F (v 'clojure.core.typed.type-rep/make-F)
                       abstract-many (v 'clojure.core.typed.type-ctors/abstract-many)
                       with-frees* (v 'clojure.core.typed.free-ops/with-frees*)
                       parse-field (fn [[n _ t]] [n (parse-type t)])
                       ]
                   (apply array-map (apply concat (with-frees* (mapv make-F (force args))
                                                    (fn []
                                                      (binding [vs/*current-env* current-env]
                                                        (with-parse-ns* current-ns
                                                          #(mapv parse-field (partition 3 fields))))))))))
            as (into {}
                     (map
                       (fn [an]
                         [an (delay
                               (let [_ (require 'clojure.core.typed.parse-unparse
                                                'clojure.core.typed.type-rep
                                                'clojure.core.typed.type-ctors
                                                'clojure.core.typed.free-ops)
                                     with-parse-ns* (v 'clojure.core.typed.parse-unparse/with-parse-ns*)
                                     parse-type (v 'clojure.core.typed.parse-unparse/parse-type)
                                     make-F (v 'clojure.core.typed.type-rep/make-F)
                                     with-frees* (v 'clojure.core.typed.free-ops/with-frees*)
                                     abstract-many (v 'clojure.core.typed.type-ctors/abstract-many)]
                                 (with-frees* (mapv make-F (force args))
                                   (fn []
                                     (binding [vs/*current-env* current-env]
                                       (with-parse-ns* current-ns
                                         #(let [t (parse-type an)]
                                            (abstract-many (force args) t))))))))]))
                     ancests)
            ;_ (prn "collected ancestors" as)
            _ (add-datatype-ancestors s as)
            pos-ctor-name (symbol demunged-ns-str (str "->" local-name))
            map-ctor-name (symbol demunged-ns-str (str "map->" local-name))
            dt (delay 
                 (let [_ (require 'clojure.core.typed.type-ctors
                                  'clojure.core.typed.type-rep)
                       DataType* (v 'clojure.core.typed.type-ctors/DataType*)
                       make-F (v 'clojure.core.typed.type-rep/make-F)]
                   (DataType* (force args) (force vs) (map make-F (force args)) s (force bnds) (force fs) record?)))
            _ (add-datatype s dt)
            pos-ctor (delay
                       (let [_ (require 'clojure.core.typed.subtype
                                        'clojure.core.typed.type-rep
                                        'clojure.core.typed.frees
                                        'clojure.core.typed.type-ctors)
                             Poly* (v 'clojure.core.typed.type-ctors/Poly*)
                             make-FnIntersection (v 'clojure.core.typed.type-rep/make-FnIntersection)
                             make-Function (v 'clojure.core.typed.type-rep/make-Function)
                             make-F (v 'clojure.core.typed.type-rep/make-F)
                             DataType-of (v 'clojure.core.typed.type-ctors/DataType-of)
                             ]
                         (if args
                           (Poly* (force args) (force bnds)
                                  (make-FnIntersection
                                    (make-Function (vec (vals (force fs))) (DataType-of s (map make-F (force args))))))
                           (make-FnIntersection
                             (make-Function (vec (vals (force fs))) (DataType-of s))))))
            map-ctor (delay
                       (let [_ (require 'clojure.core.typed.subtype
                                        'clojure.core.typed.type-rep
                                        'clojure.core.typed.frees
                                        'clojure.core.typed.type-ctors)
                             subtype? (v 'clojure.core.typed.subtype/subtype?)
                             -val (v 'clojure.core.typed.type-rep/-val)
                             -nil (v 'clojure.core.typed.type-rep/-nil)
                             fv (v 'clojure.core.typed.frees/fv)
                             fi (v 'clojure.core.typed.frees/fi)
                             make-HMap (v 'clojure.core.typed.type-ctors/make-HMap)
                             Poly* (v 'clojure.core.typed.type-ctors/Poly*)
                             make-FnIntersection (v 'clojure.core.typed.type-rep/make-FnIntersection)
                             make-Function (v 'clojure.core.typed.type-rep/make-Function)
                             make-F (v 'clojure.core.typed.type-rep/make-F)
                             DataType-of (v 'clojure.core.typed.type-ctors/DataType-of)]
                         (when record?
                           (let [hmap-arg ; allow omission of keys if nil is allowed and field is monomorphic
                                 (let [{optional true mandatory false} 
                                       (group-by (fn [[_ t]] (and (empty? (fv t))
                                                                  (empty? (fi t))
                                                                  (subtype? -nil t)))
                                                 (zipmap (map (comp -val keyword) (keys (force fs)))
                                                         (vals (force fs))))]
                                   (make-HMap :optional (into {} optional)
                                              :mandatory (into {} mandatory)))]
                             (if args
                               (Poly* (force args) (force bnds)
                                      (make-FnIntersection
                                        (make-Function [hmap-arg] (DataType-of s (map make-F (force args))))))
                               (make-FnIntersection
                                 (make-Function [hmap-arg] (DataType-of s))))))))]
        (do 
          ;(when vs
          ;  (let [f (mapv r/make-F (repeatedly (count vs) gensym))]
          ;    ;TODO replacements and unchecked-ancestors go here
          ;    (rcls/alter-class* s (c/RClass* (map :name f) (force vs) f s {} {} (force bnds)))))
          (add-tc-var-type pos-ctor-name pos-ctor)
          (add-nocheck-var pos-ctor-name)
          (when record?
            (add-method-override (symbol (str s) "create") map-ctor)
            (add-tc-var-type map-ctor-name map-ctor)
            (add-nocheck-var map-ctor-name))))))))
