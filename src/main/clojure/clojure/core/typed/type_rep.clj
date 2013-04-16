(ns clojure.core.typed.type-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed
             [utils :as u]]
            [clojure.set :as set]))

;(set! *warn-on-reflection* true)

;;; Type rep predicates

(def Type ::Type)

(defn Type? [a]
  (isa? (class a) Type))

(def AnyType ::AnyType)

(defn AnyType? [a]
  (isa? (class a) AnyType))

(derive Type AnyType)

(defn declare-type [a]
  (derive a Type))

(defn declare-AnyType [a]
  (derive a AnyType))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(u/defrecord Top []
  "The top type"
  [])

(def -any (->Top))

(declare-type Top)

;FIXME proper union maker, with sorted types
(u/defrecord Union [types]
  "An flattened, unordered union of types"
  [(set? types)
   (every? Type? types)
   (not (some Union? types))])

;temporary union maker
(defn- Un [& types]
  (->Union (set types)))

(declare-type Union)

(def empty-union (Un))

(defn Bottom []
  empty-union)

(def -nothing (Bottom))

(defn Bottom? [a]
  (= empty-union a))

(u/defrecord TCError []
  "Use *only* when a type error occurs"
  [])

(def Err (->TCError))

(declare-type TCError)

;should probably be ordered
(u/defrecord Intersection [types]
  "An unordered intersection of types."
  [(seq types)
   (every? Type? types)])

(declare Function?)

(u/defrecord FnIntersection [types]
  "An ordered intersection of Functions."
  [(seq types)
   (sequential? types)
   (every? Function? types)])

(declare-type FnIntersection)

(declare-type Intersection)

(def variances #{:constant :covariant :contravariant :invariant :dotted})

(defn variance? [v]
  (contains? variances v))

(declare Scope? TypeFn?)

(u/defrecord Bounds [upper-bound lower-bound higher-kind]
  "A type bound or higher-kind bound on a variable"
  [(some-fn (and (every? (some-fn Type? Scope?) [upper-bound lower-bound])
                 (nil? higher-kind))
            (and (every? nil? [upper-bound lower-bound])
                 (TypeFn? higher-kind)))])

(u/defrecord B [idx]
  "A bound variable. Should not appear outside this file"
  [(u/nat? idx)])

(declare-type B)

(u/defrecord F [name]
  "A named free variable"
  [(symbol? name)])

(defn make-F
  "Make a free variable "
  [name] (->F name))

(declare-type F)

(u/defrecord Scope [body]
  "A scope that contains one bound variable, can be nested. Not used directly"
  [((some-fn Type? Scope?) body)])

(defn scope-depth? 
  "True if scope is has depth number of scopes nested"
  [scope depth]
  {:pre [(Scope? scope)
         (u/nat? depth)]}
  (Type? (last (take (inc depth) (iterate #(and (Scope? %)
                                                (:body %))
                                          scope)))))

(u/defrecord Projection [afn ts]
  "Projects type variables as arguments to afn"
  [(fn? afn)
   (every? AnyType? ts)])

(declare-type Projection)

(u/defrecord RClass [variances poly? the-class replacements unchecked-ancestors]
  "A restricted class, where ancestors are
  (replace replacements (ancestors the-class))"
  [(or (nil? variances)
       (and (seq variances)
            (sequential? variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (sequential? poly?)
            (every? Type? poly?)))
   (symbol? the-class)
   ((u/hash-c? symbol? (some-fn Type? Scope?)) replacements)
   ((u/set-c? (some-fn Type? Scope?)) unchecked-ancestors)])

(declare-type RClass)

(defn ^Class RClass->Class [^RClass rcls]
  (u/symbol->Class (.the-class rcls)))

(u/defrecord Record [the-class variances poly? fields]
  "A Clojure record"
  [(or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (symbol? the-class)
   ((u/array-map-c? symbol? (some-fn Scope? Type?)) fields)])

(declare-type Record)

(u/defrecord DataType [the-class variances poly? fields]
  "A Clojure datatype"
  [(or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (symbol? the-class)
   ((u/array-map-c? symbol? (some-fn Scope? Type?)) fields)])

(declare-type DataType)

(u/defrecord Protocol [the-var variances poly? on-class methods]
  "A Clojure Protocol"
  [(symbol? the-var)
   (or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (= (count poly?) (count variances))
   (symbol? on-class)
   ((u/hash-c? (every-pred symbol? (complement namespace)) Type?) methods)])

(declare-type Protocol)

(u/defrecord TypeFn [nbound variances bbnds scope]
  "A type function containing n bound variables with variances.
  It is of a higher kind"
  [(u/nat? nbound)
   (every? variance? variances)
   (every? Bounds? bbnds)
   (apply = nbound (map count [variances bbnds]))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type TypeFn)

(defn tfn-bound [tfn]
  (->Bounds nil nil tfn))

;FIXME actual-frees should be metadata. ie. it should not affect equality
(u/defrecord Poly [nbound bbnds scope actual-frees]
  "A polymorphic type containing n bound variables, with display names actual-frees"
  [(u/nat? nbound)
   (every? Bounds? bbnds)
   (every? symbol? actual-frees)
   (apply = nbound (map count [bbnds actual-frees]))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type Poly)

(u/defrecord PolyDots [nbound bbnds ^Scope scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(u/nat? nbound)
   (every? Bounds? bbnds)
   (= nbound (count bbnds))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type PolyDots)

(u/defrecord Name [id]
  "A late bound name"
  [((every-pred (some-fn namespace (fn [a] (some (fn [c] (= \. c)) (str a))))
                symbol?) 
     id)])

(u/defrecord TApp [rator rands]
  "An application of a type function to arguments."
  [((some-fn Name? TypeFn? F? B?) rator)
   (every? (some-fn TypeFn? Type?) rands)])

(declare-type TApp) ;not always a type

(u/defrecord App [rator rands]
  "An application of a polymorphic type to type arguments"
  [(Type? rator)
   (every? Type? rands)])

(declare-type App)

(declare-type Name)

(u/defrecord Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)])

(declare-type Mu)

(u/defrecord Value [val]
  "A Clojure value"
  [])

(u/defrecord AnyValue []
  "Any Value"
  [])

(def -val ->Value)

(def -false (-val false))
(def -true (-val true))
(def -nil (-val nil))

(defn Nil? [a] (= -nil a))
(defn False? [a] (= -false a))
(defn True? [a] (= -true a))

(declare-type Value)
(declare-type AnyValue)

(declare Result?)

(u/defrecord HeterogeneousMap [types absent-keys other-keys?]
  "A constant map, clojure.lang.IPersistentMap"
  [((u/hash-c? Value? (some-fn Type? Result?))
     types)
   ((u/set-c? Value?) absent-keys)
   (u/boolean? other-keys?)])

(declare-type HeterogeneousMap)

(u/defrecord HeterogeneousVector [types]
  "A constant vector, clojure.lang.IPersistentVector"
  [(vector? types)
   (every? (some-fn Type? Result?) types)])

(defn -hvec [types]
  (if (some Bottom? types)
    (Bottom)
    (->HeterogeneousVector types)))

(declare-type HeterogeneousVector)

(u/defrecord HeterogeneousList [types]
  "A constant list, clojure.lang.IPersistentList"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousList)

(u/defrecord HeterogeneousSeq [types]
  "A constant seq, clojure.lang.ISeq"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousSeq)

(u/defrecord PrimitiveArray [jtype input-type output-type]
  "A Java Primitive array"
  [(class? jtype)
   (Type? input-type)
   (Type? output-type)])

(declare-type PrimitiveArray)

(u/defrecord DottedPretype [pre-type name]
  "A dotted pre-type. Not a type"
  [(Type? pre-type)
   ((some-fn symbol? u/nat?) name)])

(declare-AnyType DottedPretype)

;not a type, see KwArgsSeq
(u/defrecord KwArgs [mandatory optional]
  "A set of mandatory and optional keywords"
  [(every? (u/hash-c? Value? Type?) [mandatory optional])
   (= #{} (set/intersection (set (keys mandatory)) 
                            (set (keys optional))))])

(u/defrecord KwArgsSeq [mandatory optional]
  "A sequential seq representing a flattened map (for keyword arguments)."
  [(every? (u/hash-c? Value? Type?) [mandatory optional])
   (= #{} (set/intersection (set (keys mandatory)) 
                            (set (keys optional))))])

(declare-type KwArgsSeq)

(u/defrecord Function [dom rng rest drest kws]
  "A function arity, must be part of an intersection"
  [(or (nil? dom)
       (sequential? dom))
   (every? Type? dom)
   (Result? rng)
   ;at most one of rest drest or kws can be provided
   (#{0 1} (count (filter identity [rest drest kws])))
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (DottedPretype? drest))
   (or (nil? kws)
       (KwArgs? kws))])

(declare-AnyType Function)

(u/defrecord TopFunction []
  "Supertype to all functions"
  [])

(u/defrecord CountRange [lower upper]
  "A sequence of count between lower and upper.
  If upper is nil, between lower and infinity."
  [(u/nat? lower)
   (or (nil? upper)
       (and (u/nat? upper)
            (<= lower upper)))])

(u/defrecord GTRange [n]
  "The type of all numbers greater than n"
  [(number? n)])

(u/defrecord LTRange [n]
  "The type of all numbers less than n"
  [(number? n)])

(declare-type CountRange)
(declare-type GTRange)
(declare-type LTRange)

(defn make-CountRange
  ([lower] (make-CountRange lower nil))
  ([lower upper] (->CountRange lower upper)))

(defn make-ExactCountRange [c]
  {:pre [(u/nat? c)]}
  (make-CountRange c c))

(declare ->Result)

(defn make-FnIntersection [& fns]
  {:pre [(every? Function? fns)]}
  (->FnIntersection fns))

(u/defrecord NotType [type]
  "A type that does not include type"
  [(Type? type)])

(declare-type NotType)

(u/defrecord ListDots [pre-type bound]
  "A dotted list"
  [(Type? pre-type)
   ((some-fn F? B?) bound)])

(declare-type ListDots)

(defn- Filter?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-rep) 'Filter?)]
    (assert (var? v) "Filter? unbound")
    v))

(defn- FilterSet?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-rep) 'FilterSet?)]
    (assert (var? v) "FilterSet? unbound")
    v))

(defn RObject?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.object-rep) 'RObject?)]
    (assert (var? v) "RObject? unbound")
    v))

(u/defrecord Result [t fl o]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (@(Filter?-var) fl)
   (@(RObject?-var) o)])

(declare-AnyType Result)

(declare ret TCResult?)

(defn Result->TCResult [{:keys [t fl o] :as r}]
  {:pre [(Result? r)]
   :post [(TCResult? %)]}
  (ret t fl o))

(defn Result-type* [r]
  {:pre [(Result? r)]
   :post [(Type? %)]}
  (:t r))

(defn Result-filter* [r]
  {:pre [(Result? r)]
   :post [(@(Filter?-var) %)]}
  (:fl r))

(defn Result-object* [r]
  {:pre [(Result? r)]
   :post [(@(RObject?-var) %)]}
  (:o r))

(def no-bounds (->Bounds (->Top) (Un) nil))

(def ^:dynamic *mutated-bindings* #{})

(defn is-var-mutated? [id]
  (contains? *mutated-bindings* id))

(u/defrecord FlowSet [normal]
  "The filter that is true when an expression returns normally ie. not an exception."
  [(@(Filter?-var) normal)])

(u/defrecord TCResult [t fl o flow]
  "This record represents the result of typechecking an expression"
  [(Type? t)
   (@(FilterSet?-var) fl)
   (@(RObject?-var) o)
   (FlowSet? flow)])

(declare-AnyType TCResult)

(defn -flow [normal]
  (->FlowSet normal))

(defn- -FS-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-ops) '-FS)]
    (assert (var? v) "-FS unbound")
    v))

(defn- -top-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-rep) '-top)]
    (assert (var? v) "-top unbound")
    v))

(defn- ->EmptyObject-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.object-rep) '->EmptyObject)]
    (assert (var? v) "->EmptyObject unbound")
    v))

;[Type -> TCResult]
;[Type FilterSet -> TCResult]
;[Type FilterSet RObject -> TCResult]
(defn ret
  "Convenience function for returning the type of an expression"
  ([t] (let [-FS @(-FS-var)
             -top @(-top-var)
             ->EmptyObject @(->EmptyObject-var)]
         (ret t (-FS -top -top) (->EmptyObject) (-flow -top))))
  ([t f] 
   (let [-top @(-top-var)
         ->EmptyObject @(->EmptyObject-var)]
     (ret t f (->EmptyObject) (-flow -top))))
  ([t f o] 
   (let [-top @(-top-var)]
     (ret t f o (-flow -top))))
  ([t f o flow]
   {:pre [(AnyType? t)
          (@(FilterSet?-var) f)
          (@(RObject?-var) o)
          (FlowSet? flow)]
    :post [(TCResult? %)]}
   (->TCResult t f o flow)))

;[TCResult -> Type]
(defn ret-t [r]
  {:pre [(TCResult? r)]
   :post [(AnyType? %)]}
  (:t r))

;[TCResult -> FilterSet]
(defn ret-f [r]
  {:pre [(TCResult? r)]
   :post [(@(FilterSet?-var) %)]}
  (:fl r))

;[TCResult -> RObject]
(defn ret-o [r]
  {:pre [(TCResult? r)]
   :post [(@(RObject?-var) %)]}
  (:o r))

;[TCResult -> FlowSet]
(defn ret-flow [r]
  {:pre [(TCResult? r)]
   :post [(FlowSet? %)]}
  (:flow r))

;[Flow -> Filter]
(defn flow-normal [f]
  {:pre [(FlowSet? f)]
   :post [(@(Filter?-var) %)]}
  (:normal f))

;; Utils
;; It seems easier to put these here because of dependencies

(defn visit-bounds 
  "Apply f to each element of bounds"
  [ty f]
  {:pre [(Bounds? ty)]
   :post [(Bounds? ty)]}
  (-> ty
    (update-in [:upper-bound] #(when %
                                 (f %)))
    (update-in [:lower-bound] #(when %
                                 (f %)))
    (update-in [:higher-kind] #(when %
                                 (f %)))))

(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil))
  ([t f] (make-Result t f nil))
  ([t f o] 
   (let [-FS @(-FS-var)
         -top @(-top-var)
         ->EmptyObject @(->EmptyObject-var)]
     (->Result t (or f (-FS -top -top)) (or o (->EmptyObject))))))

(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to the most general filter
  and EmptyObject"
  ([dom rng] (make-Function dom rng nil nil))
  ([dom rng rest] (make-Function dom rng rest nil))
  ([dom rng rest drest & {:keys [filter object mandatory-kws optional-kws]}]
   (let [-FS @(-FS-var)
         -top @(-top-var)
         ->EmptyObject @(->EmptyObject-var)]
     (->Function dom (->Result rng (or filter (-FS -top -top)) (or object (->EmptyObject))) 
                 rest drest (when (or mandatory-kws optional-kws)
                              (->KwArgs (or mandatory-kws {})
                                        (or optional-kws {})))))))

