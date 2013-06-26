(ns clojure.core.typed.type-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require (clojure.core.typed
             [utils :as u])
            [clojure.core.typed :as t]
            [clojure.set :as set])
  (:import (clojure.lang IPersistentSet Seqable Symbol Keyword IPersistentMap
                         IPersistentVector)))

; cyclic deps
(t/typed-deps clojure.core.typed.filter-rep
              clojure.core.typed.object-rep)

(t/tc-ignore
; add fr as an alias to filter-rep
(create-ns 'clojure.core.typed.filter-rep)
(alias 'fr 'clojure.core.typed.filter-rep)

; add or as an alias to object-rep
(create-ns 'clojure.core.typed.object-rep)
(alias 'or 'clojure.core.typed.object-rep)
  )

(t/tc-ignore
(defn- Filter?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-rep) 'Filter?)]
    (assert (var? v) "Filter? unbound")
    v))

(defn- FilterSet?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-rep) 'FilterSet?)]
    (assert (var? v) "FilterSet? unbound")
    v))

(defn- -FS-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-ops) '-FS)]
    (assert (var? v) "-FS unbound")
    v))

(defn RObject?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.object-rep) 'RObject?)]
    (assert (var? v) "RObject? unbound")
    v))

(defn -top-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.filter-rep) '-top)]
    (assert (var? v) "-top unbound")
    v))

(defn -empty-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.object-rep) '-empty)]
    (assert (var? v) "-empty unbound")
    v))
  )

(t/def-alias SeqNumber Long)

(t/ann next-sequence-number (t/Atom1 SeqNumber))
(def ^:private next-sequence-number 
  "The next number to use for sequence hashing"
  (atom 0))

(t/ann type-sequence-mapping (t/Atom1 (IPersistentMap TCType SeqNumber)))
(def ^:private type-sequence-mapping 
  "Mapping from types to sequence number"
  (atom {}))

;(set! *warn-on-reflection* true)

;;; Type rep predicates

(t/ann-protocol TCType)
(t/ann-protocol TCAnyType)

;clj bug means protocols need at least one method
; Seems fixed for 1.5.1, but leaving for compatibility.
(u/defprotocol TCType
  (dummy [_]))
(u/defprotocol TCAnyType
  (dummy2 [_]))

(t/ann ^:nocheck Type? (predicate TCType))
(defn Type? [a]
  (satisfies? TCType a))

(t/ann ^:nocheck AnyType? (predicate TCAnyType))
(defn AnyType? [a]
  (or (Type? a)
      (satisfies? TCAnyType a)))

(t/ann ^:nocheck declare-type [Class -> Any])
(defn declare-type [a]
  (extend a TCType {}))

(t/ann ^:nocheck declare-AnyType [Class -> Any])
(defn declare-AnyType [a]
  (extend a TCAnyType {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(u/ann-record Top [])
(u/defrecord Top []
  "The top type"
  [])

(t/ann -any TCType)
(def -any (->Top))

(declare-type Top)

;FIXME proper union maker, with sorted types
(u/ann-record Union [types :- (IPersistentSet TCType)])
(u/defrecord Union [types]
  "An flattened, unordered union of types"
  [(set? types)
   (every? Type? types)
   (not (some Union? types))])

;temporary union maker
(t/ann Un [TCType * -> Union])
(defn- Un [& types]
  (->Union (set types)))

(declare-type Union)

(t/ann empty-union TCType)
(def empty-union (Un))

(t/ann Bottom [-> TCType])
(defn Bottom []
  empty-union)

(t/ann -nothing TCType)
(def -nothing (Bottom))

(t/ann ^:nocheck Bottom? (predicate TCType))
(defn Bottom? [a]
  (= empty-union a))

(u/ann-record TCError [])
(u/defrecord TCError []
  "Use *only* when a type error occurs"
  [])

(t/ann Err TCType)
(def Err (->TCError))

(declare-type TCError)

;should probably be ordered
(u/ann-record Intersection [types :- (I (Seqable TCType)
                                        (CountRange 1))])
(u/defrecord Intersection [types]
  "An unordered intersection of types."
  [(seq types)
   (every? Type? types)])

(declare Function?)

(u/ann-record FnIntersection [types :- (I (Seqable TCType)
                                          (CountRange 1))])
(u/defrecord FnIntersection [types]
  "An ordered intersection of Functions."
  [(seq types)
   (sequential? types)
   (every? Function? types)])

(declare-type FnIntersection)

(declare-type Intersection)

(t/def-alias Variance
  "Keywords that represent a certain variance"
  (U ':constant ':covariant ':contravariant ':invariant ':dotted))

(t/ann variances (IPersistentSet Variance))
(def variances #{:constant :covariant :contravariant :invariant :dotted})

(t/ann ^:nocheck variance? (predicate Variance))
(defn variance? [v]
  (contains? variances v))

(declare Scope? TypeFn?)

; This annotation needs to go before the first reference to TypeFn,
; otherwise it will resolve to an RClass, instead of a DataType.
; DataType should be combined with RClass in the future.
(u/ann-record TypeFn [nbound :- Number,
                      variances :- (U nil (Seqable Variance))
                      bbnds :- (U nil (Seqable Bounds)),
                      scope :- Scope])

(u/ann-record Bounds [upper-bound :- (U nil TCType Scope)
                      lower-bound :- (U nil TCType Scope)
                      higher-kind :- (U nil TypeFn)])
(u/defrecord Bounds [upper-bound lower-bound higher-kind]
  "A type bound or higher-kind bound on a variable"
  [(some-fn (and (every? (some-fn Type? Scope?) [upper-bound lower-bound])
                 (nil? higher-kind))
            (and (every? nil? [upper-bound lower-bound])
                 (TypeFn? higher-kind)))])

(u/ann-record B [idx :- Number])
(u/defrecord B [idx]
  "A bound variable. Should not appear outside this file"
  [(u/nat? idx)])

(declare-type B)

;FIXME kind should be part of the identity of a free, otherwise type caching is unsound
; Same with bounds.
(u/ann-record F [name :- Symbol])
(u/defrecord F [name]
  "A named free variable"
  [(symbol? name)])

(t/ann make-F [Symbol -> F])
(defn make-F
  "Make a free variable "
  [name] (->F name))

(declare-type F)

(u/ann-record Scope [body :- (U Scope TCType)])
(u/defrecord Scope [body]
  "A scope that contains one bound variable, can be nested. Not used directly"
  [((some-fn Type? Scope?) body)])

(t/ann ^:nocheck scope-depth? [Scope Number -> Any])
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

(u/ann-record RClass [variances :- (U nil (I (CountRange 1) (Seqable Variance)))
                      poly? :- (U nil (I (CountRange 1) (Seqable TCType)))
                      the-class :- Symbol
                      replacements :- (IPersistentMap Symbol (U TCType Scope))
                      unchecked-ancestors :- (IPersistentSet (U TCType Scope))])
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

(t/ann ^:nocheck RClass->Class [RClass -> Class])
(defn ^Class RClass->Class [^RClass rcls]
  (u/symbol->Class (.the-class rcls)))

(u/ann-record DataType [the-class :- Symbol,
                        variances :- (U nil (I (CountRange 1) (Seqable Variance))),
                        poly? :- (U nil (I (CountRange 1) (Seqable TCType))),
                        fields :- (IPersistentMap Symbol (U Scope TCType))
                        record? :- Boolean])
(u/defrecord DataType [the-class variances poly? fields record?]
  "A Clojure datatype"
  [(or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (symbol? the-class)
   ((u/array-map-c? symbol? (some-fn Scope? Type?)) fields)
   (u/boolean? record?)])

(t/ann ^:nocheck DataType->Class [DataType -> Class])
(defn ^Class DataType->Class [^DataType dt]
  (u/symbol->Class (.the-class dt)))

(t/ann ^:nocheck Record? [Any -> Boolean])
(defn Record? [^DataType a]
  (boolean
    (when (DataType? a)
      (.record? a))))

(declare-type DataType)

(u/ann-record Protocol [the-var :- Symbol,
                        variances :- (U nil (Seqable Variance)),
                        poly? :- (U nil TCType),
                        on-class :- Symbol,
                        methods :- (IPersistentMap Symbol TCType)])
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

(t/ann tfn-bound [TypeFn -> Bounds])
(defn tfn-bound [tfn]
  (->Bounds nil nil tfn))

;FIXME actual-frees should be metadata. ie. it should not affect equality
(u/ann-record Poly [nbound :- Number,
                    bbnds :- (U nil (Seqable Bounds)),
                    scope :- Scope,
                    actual-frees :- (U nil (Seqable Symbol))])
(u/defrecord Poly [nbound bbnds scope actual-frees]
  "A polymorphic type containing n bound variables, with display names actual-frees"
  [(u/nat? nbound)
   (every? Bounds? bbnds)
   (every? symbol? actual-frees)
   (apply = nbound (map count [bbnds actual-frees]))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type Poly)

(u/ann-record PolyDots [nbound :- Number,
                        bbnds :- (U nil (Seqable Bounds)),
                        scope :- Scope])
(u/defrecord PolyDots [nbound bbnds ^Scope scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(u/nat? nbound)
   (every? Bounds? bbnds)
   (= nbound (count bbnds))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type PolyDots)

(u/ann-record Name [id :- Symbol])
(u/defrecord Name [id]
  "A late bound name"
  [((every-pred (some-fn namespace (fn [a] (some (fn [c] (= \. c)) (str a))))
                symbol?) 
     id)])

(u/ann-record TApp [rator :- TCType,
                    rands :- (Seqable TCType)])
(u/defrecord TApp [rator rands]
  "An application of a type function to arguments."
  [((some-fn Name? TypeFn? F? B?) rator)
   (every? (some-fn TypeFn? Type?) rands)])

(declare-type TApp) ;not always a type

(u/ann-record App [rator :- TCType,
                   rands :- (Seqable TCType)])
(u/defrecord App [rator rands]
  "An application of a polymorphic type to type arguments"
  [(Type? rator)
   (every? Type? rands)])

(declare-type App)

(declare-type Name)

(u/ann-record Mu [scope :- Scope])
(u/defrecord Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)])

(declare-type Mu)

(u/ann-record Value [val :- Any])
(u/defrecord Value [val]
  "A Clojure value"
  [])

(u/ann-record AnyValue [])
(u/defrecord AnyValue []
  "Any Value"
  [])

(t/ann -val [Any -> TCType])
(def -val ->Value)

(t/ann -false TCType)
(t/ann -true TCType)
(t/ann -nil TCType)
(def -false (-val false))
(def -true (-val true))
(def -nil (-val nil))

(t/ann Nil? [Any -> Boolean])
(t/ann False? [Any -> Boolean])
(t/ann True? [Any -> Boolean])
(defn Nil? [a] (= -nil a))
(defn False? [a] (= -false a))
(defn True? [a] (= -true a))

(declare-type Value)
(declare-type AnyValue)

(declare Result?)

(u/ann-record HeterogeneousMap [types :- (IPersistentMap TCType TCType),
                                absent-keys :- (IPersistentSet TCType),
                                other-keys? :- Boolean])
(u/defrecord HeterogeneousMap [types absent-keys other-keys?]
  "A constant map, clojure.lang.IPersistentMap"
  [((u/hash-c? Value? (some-fn Type? Result?))
     types)
   ((u/set-c? Value?) absent-keys)
   (u/boolean? other-keys?)])

(declare-type HeterogeneousMap)

(t/def-alias TempFilterSet
  "Can't import FilterSet here, so an alias will do for now"
  clojure.core.typed.filter_rep.FilterSet)

(u/ann-record HeterogeneousVector [types :- (IPersistentVector TCType)
                                   fs :- (IPersistentVector TempFilterSet)
                                   objects :- (IPersistentVector or/IRObject)])
(u/defrecord HeterogeneousVector [types fs objects]
  "A constant vector, clojure.lang.IPersistentVector"
  [(vector? types)
   (every? (some-fn Type? Result?) types)
   (vector? fs)
   (let [FilterSet? @(FilterSet?-var)]
     (every? FilterSet? fs))
   (vector? objects)
   (let [RObject? @(RObject?-var)]
     (every? RObject? objects))
   (apply = (map count [types fs objects]))])

(t/ann ^:nocheck -hvec 
       [(IPersistentVector TCType) & {:filters (Seqable TempFilterSet) :objects (Seqable or/IRObject)} -> TCType])
(defn -hvec 
  [types & {:keys [filters objects]}]
  (let [-FS @(-FS-var)
        -top @(-top-var)
        -empty @(-empty-var)]
    (if (some Bottom? types)
      (Bottom)
      (->HeterogeneousVector types
                             (if filters
                               (vec filters)
                               (vec (repeat (count types) (-FS -top -top))))
                             (if objects
                               (vec objects)
                               (vec (repeat (count types) -empty)))))))

(declare-type HeterogeneousVector)

(u/ann-record HeterogeneousList [types :- (Seqable TCType)])
(u/defrecord HeterogeneousList [types]
  "A constant list, clojure.lang.IPersistentList"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousList)

(u/ann-record HeterogeneousSeq [types :- (Seqable TCType)])
(u/defrecord HeterogeneousSeq [types]
  "A constant seq, clojure.lang.ISeq"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousSeq)

(u/ann-record PrimitiveArray [jtype :- Class,
                              input-type :- TCType
                              output-type :- TCType])
(u/defrecord PrimitiveArray [jtype input-type output-type]
  "A Java Primitive array"
  [(class? jtype)
   (Type? input-type)
   (Type? output-type)])

(declare-type PrimitiveArray)

(u/ann-record DottedPretype [pre-type :- TCType,
                             name :- (U Symbol Number)])
(u/defrecord DottedPretype [pre-type name]
  "A dotted pre-type. Not a type"
  [(Type? pre-type)
   ((some-fn symbol? u/nat?) name)])

(declare-AnyType DottedPretype)

;not a type, see KwArgsSeq
(u/ann-record KwArgs [mandatory :- (IPersistentMap TCType TCType)
                      optional  :- (IPersistentMap TCType TCType)])
(u/defrecord KwArgs [mandatory optional]
  "A set of mandatory and optional keywords"
  [(every? (u/hash-c? Value? Type?) [mandatory optional])
   (= #{} (set/intersection (set (keys mandatory)) 
                            (set (keys optional))))])

(u/ann-record KwArgsSeq [mandatory :- (IPersistentMap TCType TCType)
                         optional  :- (IPersistentMap TCType TCType)])
(u/defrecord KwArgsSeq [mandatory optional]
  "A sequential seq representing a flattened map (for keyword arguments)."
  [(every? (u/hash-c? Value? Type?) [mandatory optional])
   (= #{} (set/intersection (set (keys mandatory)) 
                            (set (keys optional))))])

(declare-type KwArgsSeq)

(u/ann-record Function [dom :- (U nil (Seqable TCType)),
                        rng :- Result,
                        rest :- (U nil TCType)
                        drest :- (U nil DottedPretype)
                        kws :- (U nil KwArgs)])
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

(u/ann-record TopFunction [])
(u/defrecord TopFunction []
  "Supertype to all functions"
  [])

(u/ann-record CountRange [lower :- Number,
                          upper :- (U nil Number)])
(u/defrecord CountRange [lower upper]
  "A sequence of count between lower and upper.
  If upper is nil, between lower and infinity."
  [(u/nat? lower)
   (or (nil? upper)
       (and (u/nat? upper)
            (<= lower upper)))])

(u/ann-record GTRange [n :- Number])
(u/defrecord GTRange [n]
  "The type of all numbers greater than n"
  [(number? n)])

(u/ann-record LTRange [n :- Number])
(u/defrecord LTRange [n]
  "The type of all numbers less than n"
  [(number? n)])

(declare-type CountRange)
(declare-type GTRange)
(declare-type LTRange)

(t/ann make-CountRange (Fn [Number -> CountRange]
                           [Number (U nil Number) -> CountRange]))
(defn make-CountRange
  ([lower] (make-CountRange lower nil))
  ([lower upper] (->CountRange lower upper)))

(t/ann make-ExactCountRange (Fn [Number -> CountRange]))
(defn make-ExactCountRange [c]
  {:pre [(u/nat? c)]}
  (make-CountRange c c))

(declare ->Result)

(t/ann ^:nocheck make-FnIntersection [Function * -> FnIntersection])
(defn make-FnIntersection [& fns]
  {:pre [(every? Function? fns)]}
  (->FnIntersection fns))

(u/ann-record NotType [type :- TCType])
(u/defrecord NotType [type]
  "A type that does not include type"
  [(Type? type)])

(declare-type NotType)

(u/ann-record ListDots [pre-type :- TCType,
                        bound :- (U F B)])
(u/defrecord ListDots [pre-type bound]
  "A dotted list"
  [(Type? pre-type)
   ((some-fn F? B?) bound)])

(declare-type ListDots)

(u/ann-record Extends [extends :- (U nil (Seqable TCType))
                       without :- (U nil (Seqable TCType))])
(u/defrecord Extends [extends without]
  "A set of ancestors that always and never occur."
  [(every? Type? extends)
   (every? Type? without)])

(declare-type Extends)

(declare FlowSet?)

(u/ann-record Result [t :- TCType,
                      fl :- fr/IFilter
                      o :- or/IRObject])
(u/defrecord Result [t fl o flow]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (@(FilterSet?-var) fl)
   (@(RObject?-var) o)
   (FlowSet? flow)])

(declare-AnyType Result)

(declare ret TCResult?)

(t/ann ^:nocheck Result->TCResult [Result -> TCResult])
(defn Result->TCResult [{:keys [t fl o] :as r}]
  {:pre [(Result? r)]
   :post [(TCResult? %)]}
  (ret t fl o))

(t/ann Result-type* [Result -> TCType])
(defn Result-type* [r]
  {:pre [(Result? r)]
   :post [(Type? %)]}
  (:t r))

(t/ann ^:nocheck Result-filter* [Result -> fr/IFilter])
(defn Result-filter* [r]
  {:pre [(Result? r)]
   :post [(@(Filter?-var) %)]}
  (:fl r))

(t/ann ^:nocheck Result-object* [Result -> or/IRObject])
(defn Result-object* [r]
  {:pre [(Result? r)]
   :post [(@(RObject?-var) %)]}
  (:o r))

(t/ann ^:nocheck Result-flow* [Result -> FlowSet])
(defn Result-flow* [r]
  {:pre [(Result? r)]
   :post [(FlowSet? %)]}
  (:flow r))

(t/ann no-bounds Bounds)
(def no-bounds (->Bounds (->Top) (Un) nil))

;unused
(t/tc-ignore
(def ^:dynamic *mutated-bindings* #{})

(defn is-var-mutated? [id]
  (contains? *mutated-bindings* id))
  )

(u/ann-record FlowSet [normal :- fr/IFilter])
(u/defrecord FlowSet [normal]
  "The filter that is true when an expression returns normally ie. not an exception."
  [(@(Filter?-var) normal)])

(u/ann-record TCResult [t :- TCType
                        fl :- fr/IFilter
                        o :- or/IRObject
                        flow :- FlowSet])
(u/defrecord TCResult [t fl o flow]
  "This record represents the result of typechecking an expression"
  [(Type? t)
   (@(FilterSet?-var) fl)
   (@(RObject?-var) o)
   (FlowSet? flow)])

(declare-AnyType TCResult)

(t/ann -flow [fr/IFilter -> FlowSet])
(defn -flow [normal]
  (->FlowSet normal))

(t/ann ^:nocheck ret
       (Fn [TCType -> TCResult]
           [TCType TempFilterSet -> TCResult]
           [TCType TempFilterSet or/IRObject -> TCResult]
           [TCType TempFilterSet or/IRObject FlowSet -> TCResult]))
(defn ret
  "Convenience function for returning the type of an expression"
  ([t] (let [-FS @(-FS-var)
             -top @(-top-var)
             -empty @(-empty-var)]
         (ret t (-FS -top -top) -empty (-flow -top))))
  ([t f] 
   (let [-top @(-top-var)
         -empty @(-empty-var)]
     (ret t f -empty (-flow -top))))
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

(t/ann ret-t [TCResult -> TCType])
(defn ret-t [r]
  {:pre [(TCResult? r)]
   :post [(AnyType? %)]}
  (:t r))

(t/ann ^:nocheck ret-f [TCResult -> TempFilterSet])
(defn ret-f [r]
  {:pre [(TCResult? r)]
   :post [(@(FilterSet?-var) %)]}
  (:fl r))

(t/ann ^:nocheck ret-o [TCResult -> or/IRObject])
(defn ret-o [r]
  {:pre [(TCResult? r)]
   :post [(@(RObject?-var) %)]}
  (:o r))

(t/ann ret-flow [TCResult -> FlowSet])
(defn ret-flow [r]
  {:pre [(TCResult? r)]
   :post [(FlowSet? %)]}
  (:flow r))

(t/ann ^:nocheck flow-normal [FlowSet -> fr/IFilter])
(defn flow-normal [f]
  {:pre [(FlowSet? f)]
   :post [(@(Filter?-var) %)]}
  (:normal f))

;; Utils
;; It seems easier to put these here because of dependencies

(t/ann ^:nocheck visit-bounds [Bounds [TCType -> TCType] -> Bounds])
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

(t/ann ^:nocheck make-Result
       (Fn [TCType -> Result]
           [TCType (U nil fr/IFilter) -> Result]
           [TCType (U nil fr/IFilter) (U nil or/IRObject) -> Result]
           [TCType (U nil fr/IFilter) (U nil or/IRObject) (U nil FlowSet) -> Result]))
(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil nil))
  ([t f] (make-Result t f nil nil))
  ([t f o] (make-Result t f o nil))
  ([t f o flow]
   (let [-FS @(-FS-var)
         -top @(-top-var)
         -empty @(-empty-var)]
     (->Result t (or f (-FS -top -top)) (or o -empty) (or flow (-flow -top))))))

(t/ann ^:nocheck make-Function
       (Fn [(U nil (Seqable TCType)) TCType -> Function]
           [(U nil (Seqable TCType)) TCType (U nil TCType) -> Function]
           [(U nil (Seqable TCType)) TCType (U nil TCType) (U nil TCType) 
            & {:filter (U nil TempFilterSet) :object (U nil or/IRObject)
               :mandatory-kws (U nil (IPersistentMap TCType TCType))
               :optional-kws (U nil (IPersistentMap TCType TCType))}
            -> Function]))
(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to the most general filter
  and EmptyObject"
  ([dom rng] (make-Function dom rng nil nil))
  ([dom rng rest] (make-Function dom rng rest nil))
  ([dom rng rest drest & {:keys [filter object mandatory-kws optional-kws flow]}]
   (let [-FS @(-FS-var)
         -top @(-top-var)
         -empty @(-empty-var)]
     (->Function dom (make-Result rng filter object flow)
                 rest drest (when (or mandatory-kws optional-kws)
                              (->KwArgs (or mandatory-kws {})
                                        (or optional-kws {})))))))

