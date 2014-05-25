(ns ^:skip-wiki 
  clojure.core.typed.type-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            clojure.core.typed.contract-ann
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed :as t]
            [clojure.set :as set]
            [clojure.core.typed.current-impl :as impl]))

#_(t/typed-deps clojure.core.typed.coerce-utils
              clojure.core.typed.contract-utils)

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )


(t/ann ^:no-check -FS-var [-> (t/Var1 [p/IFilter p/IFilter -> p/IFilterSet])])
(defn- -FS-var []
  (impl/the-var 'clojure.core.typed.filter-ops/-FS))

(t/ann ^:no-check -top-var [-> (t/Var1 p/IFilter)])
(defn -top-var []
  (impl/the-var 'clojure.core.typed.filter-rep/-top))

(t/ann ^:no-check -empty-var [-> (t/Var1 p/IRObject)])
(defn -empty-var []
  (impl/the-var 'clojure.core.typed.object-rep/-empty))

(t/defalias SeqNumber Long)

;(set! *warn-on-reflection* true)

;;; Type rep predicates

(t/defalias Type
  "A normal type"
  p/TCType)

(t/defalias AnyType
  "A normal type or special type like Function."
  (U Type p/TCAnyType))

(t/defalias MaybeScopedType
  "A type or a scope"
  (U Type p/IScope))

; consider making this a definterface
(t/ann-protocol TypeId
                type-id [TypeId -> Long])

(u/defprotocol TypeId
  (type-id [_]))

; not a real symmetric predicate, but we always extend Type with the
; interface for speed, so it's sufficient.
(t/ann ^:no-check Type? (predicate Type))
(defn Type? [a]
  (instance? clojure.core.typed.impl_protocols.TCType a))

; similar for AnyType
(t/ann ^:no-check AnyType? (predicate AnyType))
(defn AnyType? [a]
  (or (Type? a)
      (instance? clojure.core.typed.impl_protocols.TCAnyType a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(u/ann-record Top [])
(u/def-type Top []
  "The top type"
  []
  :methods
  [p/TCType])

(t/ann -any Type)
(def -any (Top-maker))


(u/ann-record Union [types :- (t/Set Type)])
(u/def-type Union [types]
  "An flattened, unordered union of types"
  [(set? types)
   (every? Type? types)
   (not-any? Union? types)]
  :methods
  [p/TCType])

;temporary union maker
(t/ann Un [Type * -> Union])
(defn- Un [& types]
  (Union-maker (set types)))

(t/ann empty-union Type)
(def empty-union (Un))

(t/ann Bottom [-> Type])
(defn Bottom []
  empty-union)

(t/ann -nothing Type)
(def -nothing (Bottom))

(t/ann Bottom? [Any -> Boolean])
(defn Bottom? [a]
  (= empty-union a))

(u/ann-record TCError [])
(u/def-type TCError []
  "Use *only* when a type error occurs"
  []
  :methods
  [p/TCType])

(t/ann Err Type)
(def Err (TCError-maker))


;should probably be ordered
(u/ann-record Intersection [types :- (t/NonEmptySeqable Type)])
(u/def-type Intersection [types]
  "An unordered intersection of types."
  [(seq types)
   (every? Type? types)]
  :methods 
  [p/TCType])

(t/defalias Variance
  "Keywords that represent a certain variance"
  (U ':constant ':covariant ':contravariant ':invariant ':dotted))

(t/ann variances (t/Set Variance))
(def variances #{:constant :covariant :contravariant :invariant :dotted})

(t/ann ^:no-check variance? (predicate Variance))
(defn variance? [v]
  (contains? variances v))

(declare Scope? TypeFn?)

(u/ann-record Bounds [upper-bound :- MaybeScopedType
                      lower-bound :- MaybeScopedType
                      higher-kind :- nil])

;;;; FIXME playing around with order shouldn't be needed anymore
; This annotation needs to go before the first reference to TypeFn,
; otherwise it will resolve to an RClass, instead of a DataType.
; DataType should be combined with RClass in the future.
(u/ann-record TypeFn [nbound :- Number,
                      variances :- (U nil (t/Seqable Variance))
                      bbnds :- (U nil (t/Seqable Bounds)),
                      scope :- p/IScope])
(u/def-type Bounds [upper-bound lower-bound higher-kind]
  "A type bound or higher-kind bound on a variable"
  [(every? (some-fn Type? Scope?) [upper-bound lower-bound])
   ;deprecated/unused
   (nil? higher-kind)])

(u/ann-record B [idx :- Number])
(u/def-type B [idx]
  "de Bruijn indexes - should never appear outside of this file.
  Bound type variables"
  [(con/nat? idx)]
  :methods
  [p/TCType])

; Always naming frees as fresh is crucial in Typed Clojure.
; Typed Clojure has bounded-polymorphism, which means we need to be very careful
; when caching results of subtyping, intersections and similar. 
;
; We use bounds to our advantage to make subtyping between free variables more useful
;
; eg. 
; In 
;   (All [[x :< Long]] [-> x]) <: (All [[y :< Number]] [-> y])
; x <: y
;
; Because of the way we check function return values, we cache this result.

; Same with bounds.
(u/ann-record F [name :- t/Sym])
(u/def-type F [name]
  "A named free variable"
  [(symbol? name)]
  :methods
  [p/TCType])

(t/ann make-F [t/Sym -> F])
(defn make-F
  "Make a free variable "
  [name] (F-maker name))

(t/ann F-original-name [F -> t/Sym])
(defn F-original-name 
  "Get the printable name of a free variable.
  
  Used for pretty-printing errors or similar, only instantiate
  an instance of F with this name for explicit scoping."
  [f]
  {:pre [(F? f)]
   :post [(symbol? %)]}
  (or (-> f :name meta :original-name)
      (:name f)))

(u/ann-record Scope [body :- MaybeScopedType])
(u/def-type Scope [body]
  "A scope that contains one bound variable, can be nested. Not used directly"
  [((some-fn Type? Scope?) body)]
  :methods
  [p/IScope
   (scope-body [this] body)])

(t/defalias ScopedType
  (U Type Scope))

(t/ann ^:no-check scoped-Type? (predicate (U Scope Type)))
(def scoped-Type? (some-fn Scope? Type?))

(t/ann ^:no-check scope-depth? [Scope Number -> Any])
(defn scope-depth? 
  "True if scope is has depth number of scopes nested"
  [scope depth]
  {:pre [(Scope? scope)
         (con/nat? depth)]}
  (Type? (last (take (inc depth) (iterate #(and (Scope? %)
                                                (:body %))
                                          scope)))))

(u/ann-record RClass [variances :- (U nil (t/NonEmptySeqable Variance))
                      poly? :- (U nil (t/NonEmptySeqable Type))
                      the-class :- t/Sym])
(u/def-type RClass [variances poly? the-class]
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
   (symbol? the-class)]
  :intern
  [variances
   (map hash poly?)
   (keyword the-class)]
  :methods
  [p/TCType])

(t/ann ^:no-check RClass->Class [RClass -> Class])
(defn ^Class RClass->Class [^RClass rcls]
  (coerce/symbol->Class (.the-class rcls)))

(u/ann-record JSNominal [variances :- (U nil (t/NonEmptySeqable Variance))
                         poly? :- (U nil (t/NonEmptySeqable Type))
                         name :- t/Sym])
(u/def-type JSNominal [variances poly? name]
  "A Javascript nominal type"
  [(or (nil? variances)
       (and (seq variances)
            (sequential? variances)
            (every? variance? variances)))
   (= (count variances) (count poly?))
   (or (nil? poly?)
       (and (seq poly?)
            (sequential? poly?)
            (every? Type? poly?)))
   (symbol? name)]
  :methods
  [p/TCType])

(u/ann-record DataType [the-class :- t/Sym,
                        variances :- (U nil (t/NonEmptySeqable Variance)),
                        poly? :- (U nil (t/NonEmptySeqable Type)),
                        fields :- (t/Map t/Sym MaybeScopedType)
                        record? :- Boolean])
(u/def-type DataType [the-class variances poly? fields record?]
  "A Clojure datatype"
  [(or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (= (count variances) (count poly?))
   (symbol? the-class)
   ((con/array-map-c? symbol? (some-fn Scope? Type?)) fields)
   (con/boolean? record?)]
  :methods
  [p/TCType])

(t/ann ^:no-check DataType->Class [DataType -> Class])
(defn ^Class DataType->Class [^DataType dt]
  (coerce/symbol->Class (.the-class dt)))

(t/ann Record? [Any -> Boolean])
(defn Record? [^DataType a]
  (boolean
    (when (DataType? a)
      (.record? a))))

(u/ann-record Protocol [the-var :- t/Sym,
                        variances :- (U nil (t/NonEmptySeqable Variance)),
                        poly? :- (U nil (t/NonEmptySeqable Type)),
                        on-class :- t/Sym,
                        methods :- (t/Map t/Sym Type)
                        declared? :- Boolean])
(u/def-type Protocol [the-var variances poly? on-class methods declared?]
  "A Clojure Protocol"
  [(symbol? the-var)
   (or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? (some-fn Scope? Type?) poly?)))
   (= (count poly?) (count variances))
   (symbol? on-class)
   ((con/hash-c? (every-pred symbol? (complement namespace)) (some-fn Scope? Type?)) methods)
   (con/boolean? declared?)]
  :methods
  [p/TCType])

(u/def-type TypeFn [nbound variances bbnds scope]
  "A type function containing n bound variables with variances.
  It is of a higher kind"
  [(con/nat? nbound)
   (every? variance? variances)
   (every? Bounds? bbnds)
   (apply = nbound (map count [variances bbnds]))
   (scope-depth? scope nbound)
   (Scope? scope)]
  :methods
  [p/TCType])

(u/ann-record Poly [nbound :- Number,
                    bbnds :- (U nil (t/Seqable Bounds)),
                    scope :- p/IScope,])
(u/def-type Poly [nbound bbnds scope]
  "A polymorphic type containing n bound variables"
  [(con/nat? nbound)
   (every? Bounds? bbnds)
   (apply = nbound (map count [bbnds]))
   (scope-depth? scope nbound)
   (Scope? scope)]
  :methods
  [p/TCType])

(u/ann-record PolyDots [nbound :- Number,
                        bbnds :- (U nil (t/Seqable Bounds)),
                        scope :- p/IScope])
(u/def-type PolyDots [nbound bbnds scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(con/nat? nbound)
   (every? Bounds? bbnds)
   (= nbound (count bbnds))
   (scope-depth? scope nbound)
   (Scope? scope)]
  :methods
  [p/TCType])

(u/ann-record Name [id :- t/Sym])
(u/def-type Name [id]
  "A late bound name"
  [((every-pred symbol?
                (some-fn namespace #(some #{\.} (str %))))
     id)]
  :methods
  [p/TCType])

(u/ann-record TApp [rator :- Type,
                    rands :- (t/Seqable Type)])
(u/def-type TApp [rator rands]
  "An application of a type function to arguments."
  [(Type? rator)
   (every? Type? rands)]
  :methods
  [p/TCType])

(u/ann-record App [rator :- Type,
                   rands :- (t/Seqable Type)])
(u/def-type App [rator rands]
  "An application of a polymorphic type to type arguments"
  [(Type? rator)
   (every? Type? rands)]
  :methods
  [p/TCType])

(u/ann-record Mu [scope :- p/IScope])
(u/def-type Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)]
  :methods
  [p/TCType
   p/IMu
   (mu-scope [_] scope)])

(t/ann Mu-body-unsafe [Mu -> Type])
(defn Mu-body-unsafe [mu]
  {:pre [(Mu? mu)]
   :post [(Type? %)]}
  (-> mu :scope :body))

(u/ann-record Value [val :- Any])
(u/def-type Value [val]
  "A Clojure value"
  []
  :methods
  [p/TCType])

(u/ann-record AnyValue [])
(u/def-type AnyValue []
  "Any Value"
  []
  :methods
  [p/TCType])

(t/ann -val [Any -> Type])
(def -val Value-maker)

(t/ann-many Type 
            -false -true -nil)
(def -false (-val false))
(def -true (-val true))
(def -nil (-val nil))

(t/ann-many [Any -> Boolean]
            Nil? False? True?)
(defn Nil? [a] (= -nil a))
(defn False? [a] (= -false a))
(defn True? [a] (= -true a))

(declare Result?)

(u/ann-record HeterogeneousMap [types :- (t/Map Type Type),
                                optional :- (t/Map Type Type),
                                absent-keys :- (t/Set Type),
                                other-keys? :- Boolean])
(u/def-type HeterogeneousMap [types optional absent-keys other-keys?]
  "A constant map, clojure.lang.IPersistentMap"
  [((con/hash-c? Value? (some-fn Type? Result?))
     types)
   ((con/hash-c? Value? (some-fn Type? Result?))
     optional)
   ((con/set-c? Value?) absent-keys)
   (empty? (set/intersection
             (set (keys types))
             (set (keys optional))
             absent-keys))
   (con/boolean? other-keys?)]
  :methods
  [p/TCType])

(u/ann-record DottedPretype [pre-type :- Type,
                             name :- (U t/Sym Number)
                             partition-count :- Number])
(u/def-type DottedPretype [pre-type name partition-count]
  "A dotted pre-type. Not a type"
  [(Type? pre-type)
   ((some-fn symbol? con/nat?) name)
   (con/nat? partition-count)]
  :methods
  [p/TCAnyType])

(t/ann-many [Type (U t/Sym Number) -> DottedPretype]
            DottedPretype1-maker
            DottedPretype2-maker)

(defn DottedPretype1-maker [pre-type name]
  (DottedPretype-maker pre-type name 1))

(defn DottedPretype2-maker [pre-type name]
  (DottedPretype-maker pre-type name 2))

(u/ann-record HeterogeneousVector [;fixed members
                                   types :- (t/Vec Type)
                                   fs :- (t/Vec p/IFilterSet)
                                   objects :- (t/Vec p/IRObject)
                                   ;variable members to the right of fixed
                                   rest :- (U nil Type)
                                   drest :- (U nil DottedPretype)
                                   repeat :- Boolean])
(u/def-type HeterogeneousVector [types fs objects rest drest repeat]
  "A constant vector, clojure.lang.IPersistentVector"
  [(vector? types)
   (every? (some-fn Type? Result?) types)
   (vector? fs)
   (every? p/IFilterSet? fs)
   (vector? objects)
   (every? p/IRObject? objects)
   (apply = (map count [types fs objects]))
   (#{0 1} (count (filter identity [rest drest repeat])))
   ((some-fn nil? Type?) rest)
   ((some-fn nil? DottedPretype?) drest)
   ((some-fn true? false?) repeat)]
  :methods
  [p/TCType])

(t/ann ^:no-check -hvec
       [(t/Vec Type) & :optional {:filters (t/Seqable p/IFilterSet) :objects (t/Seqable p/IRObject)
                                  :rest (U nil Type) :drest (U nil DottedPretype) :repeat Boolean} -> Type])
(defn -hvec
  [types & {:keys [filters objects rest drest] repeat? :repeat}]
  (let [-FS @(-FS-var)
        -top @(-top-var)
        -empty @(-empty-var)]
    (if (some Bottom? types)
      (Bottom)
      (HeterogeneousVector-maker types
                                 (if filters
                                   (vec filters)
                                   (vec (repeat (count types) (-FS -top -top))))
                                 (if objects
                                   (vec objects)
                                   (vec (repeat (count types) -empty)))
                                 rest
                                 drest
                                 (if repeat? true false)))))

(u/ann-record HeterogeneousList [types :- (t/Seqable Type)])
(u/def-type HeterogeneousList [types]
  "A constant list, clojure.lang.IPersistentList"
  [(sequential? types)
   (every? Type? types)]
  :methods
  [p/TCType])

(u/ann-record HeterogeneousSeq [types :- (t/Seqable Type)
                                fs :- (t/Vec p/IFilterSet)
                                objects :- (t/Vec p/IRObject)
                                ;variable members to the right of fixed
                                rest :- (U nil Type)
                                drest :- (U nil DottedPretype)
                                repeat :- Boolean])
(u/def-type HeterogeneousSeq [types fs objects rest drest repeat]
  "A constant seq, clojure.lang.ISeq"
  [(sequential? types)
   (every? Type? types)
   (vector? fs)
   (every? p/IFilterSet? fs)
   (vector? objects)
   (every? p/IRObject? objects)
   (apply = (map count [types fs objects]))
   (#{0 1} (count (filter identity [rest drest repeat])))
   ((some-fn nil? Type?) rest)
   ((some-fn nil? DottedPretype?) drest)
   ((some-fn true? false?) repeat)]
  :methods
  [p/TCType])

(t/ann ^:no-check -hseq
       [(t/Seqable Type) & :optional {:filters (t/Seqable p/IFilterSet) :objects (t/Seqable p/IRObject)
                                  :rest (U nil Type) :drest (U nil DottedPretype) :repeat Boolean} -> Type])
(defn -hseq
  [types & {:keys [filters objects rest drest] repeat? :repeat}]
  (let [-FS @(-FS-var)
        -top @(-top-var)
        -empty @(-empty-var)]
    (if (some Bottom? types)
      (Bottom)
      (HeterogeneousSeq-maker types
                              (if filters
                                (vec filters)
                                (vec (repeat (count types) (-FS -top -top))))
                              (if objects
                                (vec objects)
                                (vec (repeat (count types) -empty)))
                              rest
                              drest
                              (if repeat? true false)))))

(u/ann-record HSequential [types :- (t/Seqable Type)
                           fs :- (t/Vec p/IFilterSet)
                           objects :- (t/Vec p/IRObject)
                           ;variable members to the right of fixed
                           rest :- (U nil Type)
                           drest :- (U nil DottedPretype)
                           repeat :- Boolean])
(u/def-type HSequential [types fs objects rest drest repeat]
  "A constant Sequential, clojure.lang.Sequential"
  [(sequential? types)
   (every? (some-fn Type? Result?) types)
   (vector? fs)
   (every? p/IFilterSet? fs)
   (vector? objects)
   (every? p/IRObject? objects)
   (apply = (map count [types fs objects]))
   (#{0 1} (count (filter identity [rest drest repeat])))
   ((some-fn nil? Type?) rest)
   ((some-fn nil? DottedPretype?) drest)
   ((some-fn true? false?) repeat)]
  :methods
  [p/TCType])

(t/ann ^:no-check -hsequential
       [(t/Seqable Type) & :optional {:filters (t/Seqable p/IFilterSet) :objects (t/Seqable p/IRObject)
                                  :rest (U nil Type) :drest (U nil DottedPretype) :repeat Boolean} -> Type])
(defn -hsequential
  [types & {:keys [filters objects rest drest] repeat? :repeat}]
  (let [-FS @(-FS-var)
        -top @(-top-var)
        -empty @(-empty-var)]
    (if (some Bottom? types)
      (Bottom)
      (HSequential-maker types
                         (if filters
                           (vec filters)
                           (vec (repeat (count types) (-FS -top -top))))
                         (if objects
                           (vec objects)
                           (vec (repeat (count types) -empty)))
                         rest
                         drest
                         (if repeat? true false)))))

(u/ann-record PrimitiveArray [jtype :- Class,
                              input-type :- Type
                              output-type :- Type])
(u/def-type PrimitiveArray [jtype input-type output-type]
  "A Java Primitive array"
  [(class? jtype)
   (Type? input-type)
   (Type? output-type)]
  :methods
  [p/TCType])

;; Heterogeneous ops

(declare DottedPretype?)

(u/ann-record AssocType [target :- Type,
                         entries :- (t/Coll '[Type Type])
                         dentries :- (U nil DottedPretype)])
(u/def-type AssocType [target entries dentries]
  "An assoc[iate] operation on the type level"
  [(Type? target)
   (or (DottedPretype? dentries)
       (nil? dentries))
   (and (every? (con/hvector-c? Type? Type?) entries)
        (sequential? entries))
   (not (and entries dentries))]
  :methods
  [p/TCType])

(u/ann-record DissocType [target :- Type,
                          keys :- (t/Coll Type)
                          dkeys :- (U nil DottedPretype)])
(u/def-type DissocType [target keys dkeys]
  "A dissoc[iate] operation on the type level"
  [(Type? target)
   (or (DottedPretype? dkeys)
       (nil? dkeys))
   (and (every? Type? keys)
        (sequential? keys))
   (not (and keys dkeys))]
  :methods
  [p/TCType])

(u/ann-record GetType [target :- Type,
                       key :- Type
                       not-found :- Type
                       target-fs :- p/IFilterSet
                       target-object :- p/IRObject])
(u/def-type GetType [target key not-found target-fs target-object]
  "get on the type level"
  [(Type? target) 
   (Type? key) 
   (Type? not-found)
   (p/IFilterSet? target-fs)
   (p/IRObject? target-object)]
  :methods
  [p/TCType])

(t/ann ^:no-check -get 
       [Type Type & :optional {:not-found (U nil Type)
                               :target-fs (U nil p/IFilterSet)
                               :target-object (U nil p/IRObject)}
        -> GetType])
(defn -get 
  [target key & {:keys [not-found target-fs target-object]}]
  (GetType-maker target key (or not-found -nil) 
                 (or target-fs ((-FS-var) @(-top-var) @(-top-var)))
                 (or target-object @(-empty-var))))

;not a type, see KwArgsSeq
(u/ann-record KwArgs [mandatory :- (t/Map Type Type)
                      optional  :- (t/Map Type Type)])
(u/def-type KwArgs [mandatory optional]
  "A set of mandatory and optional keywords"
  [(every? (con/hash-c? Value? Type?) [mandatory optional])
   (= #{} (set/intersection (set (keys mandatory)) 
                            (set (keys optional))))])

(u/ann-record KwArgsSeq [mandatory :- (t/Map Type Type)
                         optional  :- (t/Map Type Type)
                         nilable-non-empty? :- Boolean
                         complete? :- Boolean])
(u/def-type KwArgsSeq [mandatory optional nilable-non-empty? complete?]
  "A sequential seq representing a flattened map (for keyword arguments)."
  [(every? (con/hash-c? Value? Type?) [mandatory optional])
   (= #{} (set/intersection (set (keys mandatory)) 
                            (set (keys optional))))
   (con/boolean? nilable-non-empty?)
   (con/boolean? complete?)]
  :methods
  [p/TCType])

(t/ann -kw-args [& :optional {:mandatory (t/Map Type Type)
                              :optional (t/Map Type Type)}
                 -> KwArgs])
(defn -kw-args [& {:keys [mandatory optional]
                   :or {mandatory {} optional {}}}]
  {:post [(KwArgs? %)]}
  (KwArgs-maker mandatory optional))

(t/ann -kw-args-seq [& :optional {:mandatory (t/Map Type Type)
                                  :optional (t/Map Type Type)
                                  :nilable-non-empty? Boolean
                                  :complete? Boolean}
                     -> KwArgsSeq])
(defn -kw-args-seq [& {:keys [mandatory optional nilable-non-empty? complete?]
                       :or {mandatory {} optional {} complete? false
                            nilable-non-empty? false}}]
  {:post [(KwArgsSeq? %)]}
  (KwArgsSeq-maker mandatory optional nilable-non-empty? complete?))

; must go before Result
(u/ann-record FlowSet [normal :- p/IFilter])

;must go before Function
(u/ann-record Result [t :- Type,
                      fl :- p/IFilterSet
                      o :- p/IRObject
                      flow :- FlowSet])

(u/ann-record Function [dom :- (U nil (t/Seqable Type)),
                        rng :- Result,
                        rest :- (U nil Type)
                        drest :- (U nil DottedPretype)
                        kws :- (U nil KwArgs)
                        prest :- (U nil Type)])
(u/def-type Function [dom rng rest drest kws prest]
  "A function arity, must be part of an intersection"
  [(or (nil? dom)
       (sequential? dom))
   (every? Type? dom)
   (Result? rng)
   ;at most one of rest drest or kws can be provided
   (#{0 1} (count (filter identity [rest drest kws prest])))
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (DottedPretype? drest))
   (or (nil? kws)
       (KwArgs? kws))
   (or (nil? prest)
       (Type? prest))]
  :methods
  [p/TCAnyType])

(u/ann-record TopFunction [])
(u/def-type TopFunction []
  "Supertype to all functions"
  []
  :methods
  [p/TCType])

(u/ann-record FnIntersection [types :- (t/NonEmptySeqable Function)])
(u/def-type FnIntersection [types]
  "An ordered intersection of Functions."
  [(seq types)
   (sequential? types)
   (every? Function? types)]
  :methods
  [p/TCType])

(u/ann-record CountRange [lower :- Number,
                          upper :- (U nil Number)])
(u/def-type CountRange [lower upper]
  "A sequence of count between lower (inclusive) and upper (inclusive).
  If upper is nil, between lower and infinity."
  [(con/nat? lower)
   (or (nil? upper)
       (and (con/nat? upper)
            (<= lower upper)))]
  :methods
  [p/TCType])

(u/ann-record GTRange [n :- Number])
(u/def-type GTRange [n]
  "The type of all numbers greater than n"
  [(number? n)]
  :methods
  [p/TCType])

(u/ann-record LTRange [n :- Number])
(u/def-type LTRange [n]
  "The type of all numbers less than n"
  [(number? n)]
  :methods
  [p/TCType])

(t/ann make-CountRange (Fn [Number -> CountRange]
                           [Number (U nil Number) -> CountRange]))
(defn make-CountRange
  ([lower] (make-CountRange lower nil))
  ([lower upper] (CountRange-maker lower upper)))

(t/ann make-ExactCountRange (Fn [Number -> CountRange]))
(defn make-ExactCountRange [c]
  {:pre [(con/nat? c)]}
  (make-CountRange c c))

(declare Result-maker)

(t/ann ^:no-check make-FnIntersection [Function * -> FnIntersection])
(defn make-FnIntersection [& fns]
  {:pre [(every? Function? fns)]}
  (FnIntersection-maker fns))

(u/ann-record NotType [type :- Type])
(u/def-type NotType [type]
  "A type that does not include type"
  [(Type? type)]
  :methods
  [p/TCType])

(u/ann-record DifferenceType [type :- Type
                              without :- (t/Coll Type)])
(u/def-type DifferenceType [type without]
  "A type that does not include type"
  [(Type? type)
   (every? Type? without)]
  :methods
  [p/TCType])

(t/ann -difference [Type Type * -> DifferenceType])
(defn -difference [t & without]
  {:pre [without]}
  (DifferenceType-maker t without))

(u/ann-record ListDots [pre-type :- Type,
                        bound :- (U F B)])
(u/def-type ListDots [pre-type bound]
  "A dotted list"
  [(Type? pre-type)
   ((some-fn F? B?) bound)]
  :methods
  [p/TCType])

(u/ann-record Extends [extends :- (t/NonEmptySeqable Type)
                       without :- (U nil (t/Seqable Type))])
(u/def-type Extends [extends without]
  "A set of ancestors that always and never occur."
  [(every? Type? extends)
   (seq extends)
   (every? Type? without)]
  :methods
  [p/TCType])

(declare FlowSet?)

(u/def-type Result [t fl o flow]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (p/IFilterSet? fl)
   (p/IRObject? o)
   (FlowSet? flow)]
  :methods
  [p/TCAnyType])

(declare ret TCResult?)

(u/ann-record TCResult [t :- Type
                        fl :- p/IFilterSet
                        o :- p/IRObject
                        flow :- FlowSet])

(t/ann Result->TCResult [Result -> TCResult])
(defn Result->TCResult [{:keys [t fl o] :as r}]
  {:pre [(Result? r)]
   :post [(TCResult? %)]}
  (ret t fl o))

(t/ann Result-type* [Result -> Type])
(defn Result-type* [r]
  {:pre [(Result? r)]
   :post [(Type? %)]}
  (:t r))

(t/ann ^:no-check Result-filter* [Result -> p/IFilter])
(defn Result-filter* [r]
  {:pre [(Result? r)]
   :post [(p/IFilter? %)]}
  (:fl r))

(t/ann ^:no-check Result-object* [Result -> p/IRObject])
(defn Result-object* [r]
  {:pre [(Result? r)]
   :post [(p/IRObject? %)]}
  (:o r))

(t/ann ^:no-check Result-flow* [Result -> FlowSet])
(defn Result-flow* [r]
  {:pre [(Result? r)]
   :post [(FlowSet? %)]}
  (:flow r))

(t/ann no-bounds Bounds)
(def no-bounds (Bounds-maker -any (Un) nil))

(t/ann -bounds [Type Type -> Bounds])
(defn -bounds [u l]
  (Bounds-maker u l nil))

;unused
(t/tc-ignore
(defonce ^:dynamic *mutated-bindings* #{})

(defn is-var-mutated? [id]
  (contains? *mutated-bindings* id))
  )

(u/def-type FlowSet [normal]
  "The filter that is true when an expression returns normally ie. not an exception."
  [(p/IFilter? normal)]
  :methods
  [p/IFilter])

(u/def-type TCResult [t fl o flow]
  "This record represents the result of typechecking an expression"
  [(Type? t)
   (p/IFilterSet? fl)
   (p/IRObject? o)
   (FlowSet? flow)]
  ;:methods
  ;[p/TCAnyType]
  )

(t/ann -flow [p/IFilter -> FlowSet])
(defn -flow [normal]
  (FlowSet-maker normal))

(t/ann ^:no-check ret
       (Fn [Type -> TCResult]
           [Type p/IFilterSet -> TCResult]
           [Type p/IFilterSet p/IRObject -> TCResult]
           [Type p/IFilterSet p/IRObject FlowSet -> TCResult]))
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
          (p/IFilterSet? f)
          (p/IRObject? o)
          (FlowSet? flow)]
    :post [(TCResult? %)]}
   (TCResult-maker t f o flow)))

(t/ann ret-t [TCResult -> Type])
(defn ret-t [r]
  {:pre [(TCResult? r)]
   :post [(AnyType? %)]}
  (:t r))

(t/ann ^:no-check ret-f [TCResult -> p/IFilterSet])
(defn ret-f [r]
  {:pre [(TCResult? r)]
   :post [(p/IFilterSet? %)]}
  (:fl r))

(t/ann ^:no-check ret-o [TCResult -> p/IRObject])
(defn ret-o [r]
  {:pre [(TCResult? r)]
   :post [(p/IRObject? %)]}
  (:o r))

(t/ann ret-flow [TCResult -> FlowSet])
(defn ret-flow [r]
  {:pre [(TCResult? r)]
   :post [(FlowSet? %)]}
  (:flow r))

(t/ann ^:no-check flow-normal [FlowSet -> p/IFilter])
(defn flow-normal [f]
  {:pre [(FlowSet? f)]
   :post [(p/IFilter? %)]}
  (:normal f))

;; Utils
;; It seems easier to put these here because of dependencies

(t/ann ^:no-check visit-bounds [Bounds [Type -> Type] -> Bounds])
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

(t/ann ^:no-check make-Result
       (Fn [Type -> Result]
           [Type (U nil p/IFilter) -> Result]
           [Type (U nil p/IFilter) (U nil p/IRObject) -> Result]
           [Type (U nil p/IFilter) (U nil p/IRObject) (U nil FlowSet) -> Result]))
(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil nil))
  ([t f] (make-Result t f nil nil))
  ([t f o] (make-Result t f o nil))
  ([t f o flow]
   (let [-FS @(-FS-var)
         -top @(-top-var)
         -empty @(-empty-var)]
     (Result-maker t (or f (-FS -top -top)) (or o -empty) (or flow (-flow -top))))))

(t/ann ^:no-check make-Function
       [(U nil (t/Seqable Type))
        Type
        & :optional
        {:rest (U nil Type) :drest (U nil Type) :prest (U nil Type)
         :filter (U nil p/IFilterSet) :object (U nil p/IRObject)
         :flow (U nil FlowSet)
         :mandatory-kws (U nil (t/Map Type Type))
         :optional-kws (U nil (t/Map Type Type))}
        -> Function])
(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to the most general filter
  and EmptyObject"
  [dom rng & {:keys [rest drest prest filter object mandatory-kws optional-kws flow]}]
  (let [-FS @(-FS-var)
        -top @(-top-var)
        -empty @(-empty-var)]
    (Function-maker dom (make-Result rng filter object flow)
                    rest drest (when (or mandatory-kws optional-kws)
                                 (-kw-args :mandatory (or mandatory-kws {})
                                           :optional (or optional-kws {}))) prest)))


;;;;;;;;;;;;;;;;;
;; Clojurescript types

(u/ann-record BooleanCLJS [])
(u/def-type BooleanCLJS []
  "Primitive boolean in CLJS"
  []
  :methods
  [p/TCType])

(u/ann-record ObjectCLJS [])
(u/def-type ObjectCLJS []
  "Primitive object in CLJS"
  []
  :methods
  [p/TCType])

(u/ann-record StringCLJS [])
(u/def-type StringCLJS []
  "Primitive string in CLJS"
  []
  :methods
  [p/TCType])

(u/ann-record NumberCLJS [])
(u/def-type NumberCLJS []
  "Primitive number in CLJS"
  []
  :methods
  [p/TCType])

(u/ann-record IntegerCLJS [])
(u/def-type IntegerCLJS []
  "Primitive integer in CLJS"
  []
  :methods
  [p/TCType])

(u/ann-record ArrayCLJS [input-type :- Type
                         output-type :- Type])
(u/def-type ArrayCLJS [input-type output-type]
  "Primitive array in CLJS"
  [(Type? input-type)
   (Type? output-type)]
  :methods
  [p/TCType])

(u/def-type FunctionCLJS []
  "Primitive function in CLJS"
  []
  :methods
  [p/TCType])
