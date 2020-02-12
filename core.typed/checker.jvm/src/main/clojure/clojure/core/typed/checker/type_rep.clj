;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.type-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.impl-protocols :as p]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            clojure.core.typed.contract-ann
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.set :as set]))

(t/defalias SeqNumber Long)

;;; Type rep predicates

(t/defalias Type
  "A normal type"
  p/TCType)

(t/defalias AnyType
  "A normal type or special type like Function."
  (t/U Type p/TCAnyType))

(t/defalias MaybeScopedType
  "A type or a scope"
  (t/U Type p/IScope))

; not a real symmetric predicate, but we always extend Type with the
; interface for speed, so it's sufficient.
; Should just make this an interface to start with.
(t/ann ^:no-check Type? (t/Pred Type))
(defn Type? [a]
  (instance? clojure.core.typed.checker.impl_protocols.TCType a))

; similar for AnyType
(t/ann ^:no-check AnyType? (t/Pred AnyType))
(defn AnyType? [a]
  (or (Type? a)
      (instance? clojure.core.typed.checker.impl_protocols.TCAnyType a)))

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

(t/ann -infer-any Type)
(def -infer-any (with-meta -any {::t/infer true}))

(defn infer-any? [t]
  (and (= -infer-any t)
       (boolean (-> t meta ::t/infer))))

(u/ann-record Unchecked [vsym :- (U nil t/Sym)])
(u/def-type Unchecked [vsym]
  "The unchecked type, like bottom and only introduced 
  per-namespace with the :unchecked-imports feature."
  []
  :methods
  [p/TCType])

(t/ann -unchecked [(U nil t/Sym) :-> Type])
(defn -unchecked [vsym]
  {:pre [((some-fn nil? symbol?) vsym)]}
  (Unchecked-maker vsym))

(u/ann-record TypeOf [vsym :- t/Sym])
(u/def-type TypeOf [vsym]
  "The type of a local or var."
  []
  :methods
  [p/TCType])

(t/ann -type-of [t/Sym :-> Type])
(defn -type-of [vsym]
  {:pre [(symbol? vsym)]}
  (TypeOf-maker vsym))

(u/ann-record Union [types :- (t/SortedSet Type)])
(u/def-type Union [types]
  "An flattened, sorted union of types"
  [(set? types)
   (sorted? types)
   (every? Type? types)
   (not-any? Union? types)]
  :methods
  [p/TCType])

(t/ann ^:no-check sorted-type-set [(t/U nil (t/Seqable Type)) -> (t/SortedSet Type)])
(defn sorted-type-set [ts]
  (apply sorted-set-by u/type-comparator ts))

;temporary union maker
(t/ann Un [Type * -> Union])
(defn- Un [& types]
  (Union-maker (sorted-type-set types)))

(t/ann empty-union Type)
(def empty-union (Un))

(t/ann Bottom [-> Type])
(defn Bottom []
  empty-union)

(t/ann -nothing Type)
(def -nothing (Bottom))

(t/ann Bottom? [t/Any -> Boolean])
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
(def -error Err)

(u/ann-record Intersection [types :- (t/I t/NonEmptyCount 
                                          (t/SortedSet Type))])
(u/def-type Intersection [types]
  "An unordered intersection of types."
  [(sorted? types)
   (set? types)
   (seq types)
   (every? Type? types)]
  :methods 
  [p/TCType])

(t/defalias Variance
  "Keywords that represent a certain variance"
  (t/U ':constant ':covariant ':contravariant ':invariant ':dotted))

(t/ann variances (t/Set Variance))
(def variances #{:constant :covariant :contravariant :invariant :dotted})

(t/ann ^:no-check variance? (t/Pred Variance))
(defn variance? [v]
  (contains? variances v))

(declare Scope? TypeFn?)

(u/ann-record Bounds [upper-bound :- MaybeScopedType
                      lower-bound :- MaybeScopedType
                      higher-kind :- nil])
(u/def-type Bounds [upper-bound lower-bound higher-kind]
  "A type bound or higher-kind bound on a variable"
  [(every? (some-fn Type? Scope?) [upper-bound lower-bound])
   ;deprecated/unused
   (nil? higher-kind)])

(u/ann-record B [idx :- Number])
(u/def-type B [idx]
  "de Bruijn indices - should never appear outside of this file.
  Bound type variables"
  [(nat-int? idx)]
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
  (t/U Type Scope))

(t/ann ^:no-check scoped-Type? (t/Pred (t/U Scope Type)))
(def scoped-Type? (some-fn Scope? Type?))

(t/ann ^:no-check scope-depth? [Scope Number -> t/Any])
(defn scope-depth? 
  "True if scope is has depth number of scopes nested"
  [scope depth]
  {:pre [(Scope? scope)
         (nat-int? depth)]}
  (Type? (last (take (inc depth) (iterate #(and (Scope? %)
                                                (:body %))
                                          scope)))))

(u/ann-record RClass [variances :- (t/U nil (t/NonEmptySeqable Variance))
                      poly? :- (t/U nil (t/NonEmptySeqable Type))
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

(u/ann-record JSNominal [variances :- (t/U nil (t/NonEmptySeqable Variance))
                         kind :- (t/U ':interface ':class)
                         poly? :- (t/U nil (t/NonEmptySeqable Type))
                         name :- t/Sym
                         ctor :- (t/U nil MaybeScopedType)
                         instance-properties :- (t/Map t/Sym MaybeScopedType)
                         static-properties :- (t/Map t/Sym Type)])
(u/def-type JSNominal [variances kind poly? name ctor instance-properties static-properties]
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
   ((some-fn nil? Type?) ctor)
   (#{:interface :class} kind)
   ((con/hash-c? symbol? (some-fn Scope? Type?)) instance-properties)
   ((con/hash-c? symbol? Type?) static-properties)
   (symbol? name)]
  :methods
  [p/TCType])

(u/ann-record DataType [the-class :- t/Sym,
                        variances :- (t/U nil (t/NonEmptySeqable Variance)),
                        poly? :- (t/U nil (t/NonEmptySeqable Type)),
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
   (boolean? record?)]
  :methods
  [p/TCType])

(t/ann ^:no-check DataType->Class [DataType -> Class])
(defn ^Class DataType->Class [^DataType dt]
  (coerce/symbol->Class (.the-class dt)))

(t/ann Record? [t/Any -> Boolean])
(defn Record? [^DataType a]
  (boolean
    (when (DataType? a)
      (.record? a))))

(u/ann-record Protocol [the-var :- t/Sym,
                        variances :- (t/U nil (t/NonEmptySeqable Variance)),
                        poly? :- (t/U nil (t/NonEmptySeqable Type)),
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
   (boolean? declared?)]
  :methods
  [p/TCType])

(u/ann-record TypeFn [nbound :- Number,
                      variances :- (t/U nil (t/Seqable Variance))
                      bbnds :- (t/U nil (t/Seqable Bounds)),
                      scope :- p/IScope])
(u/def-type TypeFn [nbound variances bbnds scope]
  "A type function containing n bound variables with variances.
  It is of a higher kind"
  [(nat-int? nbound)
   (every? variance? variances)
   (every? Bounds? bbnds)
   (apply = nbound (map count [variances bbnds]))
   (scope-depth? scope nbound)
   (Scope? scope)]
  :methods
  [p/TCType])

(u/ann-record Poly [nbound :- Number,
                    bbnds :- (t/U nil (t/Seqable Bounds)),
                    scope :- p/IScope
                    named :- (t/Map t/Sym t/Int)])
(u/def-type Poly [nbound bbnds scope named]
  "A polymorphic type containing n bound variables.
  `named` is a map of free variable names to de Bruijn indices (range nbound)"
  [(nat-int? nbound)
   (every? Bounds? bbnds)
   (apply = nbound (map count [bbnds]))
   (scope-depth? scope nbound)
   (Scope? scope)
   (map? named)
   (every? symbol? (keys named))
   (every? #(<= 0 % (dec nbound)) (vals named))
   (or (empty? named)
       (apply distinct? (vals named)))
   ]
  :methods
  [p/TCType])

(u/ann-record PolyDots [nbound :- Number,
                        bbnds :- (t/U nil (t/Seqable Bounds)),
                        scope :- p/IScope
                        named :- (t/Map t/Sym t/Int)])
(u/def-type PolyDots [nbound bbnds scope named]
  "A polymorphic type containing n-1 bound variables and 1 ... variable
  `named` is a map of free variable names to de Bruijn indices (range nbound)."
  [(nat-int? nbound)
   (every? Bounds? bbnds)
   (= nbound (count bbnds))
   (scope-depth? scope nbound)
   (Scope? scope)
   (map? named)
   (every? symbol? (keys named))
   (every? #(<= 0 % (dec nbound)) (vals named))
   (or (empty? named)
       (apply distinct? (vals named)))]
  :methods
  [p/TCType])

(defn ^:private unsafe-body [pred p]
  {:pre [(pred p)]
   :post [((every-pred Type? (complement Scope?)) %)]}
  (let [sc (atom (:scope p))]
    (dotimes [n (:nbound p)]
      (let [s @sc
            _ (assert (Scope? s))]
        (reset! sc (:body s))))
    @sc))

(defn Poly-body-unsafe* [p]
  (unsafe-body Poly? p))

(defn PolyDots-body-unsafe* [p]
  (unsafe-body PolyDots? p))

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

(u/ann-record Value [val :- t/Any])
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

(t/ann -val [t/Any -> Type])
(def -val Value-maker)

(t/ann-many Type 
            -false -true -nil -falsy)
(def -false (-val false))
(def -true (-val true))
(def -nil (-val nil))
(def -falsy (Un -nil -false))

(t/ann-many [t/Any -> Boolean]
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
   (boolean? other-keys?)]
  :methods
  [p/TCType])

(u/ann-record JSObj [types :- (t/Map t/Kw Type)])
(u/def-type JSObj [types]
  "A JavaScript structural object"
  [((con/hash-c? keyword? Type?) types)]
  :methods
  [p/TCType])

(u/ann-record DottedPretype [pre-type :- Type,
                             name :- (t/U t/Sym Number)
                             partition-count :- Number])
(u/def-type DottedPretype [pre-type name partition-count]
  "A dotted pre-type. Not a type"
  [(Type? pre-type)
   ((some-fn symbol? nat-int?) name)
   (nat-int? partition-count)]
  :methods
  [p/TCAnyType])

(t/ann-many [Type (t/U t/Sym Number) -> DottedPretype]
            DottedPretype1-maker
            DottedPretype2-maker)

(defn DottedPretype1-maker [pre-type name]
  (DottedPretype-maker pre-type name 1))

(defn DottedPretype2-maker [pre-type name]
  (DottedPretype-maker pre-type name 2))

(u/ann-record HSequential [types :- (Seqable Type)
                           fs :- (t/Vec p/IFilterSet)
                           objects :- (t/Vec p/IRObject)
                           ;variable members to the right of fixed
                           rest :- (U nil Type)
                           drest :- (U nil DottedPretype)
                           repeat :- Boolean
                           kind :- t/Kw])
(u/def-type HSequential [types fs objects rest drest repeat kind]
  "A constant Sequential, clojure.lang.Sequential"
  [(sequential? types)
   (every? (some-fn Type? Result?) types)
   (vector? fs)
   (every? p/IFilterSet? fs)
   (vector? objects)
   (every? p/IRObject? objects)
   (apply = (map count [types fs objects]))
   (#{0 1} (count (filter identity [rest drest repeat])))
   (if repeat (not-empty types) true)
   ((some-fn nil? Type?) rest)
   ((some-fn nil? DottedPretype?) drest)
   ((some-fn true? false?) repeat)
   (#{:list :seq :vector :sequential} kind)]
  :methods
  [p/TCType])

(t/ann ^:no-check -hsequential
       [(Seqable Type) & :optional {:filters (Seqable p/IFilterSet) :objects (Seqable p/IRObject)
                                  :rest (U nil Type) :drest (U nil DottedPretype) :repeat Boolean} -> Type])
(defn -hsequential
  [types & {:keys [filters objects rest drest kind] repeat? :repeat}]
  (if (some Bottom? types)
    (Bottom)
    (HSequential-maker types
                       (if filters
                         (vec filters)
                         (vec (repeat (count types) (ind/-FS (ind/-top-fn)
                                                             (ind/-top-fn)))))
                       (if objects
                         (vec objects)
                         (vec (repeat (count types) (ind/-empty-fn))))
                       rest
                       drest
                       (if repeat? true false)
                       (or kind :sequential))))

(defn compatible-HSequential-kind?
  "True if kind s is a subtype of kind t."
  [s t]
  (or (= s t)
      (= :sequential t)
      (and (= s :list)
           (= t :seq))))

(defn HeterogeneousList? [t]
  (and (HSequential? t)
       (= :list (:kind t))))

;; TODO delete
(defn HeterogeneousList-maker [types]
  (-hsequential types :kind :list))

(defn HeterogeneousSeq? [t]
  (and (HSequential? t)
       (= :seq (:kind t))))

(t/ann ^:no-check -hseq
       [(Seqable Type) & :optional {:filters (Seqable p/IFilterSet) :objects (Seqable p/IRObject)
                                    :rest (U nil Type) :drest (U nil DottedPretype) :repeat Boolean} -> Type])
(defn -hseq
  [types & opts]
  (apply -hsequential types (concat opts [:kind :seq])))

(defn HeterogeneousVector? [t]
  (and (HSequential? t)
       (= :vector (:kind t))))

(t/ann ^:no-check -hvec
       [(t/Vec Type) & :optional {:filters (Seqable p/IFilterSet) :objects (Seqable p/IRObject)
                                  :rest (U nil Type) :drest (U nil DottedPretype) :repeat Boolean} -> Type])
(defn -hvec
  [types & opts]
  (apply -hsequential types (concat opts [:kind :vector])))

(u/ann-record HSet [fixed :- (t/Set Type)
                    complete? :- Boolean])
(u/def-type HSet [fixed complete?]
  "A constant set"
  [(every? Type? fixed)
   (set? fixed)
   (boolean? complete?)]
  :methods
  [p/TCType])

(t/ann -hset [(t/Set Type) & :optional {:complete? Boolean} -> HSet])
(defn -hset [fixed & {:keys [complete?] :or {complete? true}}]
  (HSet-maker fixed complete?))

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
                         dentries :- (t/U nil DottedPretype)])
(u/def-type AssocType [target entries dentries]
  "An assoc[iate] operation on the type level"
  [(Type? target)
   (or (DottedPretype? dentries)
       (nil? dentries))
   (and (every? (con/hvector-c? Type? Type?) entries)
        (sequential? entries))]
  :methods
  [p/TCType])

(u/ann-record DissocType [target :- Type,
                          keys :- (t/Coll Type)
                          dkeys :- (t/U nil DottedPretype)])
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
       [Type Type & :optional {:not-found (t/U nil Type)
                               :target-fs (t/U nil p/IFilterSet)
                               :target-object (t/U nil p/IRObject)}
        -> GetType])
(defn -get 
  [target key & {:keys [not-found target-fs target-object]}]
  (GetType-maker target key (or not-found -nil) 
                 (or target-fs (ind/-FS (ind/-top-fn) 
                                        (ind/-top-fn)))
                 (or target-object (ind/-empty-fn))))

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
   (boolean? nilable-non-empty?)
   (boolean? complete?)]
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

(declare HSequential? DottedPretype?)

(u/ann-record Function [dom :- (t/U nil (t/Seqable Type)),
                        rng :- Result,
                        rest :- (U nil Type)
                        drest :- (U nil DottedPretype)
                        kws :- (U nil KwArgs)
                        prest :- (U nil Type)
                        pdot :- (U nil DottedPretype)])
(u/def-type Function [dom rng rest drest kws prest pdot]
  "A function arity, must be part of an intersection"
  [(or (nil? dom)
       (sequential? dom))
   (every? Type? dom)
   (Result? rng)
   ;at most one of rest drest kws prest or pdot can be provided
   (#{0 1} (count (filter identity [rest drest kws prest pdot])))
   ; we could have prest without repeat, but why would you do that
   (or (nil? prest)
       (HSequential? prest))
   (or (nil? pdot)
       (DottedPretype? pdot))
   (if prest (and (:repeat prest) (:types prest)) true)
   ; we could have pdot without repeat, but why would you do that
   (if pdot
     (and (-> pdot :pre-type :repeat)
          (-> pdot :pre-type :types))
     true)
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (DottedPretype? drest))
   (or (nil? kws)
       (KwArgs? kws))
   (or (nil? prest)
       (Type? prest))
   (or (nil? pdot)
       (DottedPretype? pdot))]
  :methods
  [p/TCAnyType])

(u/ann-record TopFunction [])
(u/def-type TopFunction []
  "Supertype to all functions"
  []
  :methods
  [p/TCType])

(u/ann-record FnIntersection [types :- (t/NonEmptyVec Function)])
(u/def-type FnIntersection [types]
  "An ordered intersection of Functions."
  [(seq types)
   (vector? types)
   (every? Function? types)]
  :methods
  [p/TCType])

(u/ann-record CountRange [lower :- Number,
                          upper :- (t/U nil Number)])
(u/def-type CountRange [lower upper]
  "A sequence of count between lower (inclusive) and upper (inclusive).
  If upper is nil, between lower and infinity."
  [(nat-int? lower)
   (or (nil? upper)
       (and (nat-int? upper)
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

(t/ann make-CountRange (t/IFn [Number -> CountRange]
                           [Number (t/U nil Number) -> CountRange]))
(defn make-CountRange
  ([lower] (make-CountRange lower nil))
  ([lower upper] (CountRange-maker lower upper)))

(t/ann make-ExactCountRange (t/IFn [Number -> CountRange]))
(defn make-ExactCountRange [c]
  {:pre [(nat-int? c)]}
  (make-CountRange c c))

(declare Result-maker)

(t/ann ^:no-check make-FnIntersection [Function * -> FnIntersection])
(defn make-FnIntersection [& fns]
  {:pre [(every? Function? fns)]}
  (FnIntersection-maker (vec fns)))

(u/ann-record NotType [type :- Type])
(u/def-type NotType [type]
  "A type that does not include type"
  [(Type? type)]
  :methods
  [p/TCType])

(u/ann-record DifferenceType [type :- Type
                              without :- (t/SortedSet Type)])
(u/def-type DifferenceType [type without]
  "A type that does not include type"
  [(Type? type)
   (every? Type? without)
   (set? without)
   (sorted? without)]
  :methods
  [p/TCType])

(t/ann -difference [Type Type * -> DifferenceType])
(defn -difference [t & without]
  {:pre [without]}
  (DifferenceType-maker t (sorted-type-set without)))

(u/ann-record ListDots [pre-type :- Type,
                        bound :- (t/U F B)])
(u/def-type ListDots [pre-type bound]
  "A dotted list"
  [(Type? pre-type)
   ((some-fn F? B?) bound)]
  :methods
  [p/TCType])

(u/ann-record Extends [extends :- (t/I (t/SortedSet Type)
                                       t/NonEmptyCount)
                       without :- (t/SortedSet Type)])
(u/def-type Extends [extends without]
  "A set of ancestors that always and never occur."
  [(every? Type? extends)
   (set? extends)
   (sorted? extends)
   (seq extends)
   (every? Type? without)
   (set? without)
   (sorted? without)]
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

(declare ret TCResult? make-Result)

(u/ann-record TCResult [t :- Type
                        fl :- p/IFilterSet
                        o :- p/IRObject
                        flow :- FlowSet
                        opts :- (t/Map t/Any t/Any)])

(t/ann Result->TCResult [Result -> TCResult])
(defn Result->TCResult [{:keys [t fl o flow] :as r}]
  {:pre [(Result? r)]
   :post [(TCResult? %)]}
  (ret t fl o flow))

(t/ann TCResult->Result [TCResult -> Result])
(defn TCResult->Result [{:keys [t fl o flow] :as r}]
  {:pre [(Result? r)]
   :post [(TCResult? %)]}
  (make-Result t fl o flow))

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

(u/def-type TCResult [t fl o flow opts]
  "This record represents the result of typechecking an expression"
  [(Type? t)
   (p/IFilterSet? fl)
   (p/IRObject? o)
   (FlowSet? flow)
   (map? opts)]
  ;:methods
  ;[p/TCAnyType]
  )

(t/ann -flow [p/IFilter -> FlowSet])
(defn -flow [normal]
  (FlowSet-maker normal))

(t/ann ^:no-check ret
       (t/IFn [Type -> TCResult]
           [Type p/IFilterSet -> TCResult]
           [Type p/IFilterSet p/IRObject -> TCResult]
           [Type p/IFilterSet p/IRObject FlowSet -> TCResult]))
(defn ret
  "Convenience function for returning the type of an expression"
  ([t] 
   (ret t (ind/-FS (ind/-top-fn) (ind/-top-fn)) (ind/-empty-fn) (-flow (ind/-top-fn))))
  ([t f] 
   (ret t f (ind/-empty-fn) (-flow (ind/-top-fn))))
  ([t f o] 
   (ret t f o (-flow (ind/-top-fn))))
  ([t f o flow]
   {:pre [(AnyType? t)
          (p/IFilterSet? f)
          (p/IRObject? o)
          (FlowSet? flow)]
    :post [(TCResult? %)]}
   (TCResult-maker t f o flow {})))

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
       (t/IFn [Type -> Result]
           [Type (t/U nil p/IFilter) -> Result]
           [Type (t/U nil p/IFilter) (t/U nil p/IRObject) -> Result]
           [Type (t/U nil p/IFilter) (t/U nil p/IRObject) (t/U nil FlowSet) -> Result]))
(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil nil))
  ([t f] (make-Result t f nil nil))
  ([t f o] (make-Result t f o nil))
  ([t f o flow]
   (Result-maker t 
                 (or f (ind/-FS (ind/-top-fn) (ind/-top-fn))) 
                 (or o (ind/-empty-fn)) 
                 (or flow (-flow (ind/-top-fn))))))

(t/ann ^:no-check make-Function
       [(U nil (t/Seqable Type))
        Type
        & :optional
        {:rest (U nil Type) :drest (U nil Type) :prest (U nil Type)
         :pdot (U nil DottedPretype)
         :filter (U nil p/IFilterSet) :object (U nil p/IRObject)
         :flow (U nil FlowSet)
         :mandatory-kws (U nil (t/Map Type Type))
         :optional-kws (U nil (t/Map Type Type))}
        -> Function])
(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to the most general filter
  and EmptyObject"
  [dom rng & {:keys [rest drest prest pdot filter object mandatory-kws optional-kws flow] :as opt}]
  {:pre [(every? keyword? (keys opt))]}
  (Function-maker dom (make-Result rng filter object flow)
                  rest drest (when (or mandatory-kws optional-kws)
                               (-kw-args :mandatory (or mandatory-kws {})
                                         :optional (or optional-kws {}))) prest pdot))

;; Symbolic closures

(def enable-symbolic-closures? false)

(u/def-type SymbolicClosure [fexpr env]
  "Symbolic closure"
  [(map? fexpr)]
  :methods
  [p/TCType])

(defn symbolic-closure [fexpr env]
  (prn "creating symbolic-closure")
  (SymbolicClosure-maker fexpr env))

;;;;;;;;;;;;;;;;;
;; Clojurescript types

(u/ann-record JSUndefined [])
(u/def-type JSUndefined []
  "JavaScript undefined"
  []
  :methods
  [p/TCType])

(u/ann-record JSNull [])
(u/def-type JSNull []
  "JavaScript null"
  []
  :methods
  [p/TCType])

(u/ann-record JSBoolean [])
(u/def-type JSBoolean []
  "JavaScript primitive boolean"
  []
  :methods
  [p/TCType])

(u/ann-record JSObject [])
(u/def-type JSObject []
  "Any JavaScript object"
  []
  :methods
  [p/TCType])

(u/ann-record JSString [])
(u/def-type JSString []
  "JavaScript primitive string"
  []
  :methods
  [p/TCType])

(u/ann-record JSSymbol [])
(u/def-type JSSymbol []
  "JavaScript primitive symbol"
  []
  :methods
  [p/TCType])

(u/ann-record JSNumber [])
(u/def-type JSNumber []
  "JavaScript number"
  []
  :methods
  [p/TCType])

(u/ann-record CLJSInteger [])
(u/def-type CLJSInteger []
  "ClojureScript integer. Represents a primitive
  JavaScript number with no decimal places (values
  that pass `cljs.core/integer?`)."
  []
  :methods
  [p/TCType])

(t/ann -integer-cljs Type)
(def -integer-cljs (CLJSInteger-maker))

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
