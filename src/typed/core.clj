(set! *warn-on-reflection* false)

(ns typed.core
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef AReference ARef IDeref IReference APersistentSet PersistentHashSet Sorted))
  (:require [analyze.core :refer [ast] :as analyze]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [trammel.core :as contracts]
            [clojure.math.combinatorics :as comb]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))

(def third (comp second next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint shorthands

(def boolean? (some-fn true? false?))

(defn hvector-c? [& ps]
  (apply every-pred vector?
         (map (fn [p i] #(p (nth % i false))) ps (range))))

(defn array-map-c? [ks-c? vs-c?]
  (every-pred #(instance? PersistentArrayMap %)
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn hash-c? [ks-c? vs-c?]
  (every-pred map?
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn set-c? [c?]
  (every-pred set?
              #(every? c? %)))

(defn sequential-c? [c?]
  (every-pred sequential?
              #(every? c? %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions

(defn tc-pr-env 
  "Print the current type environment, and debug-string"
  [debug-string] nil)
(defn tc-pr-filters [debug-string frm] frm)

(defn inst-poly [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defn fn>-ann [fn-of param-types-syn]
  fn-of)

(defn pfn>-ann [fn-of polys param-types-syn]
  fn-of)

(defn loop>-ann [loop-of bnding-types]
  loop-of)

(defmacro pfn> 
  "Define a polymorphic anonymous function."
  [poly & forms]
  (let [methods (if (vector? (first forms))
                  (list forms)
                  forms)
        ;(pfn> [[a :- Number] & [n :- Number *]] a) 
        method-doms (for [[arg-anns] methods]
                      (let [[required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                        (assert (not rest-param) "pfn> doesn't support rest parameters yet")
                        (map (comp second next) required-params)))]
    `(pfn>-ann (fn ~@(for [[params & body] methods]
                       (apply list (vec (map first params)) body)))
               '~poly
               '~method-doms)))

(defmacro fn> 
  "Define a typed anonymous function."
  [& forms]
  (let [methods (if (vector? (first forms))
                  (list forms)
                  forms)
        ;(fn> [[a :- Number] & [n :- Number *]] a) 
        method-doms (for [[arg-anns] methods]
                      (let [[required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                        (assert (not rest-param) "fn> doesn't support rest parameters yet")
                        (map (comp second next) required-params)))]
    `(fn>-ann (fn ~@(for [[params & body] methods]
                      (apply list (vec (map first params)) body)))
              '~method-doms)))

(defmacro loop>
  "Define a typed loop"
  [bndings* & forms]
  (let [bnds (partition 2 bndings*)
        ; [[lhs :- bnd-ann] rhs]
        lhs (map ffirst bnds)
        rhs (map second bnds)
        bnd-anns (map #(-> % first next second) bnds)]
    `(loop>-ann (loop ~(vec (mapcat vector lhs rhs))
                  ~@forms)
                '~bnd-anns)))

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
    (assert (not (or (some #(= \. %) (str sym#))
                     (namespace sym#)))
            (str "Cannot declare qualified datatype: " sym#))
    (let [qsym# (symbol (str (munge (name (ns-name *ns*))) \. (name sym#)))]
      (declare-datatype* qsym#)))))

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
     (let [qsym# (if (namespace sym#)
                   sym#
                   (symbol (str (name (ns-name *ns*))) (name sym#)))]
       (declare-protocol* qsym#)))))

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
     (let [qsym# (if (namespace sym#)
                   sym#
                   (symbol (name (ns-name *ns*)) (name sym#)))]
       (declare-name* qsym#)))))

(defmacro def-alias 
  "Define a type alias"
  [sym type]
  `(tc-ignore
  (let [sym# (if (namespace '~sym)
                '~sym
                (symbol (name (ns-name *ns*)) (name '~sym)))
         ty# (parse-type '~type)]
     (add-type-name sym# ty#)
     (declare ~sym)
     [sym# (unparse-type ty#)])))

(defn ann-form* [form ty]
  form)

(defmacro ann-form [form ty]
  `(ann-form* ~form '~ty))


(defn tc-ignore-forms* [r]
  r)

(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(tc-ignore-forms* (do
                      ~@body)))

(defmacro non-nil-return 
  "Override the return type of method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg.  (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(tc-ignore
  (add-nonnilable-method-return '~msym '~arities)))

(defmacro nilable-param 
  "Overrides which parameters in a method may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(tc-ignore
  (add-method-nilable-param '~msym '~mmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defmacro defrecord [name slots inv-description invariants & etc]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc)))

(declare abstract-many instantiate-many)

;(defn- comp-mm [mm disps]
;  (set/difference disps (set (keys (methods mm)))))
;
;(comp-mm replace-image (disj kinds :scope))
;(comp-mm replace-image (disj kinds :scope))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(def nat? (every-pred integer? (complement neg?)))

(def Type ::Type)

(defn Type? [a]
  (isa? (class a) Type))

(declare TCResult? Result? Function?)

(defn AnyType? [a]
  ((some-fn Type? TCResult? Result? Function?)
     a))

(defn declare-type [a]
  (derive a Type))

(defrecord Top []
  "The top type"
  [])

(def -any (->Top))

(declare-type Top)

;FIXME proper union maker, with sorted types
(defrecord Union [types]
  "An flattened, unordered union of types"
  [(set? types)
   (every? Type? types)
   (not (some Union? types))])

(declare-type Union)

(declare HeterogeneousMap?)

(def empty-union (->Union #{}))

#_(defn simplify-HMap-Un [hmaps]
  {:pre [(every? HeterogeneousMap? hmaps)]
   :post [(Type? %)]}
  (let [mss (vals
              (group-by #(-> % :types keys set) (set hmaps)))
        ;union the vals of maps with exactly the same keys
        flat (set
               (for [ms mss]
                 (->HeterogeneousMap
                   (apply merge-with Un
                          (map :types ms)))))]
    (if (= 1 (count flat))
      (first flat)
      (->Union flat))))

(defn Un [& types]
  (let [types (disj (set types) empty-union)]
    (cond
      (empty? types) empty-union
      (= 1 (count types)) (first types)
;      (every? HeterogeneousMap? types) (simplify-HMap-Un types)
      :else (->Union (set (apply concat
                                 (for [t (set types)]
                                   (if (Union? t)
                                     (:types t)
                                     [t]))))))))

(defn Bottom []
  empty-union)

(defn Bottom? [a]
  (= empty-union a))

(declare Fn-Intersection? Function? Poly? PolyDots?)

(defrecord Intersection [types]
  "An ordered intersection of types. Either is an intersection
  of Functions, or contains at most one Function/Poly/PolyDots type"
  [(seq types)
   (or (every? Function? types)
       (<= (count (filter (some-fn Fn-Intersection? Poly? PolyDots?) types))
           1)
       (every? Type? types))])

(declare In HeterogeneousMap? ->HeterogeneousMap overlap)

(defn simplify-HMap-In [hmaps]
  {:pre [(every? HeterogeneousMap? hmaps)]
   :post [(Type? %)]}
  (let [mss (vals
              (group-by #(-> % :types keys set) (set hmaps)))
        ;intersect the vals of maps with exactly the same keys
        flat (set
               (for [ms mss]
                 (->HeterogeneousMap
                   (apply merge-with In
                          (map :types ms)))))]
    (if (= 1 (count flat))
      (first flat)
      (->Intersection flat))))

(defn In [& types]
  {:post [(Type? %)]}
           ;flatten intersections
  (let [ts (set (apply concat
                       (for [t (set types)]
                         (if (Intersection? t)
                           (:types t)
                           [t]))))]
    (cond
      (or (empty? ts)
          (ts (Un))) (Bottom)

      ; should be near the top
      (some Function? types) (do (assert (every? Function? types)
                                         "Every type must be a Function in a Fn-Intersection")
                               (->Intersection types))

      (= 1 (count ts)) (first ts)

      ; if there no overlap
      (and (<= (count ts) 2)
           (some (fn [[t1 t2]] (not (overlap t1 t2))) (comb/combinations ts 2))) (Bottom)

      (some Union? ts) (let [flat (set (mapcat #(if (Union? %)
                                                  (:types %)
                                                  [%])
                                               ts))]
                         (apply Un
                                (set
                                  (for [[t1 t2] (comb/combinations flat 2)]
                                    (In t1 t2)))))

      (every? HeterogeneousMap? ts) (simplify-HMap-In ts)
      (ts -any) (apply In (disj ts -any))
      :else (->Intersection ts))))

(declare-type Intersection)

(def variances #{:constant :covariant :contravariant :invariant :dotted})

(defn variance? [v]
  (contains? variances v))

(declare Scope?)

(defrecord Bounds [upper-bound lower-bound]
  "A bound on a type"
  [((some-fn Type? Scope?) upper-bound)
   ((some-fn Type? Scope?) lower-bound)])

(defrecord B [idx]
  "A bound variable. Should not appear outside this file"
  [(nat? idx)])

(declare-type B)

(defrecord F [name]
  "A named free variable"
  [(symbol? name)])

(defn make-F
  "Make a free variable "
  [name] (->F name))

(declare-type F)

(declare Scope?)

(defrecord Scope [body]
  "A scope that contains one bound variable, can be nested. Not used directly"
  [((some-fn Type? Scope?) body)])

(defrecord RClass [variances poly? the-class replacements]
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
   ((hash-c? symbol? (some-fn Type? Scope?)) replacements)])

(declare-type RClass)

(declare RESTRICTED-CLASS instantiate-poly Class->symbol)

(defn RClass-of 
  ([sym-or-cls] (RClass-of sym-or-cls nil))
  ([sym-or-cls args]
   {:pre [((some-fn class? symbol?) sym-or-cls)
          (every? Type? args)]
    :post [(RClass? %)]}
   (let [sym (if (class? sym-or-cls)
               (Class->symbol sym-or-cls)
               sym-or-cls)
         rc (@RESTRICTED-CLASS sym)]
     (assert ((some-fn Poly? RClass? nil?) rc))
     (assert (or (Poly? rc) (not args)) (str "Cannot instantiate non-polymorphic RClass " sym))
     (cond 
       (Poly? rc) (instantiate-poly rc args)
       (RClass? rc) rc
       :else (->RClass nil nil sym {})))))

(declare Poly* no-bounds)

;smart constructor
(defn RClass* [names variances poly? the-class replacements]
  {:pre [(every? symbol? names)
         (every? variance? variances)
         (= (count variances) (count poly?))
         (every? Type? poly?)
         (symbol? the-class)]}
  (prn ((hash-c? symbol? Type?) replacements))
  (if (seq variances)
    (Poly* names (repeat (count names) no-bounds) (->RClass variances poly? the-class replacements))
    (->RClass nil nil the-class replacements)))

(declare poly-RClass-from)

(declare substitute-many unparse-type Class->symbol symbol->Class)

(defn RClass-supers* 
  "Return a set of ancestors to the RClass"
  [{:keys [poly? replacements the-class] :as rcls}]
  {:pre [(RClass? rcls)]
   :post [(set? %)
          (every? Type? %)
          (<= (count (filter (some-fn Fn-Intersection? Poly? PolyDots?) %))
              1)]}
  (let [;set of symbols of Classes we haven't explicitly replaced
        not-replaced (set/difference (set (map Class->symbol (-> the-class symbol->Class supers)))
                                     (set (keys replacements)))]
    (set/union (set (for [csym not-replaced]
                      (RClass-of csym nil)))
               (set (vals replacements))
               #{(RClass-of Object)})))

(defrecord Record [the-class fields]
  "A record"
  [(class? the-class)
   ((array-map-c? symbol? Type?) fields)])

(declare-type Record)

(defrecord DataType [the-class variances poly? fields]
  "A Clojure datatype"
  [(or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (symbol? the-class)
   ((array-map-c? symbol? (some-fn Scope? Type?)) fields)])

(declare-type DataType)

(defrecord Protocol [the-var variances poly? on-class methods]
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
   ((hash-c? (every-pred symbol? (complement namespace)) Type?) methods)])

(declare-type Protocol)

(defrecord Poly [nbound bbnds scope]
  "A polymorphic type containing n bound variables"
  [(nat? nbound)
   (every? Bounds? bbnds)
   (Scope? scope)])

(declare-type Poly)

;smart constructor
(defn Poly* [names bbnds body]
  {:pre [(every? symbol names)
         (every? Bounds? bbnds)
         (Type? body)]}
  (if (empty? names)
    body
    (->Poly (count names) 
            (vec
              (for [bnd bbnds]
                (-> bnd
                  (update-in [:upper-bound] #(abstract-many names %))
                  (update-in [:lower-bound] #(abstract-many names %)))))
            (abstract-many names body))))

;smart destructor
(defn Poly-body* [names poly]
  {:pre [(every? symbol? names)
         (Poly? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (:scope poly)))

(defn Poly-bbnds* [names poly]
  {:pre [(every? symbol? names)
         (Poly? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (-> b
            (update-in [:upper-bound] #(instantiate-many names %))
            (update-in [:lower-bound] #(instantiate-many names %))))
        (:bbnds poly)))

(defrecord PolyDots [nbound bbnds scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(nat? nbound)
   (every? Bounds? bbnds)
   (Scope? scope)])

(declare-type PolyDots)

;smart constructor
(defn PolyDots* [names bbnds body]
  {:pre [(every? symbol names)
         (every? Bounds? bbnds)
         (Type? body)]}
  (assert (= (count names) (count bbnds)) "Wrong number of names")
  (if (empty? names)
    body
    (->PolyDots (count names) 
                (vec
                  (for [bnd bbnds]
                    (-> bnd
                      (update-in [:upper-bound] #(abstract-many names %))
                      (update-in [:lower-bound] #(abstract-many names %)))))
                (abstract-many names body))))

;smart destructor
(defn PolyDots-body* [names poly]
  {:pre [(every? symbol? names)
         (PolyDots? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (:scope poly)))

(defn PolyDots-bbnds* [names poly]
  {:pre [(every? symbol? names)
         (PolyDots? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (-> b
            (update-in [:upper-bound] #(instantiate-many names %))
            (update-in [:lower-bound] #(instantiate-many names %))))
        (:bbnds poly)))

(defrecord Name [id]
  "A late bound name"
  [((every-pred (some-fn namespace (fn [a] (some (fn [c] (= \. c)) (str a))))
                symbol?) 
     id)])

(defrecord App [rator rands]
  "An application of a polymorphic type to type arguments"
  [(Type? rator)
   (every? Type? rands)])

(declare -resolve)

(declare ->t-subst subst-all Mu? unfold)

(defn make-simple-substitution [vs ts]
  {:pre [(every? symbol? vs)
         (every? Type? ts)
         (= (count vs)
            (count ts))]}
  (into {} (for [[v t] (map vector vs ts)]
             [v (->t-subst t no-bounds)])))

(defn instantiate-poly [t types]
  (cond
    (Poly? t) (do (assert (= (:nbound t) (count types)) (str "Wrong number of arguments passed to polymorphic type: "
                                                             (unparse-type t) (mapv unparse-type types)))
                (let [nms (repeatedly (:nbound t) gensym)
                      body (Poly-body* nms t)]
                  (subst-all (make-simple-substitution nms types) body)))
    ;PolyDots NYI
    :else (throw (Exception. "instantiate-poly: requires Poly, and PolyDots NYI"))))

(declare ^:dynamic *current-env* resolve-app*)

(defn resolve-App [app]
  {:pre [(App? app)]}
  (resolve-app* (:rator app) (:rands app)))

(defn resolve-app* [rator rands]
  (let [r (-resolve rator)]
    (cond
      (Poly? rator) (do (assert (= (count rands) (:nbound rator))
                                (str "Wrong number of arguments provided to polymorphic type"
                                     (unparse-type rator)))
                      (instantiate-poly rator rands))
      ;PolyDots NYI
      :else (throw (Exception. (str (when *current-env*
                                      (str (:line *current-env*) ": "))
                                 "Cannot apply non-polymorphic type " (unparse-type rator)))))))

(declare-type App)

(declare resolve-name* resolve-Name)

(defn -resolve [ty]
  {:pre [(AnyType? ty)]}
  (cond 
    (Name? ty) (resolve-Name ty)
    (Mu? ty) (unfold ty)
    (App? ty) (resolve-App ty)
    :else ty))

(defn resolve-Name [nme]
  {:pre [(Name? nme)]}
  (resolve-name* (:id nme)))

(declare-type Name)

(defrecord Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)])

(declare instantiate substitute remove-scopes subtype? abstract)

;smart constructor
(defn Mu* [name body]
  (->Mu (abstract name body)))

;smart destructor
(defn Mu-body* [name t]
  {:pre [(Mu? t)
         (symbol? name)]}
  (instantiate name (:scope t)))

(defn unfold [t]
  {:pre [(Mu? t)]
   :post [(Type? %)]}
  (let [sym (gensym)
        body (Mu-body* sym t)]
    (substitute t sym body)))

(declare-type Mu)

(defrecord Value [val]
  "A Clojure value"
  [])

(defrecord AnyValue []
  "Any Value"
  [])

(def -val ->Value)

(declare-type Value)
(declare-type AnyValue)

(defrecord HeterogeneousMap [types]
  "A constant map, clojure.lang.IPersistentMap"
  [(map? types)
   (every? #(and (= 2 (count %))
                 (let [[k v] %]
                   (and (Value? k)
                        (Type? v))))
           types)])

(defn make-HMap [mandatory optional]
  (assert (= #{}
             (set/intersection (-> mandatory keys set)
                               (-> optional keys set))))
  (apply Un
         (for [ss (map #(into {} %) (comb/subsets optional))]
           (->HeterogeneousMap (merge mandatory ss)))))

(declare-type HeterogeneousMap)

(defrecord HeterogeneousVector [types]
  "A constant vector, clojure.lang.IPersistentVector"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousVector)

(defrecord HeterogeneousList [types]
  "A constant list, clojure.lang.IPersistentList"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousList)

(defrecord HeterogeneousSeq [types]
  "A constant seq, clojure.lang.ISeq"
  [(sequential? types)
   (seq types)
   (every? Type? types)])

(declare-type HeterogeneousSeq)

(defrecord PrimitiveArray [jtype input-type output-type]
  "A Java Primitive array"
  [(class? jtype)
   (Type? input-type)
   (Type? output-type)])

(declare-type PrimitiveArray)

(declare Result?)

(defrecord DottedPretype [pre-type name]
  "A dotted pre-type. Not a type"
  [(Type? pre-type)
   ((some-fn symbol? nat?) name)])

(defrecord KwArgs [mandatory optional]
  "A set of mandatory and optional keywords"
  [(map? mandatory)
   (map? optional)
   (every? Value? (map keys [mandatory optional]))
   (every? Type? (map vals [mandatory optional]))])

(defrecord Function [dom rng rest drest kws]
  "A function arity, must be part of an intersection"
  [(or (nil? dom)
       (sequential? dom))
   (every? Type? dom)
   (Result? rng)
   (<= (count (filter identity [rest drest kws])) 1)
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (DottedPretype? drest))
   (or (nil? kws)
       (KwArgs? kws))])

(defrecord TopFunction []
  "Supertype to all functions"
  [])

(defrecord CountRange [lower upper]
  "A sequence of count between lower and upper.
  If upper is nil, between lower and infinity."
  [(nat? lower)
   (or (nil? upper)
       (nat? upper))])

(defrecord GTRange [n]
  "The type of all numbers greater than n"
  [(number? n)])

(defrecord LTRange [n]
  "The type of all numbers less than n"
  [(number? n)])

(declare-type CountRange)
(declare-type GTRange)
(declare-type LTRange)

(defn make-CountRange
  ([lower] (make-CountRange lower nil))
  ([lower upper] (->CountRange lower upper)))

(defn make-ExactCountRange [c]
  {:pre [(nat? c)]}
  (make-CountRange c c))

(declare ->NoFilter ->NoObject ->Result -FS -top)

(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil))
  ([t f] (make-Result t f nil))
  ([t f o] (->Result t (or f (-FS -top -top)) (or o (->NoObject)))))

(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to NoFilter
  and NoObject"
  ([dom rng] (make-Function dom rng nil nil))
  ([dom rng rest] (make-Function dom rng rest nil))
  ([dom rng rest drest & {:keys [filter object kws] :or {filter (-FS -top -top), object (->NoObject)}}]
   (->Function dom (->Result rng filter object) rest drest kws)))

(defn Fn-Intersection [& fns]
  {:pre [(every? Function? fns)]}
  (->Intersection fns))

(defn Fn-Intersection? [fin]
  (and (Intersection? fin)
       (sequential? (:types fin))
       (every? Function? (:types fin))))

(defrecord NotType [type]
  "A type that does not include type"
  [(Type? type)])

(declare-type NotType)

(defrecord ListDots [pre-type bound]
  "A dotted list"
  [(Type? pre-type)
   ((some-fn F? B?) bound)])

(declare-type ListDots)

(declare abstract)

(declare Filter? RObject?)

(defrecord Result [t fl o]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (Filter? fl)
   (RObject? o)])

(defn Result-type* [r]
  {:pre [(Result? r)]
   :post [(Type? %)]}
  (:t r))

(defn Result-filter* [r]
  {:pre [(Result? r)]
   :post [(Filter? %)]}
  (:fl r))

(defn Result-object* [r]
  {:pre [(Result? r)]
   :post [(RObject? %)]}
  (:o r))

(def no-bounds (->Bounds (->Top) (Un)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Folding

(def fold-rhs-default ::fold-rhs)

;1. fold-rhs calls sends
; a. Type to type-rec
; b. Filter to filter-rec
; c. Object to object-rec

(declare unparse-type)

;visit a type nested inside ty. Add methods with a mode deriving ::visit-type-default 
(defmulti fold-rhs (fn [mode options ty]
                     [mode (class ty)]))

; fld-fn has type-rec, filter-rec and object-rec in scope
(defmacro add-fold-case [mode ty fld-fn]
  `(defmethod fold-rhs [~mode ~ty]
     [mode# options# ty#]
     (let [~'[type-rec filter-rec object-rec pathelem-rec]
           (map #(or (% options#)
                     (partial fold-rhs mode# options#))
                [:type-rec :filter-rec :object-rec :pathelem-rec])]
       (~fld-fn ty# options#))))

(defmacro add-default-fold-case [ty fld-fn]
  `(add-fold-case fold-rhs-default ~ty ~fld-fn))

(declare sub-pe)

(defn sub-f [st mode]
  #(fold-rhs mode
             {:type-rec st
              :filter-rec (sub-f st mode)
              :pathelem-rec (sub-pe st mode)}
             %))

(defn sub-o [st mode]
  #(fold-rhs mode
             {:type-rec st
              :object-rec (sub-o st mode)
              :pathelem-rec (sub-pe st mode)}
             %))

(defn sub-pe [st mode]
  #(fold-rhs fold-rhs-default
             {:type-rec st
              :pathelem-rec (sub-pe st mode)}
             %))

(add-default-fold-case NotType
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec))))

(add-default-fold-case Intersection
                       (fn [ty _]
                         (apply In (mapv type-rec (:types ty)))))

(add-default-fold-case Union 
                       (fn [ty _]
                         (apply Un (mapv type-rec (:types ty)))))

(add-default-fold-case Function
                       (fn [ty _]
                         (-> ty
                           (update-in [:dom] #(map type-rec %))
                           (update-in [:rng] type-rec)
                           (update-in [:rest] #(when %
                                                 (type-rec %)))
                           (update-in [:drest] #(when %
                                                  (-> %
                                                    (update-in [:pre-type] type-rec)))))))

(add-default-fold-case RClass 
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:replacements] #(into {} (for [[k v] %]
                                                                  [k (type-rec v)]))))))

(add-default-fold-case App
                       (fn [ty _]
                         (-> ty
                           (update-in [:rator] type-rec)
                           (update-in [:rands] #(mapv type-rec %)))))

(add-default-fold-case PrimitiveArray
                       (fn [ty _]
                         (-> ty
                           (update-in [:input-type] type-rec)
                           (update-in [:output-type] type-rec))))

(add-default-fold-case DataType
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:fields] (fn [fs]
                                                  (apply array-map
                                                         (apply concat
                                                                (for [[k v] fs]
                                                                  [k (type-rec v)]))))))))

(add-default-fold-case Protocol
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:methods] (fn [ms]
                                                   (into {}
                                                         (for [[k v] ms]
                                                           [k (type-rec v)])))))))

(add-default-fold-case Poly
                       (fn [ty _]
                         (let [names (repeatedly (:nbound ty) gensym)
                               body (Poly-body* names ty)
                               bbnds (Poly-bbnds* names ty)]
                           (Poly* names 
                                  (doall 
                                    (for [bnd bbnds]
                                      (-> bnd
                                        (update-in [:upper-bound] type-rec)
                                        (update-in [:lower-bound] type-rec))))
                                  (type-rec body)))))

(add-default-fold-case PolyDots
                       (fn [ty _]
                         (let [names (repeatedly (:nbound ty) gensym)
                               body (PolyDots-body* names ty)
                               bbnds (PolyDots-bbnds* names ty)]
                           (PolyDots* names 
                                      (doall
                                        (for [bnd bbnds]
                                          (-> bnd
                                            (update-in [:upper-bound] type-rec)
                                            (update-in [:lower-bound] type-rec))))
                                      (type-rec body)))))

(add-default-fold-case Mu
                       (fn [ty _]
                         (let [name (gensym)
                               body (Mu-body* name ty)]
                           (Mu* name (type-rec body)))))

(add-default-fold-case HeterogeneousVector
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousList 
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousSeq
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousMap
                       (fn [ty _]
                         (-> ty 
                           (update-in [:types] #(into {} (for [[k v] %]
                                                           [(type-rec k) (type-rec v)]))))))

(def ret-first (fn [a & rest] a))

(add-default-fold-case CountRange ret-first)
(add-default-fold-case Name ret-first)
(add-default-fold-case Value ret-first)
(add-default-fold-case Top ret-first)
(add-default-fold-case TopFunction ret-first)
(add-default-fold-case B ret-first)
(add-default-fold-case F ret-first)

(add-default-fold-case Result 
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(def name-ref? (some-fn symbol? integer?))

(def Filter ::filter)

(defn Filter? [a]
  (isa? (class a) Filter))

(defn declare-filter [c]
  (derive c Filter))

(defrecord BotFilter []
  "?"
  [])
(defrecord TopFilter []
  "?"
  [])

(add-default-fold-case TopFilter ret-first)
(add-default-fold-case BotFilter ret-first)

(def -top (->TopFilter))
(def -bot (->BotFilter))

(declare unparse-path-elem)



(declare TypeFilter? NotTypeFilter? type-of TCResult? ret-t Nil? False? True?)

(def ^:dynamic *mutated-bindings* #{})

(defn is-var-mutated? [id]
  (contains? *mutated-bindings* id))

;true if types t1 and t2 overlap (NYI)
(defn overlap [t1 t2]
  (cond 
    (= t1 t2) true
    (and (Value? t1)
         (Value? t2)) (= t1 t2)
    (Value? t1) (subtype? t1 t2)
    (Value? t2) (subtype? t2 t1)
;    (and (Name? t1)
;         (Name? t2)) (overlap (-resolve t1) (-resolve t2))
;    (Name? t1) (overlap (-resolve t1) t2)
;    (Name? t2) (overlap t1 (-resolve t2))
    (and (HeterogeneousMap? t1)
         (HeterogeneousMap? t2)) (and (= (set (-> t1 :types keys))
                                         (set (-> t2 :types keys)))
                                      (every? true?
                                              (for [[k1 v1] (:types t1)]
                                                (let [v2 ((:types t2) k1)]
                                                  (overlap v1 v2)))))
    :else true)) ;FIXME conservative result

(declare infer subst-all)

; restrict t1 to be a subtype of t2
(defn restrict [t1 t2]
  (cond
    (subtype? t1 t2) t1 ;; already a subtype

    (Poly? t2)
    (let [names (repeatedly (:nbound t2) gensym)
          t (Poly-body* names t2)
          subst (infer names nil (list t1) (list t) t1)]
      (and subst (restrict t1 (subst-all subst t1))))

    (Union? t1) (apply Un (map (fn [e] (restrict e t2)) (:types t1)))
    (Union? t2) (apply Un (map (fn [e] (restrict t1 e)) (:types t2)))
    (not (overlap t1 t2)) (Un) ;there's no overlap, so the restriction is empty
    ;TODO other cases
    :else (In t2 t1)))

(declare PathElem? ->TypeFilter ->NotTypeFilter ->OrFilter ->AndFilter OrFilter?
         implied-atomic? subst-type)

(defn -filter [t i & [p]]
  {:pre [(Type? t)
         (name-ref? i)
         ((some-fn nil? #(every? PathElem? %)) p)]
   :post [(Filter? %)]}
  (if (or (= (->Top) t) (and (symbol? i) (is-var-mutated? i)))
    -top
    (->TypeFilter t p i)))

(defn -not-filter [t i & [p]]
  {:pre [(Type? t)
         (name-ref? i)
         ((some-fn nil? #(every? PathElem? %)) p)]
   :post [(Filter? %)]}
  (if (or (= (Bottom) t) (and (symbol? i) (is-var-mutated? i)))
    -top
    (->NotTypeFilter t p i)))

(declare Path?)

(defn -filter-at [t o]
  (if (Path? o)
    (let [{p :path i :id} o]
      (-filter t i p))
    -top))
(defn -not-filter-at [t o]
  (if (Path? o)
    (let [{p :path i :id} o]
      (-not-filter t i p))
    -top))


(defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  [])

;Filters

(add-default-fold-case NoFilter ret-first)

(declare PathElem?)

(defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(Type? type)
   (every? PathElem? path)
   (name-ref? id)])

(add-default-fold-case TypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (map pathelem-rec %))))))

(defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(Type? type)
   (every? PathElem? path)
   (name-ref? id)])

(add-default-fold-case NotTypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (map pathelem-rec %))))))

(defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)])

(defn opposite? [f1 f2]
  {:pre [(Filter? f1)
         (Filter? f2)]
   :post [(boolean? %)]}
  (cond
    (and (TypeFilter? f1)
         (NotTypeFilter? f2))
    (let [{t1 :type p1 :path i1 :id} f1
          {t2 :type p2 :path i2 :id} f2]
      (and (= p1 p2)
           (= i1 i2)
           (subtype? t1 t2)))

    (and (NotTypeFilter? f1)
         (TypeFilter? f2))
    (let [{t2 :type p2 :path i2 :id} f1
          {t1 :type p1 :path i1 :id} f2]
      (and (= p1 p2)
           (= i1 i2)
           (subtype? t1 t2)))
    :else false))


;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrFilter (alternative is AndFilter)
(defn compact [props or?]
  {:pre [(every? Filter? props)
         (boolean? or?)]
   :post [(every? Filter? %)]}
  (let [tf-map (atom {})
        ntf-map (atom {})]
    ;; props: the propositions we're processing
    ;; others: props that are neither TF or NTF
    (loop [props props
           others nil]
      (if (empty? props)
        (concat others
                (vals @tf-map)
                (vals @ntf-map))
        (cond
          (and or? (TypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id :as p} (first props)]
            (swap! tf-map (fn [m] (update-in m [[f1 x]] #(if %
                                                           (if (TypeFilter? %)
                                                             (let [t2 (:type %)]
                                                               (-filter (Un t1 t2) x f1))
                                                             (throw (Exception. (str "got something that isn't a type filter" p))))
                                                           p))))
            (recur (rest props) others))

          (and (not or?) (TypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id} (first props)
                fl (@tf-map [f1 x])]
            (cond
              (and (TypeFilter? fl)
                   (let [t2 (:type fl)]
                     (not (overlap t1 (:type fl)))))
              ;; we're in an And, and we got two types for the same path that do not overlap
              [-bot]
              (TypeFilter? fl)
              (let [t2 (:type fl)]
                (swap! tf-map (fn [m] (assoc m [f1 x] (-filter (restrict t1 t2) x f1))))
                (recur (next props) others))
              :else
              (do 
                (swap! tf-map (fn [m] (assoc m [f1 x] (-filter t1 x f1))))
                (recur (next props) others))))

          (and (not or?) 
               (NotTypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id :as p} (first props)]
            (swap! ntf-map (fn [m] (update-in m [[f1 x]]
                                              (fn [n]
                                                (if n
                                                  (if (NotTypeFilter? n)
                                                    (let [t2 (:type n)]
                                                      (-not-filter (Un t1 t2) x f1))
                                                    (throw (Exception. (str "got something that isn't a nottypefilter" p))))
                                                  p)))))
            (recur (next props) others))
          :else
          (let [p (first props)]
            (recur (next props) (cons p others))))))))


(declare -and)

(defn inverse-atom [a]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) a)]
   :post [((some-fn TypeFilter? NotTypeFilter?) a)]}
  (cond
    (TypeFilter? a) (-not-filter (:type a) (:id a) (:path a))
    (NotTypeFilter? a) (-filter (:type a) (:id a) (:path a))))

(defn simplify-prop 
  "Try and use atomic proposition a to simplify composite
  proposition b. a must be correct polarity."
  [a b]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) a)
         ((some-fn AndFilter? OrFilter?) b)]
   :post [(Filter? %)]}
  (cond
    ; assuming a wrapping OrFilter
    (AndFilter? b)
    (let [fs (set (:fs b))
          fs (set
               (for [f fs]
                 (cond
                   ; A ^ (B v A) => A
                   (OrFilter? f) (simplify-prop a f)
                   :else f)))]
      (if (fs a)
        ; A v (notB ^ A) => A v notB
        (apply -and (disj fs a))
        b))

    ; assuming a wrapping AndFilter
    (OrFilter? b)
    (let [fs (set (:fs b))]
      ; A ^ (B v A) => A
      (if (fs a)
        a
        b))))


(comment
  (-or (-not-filter -nil 'a)
       (-and (-filter -nil 'a)
             (-filter -false 'b)))
  (simplify-prop (-filter -nil 'a) (-and (-filter -nil 'a)
                                         (-filter -false 'b)))
  ;=> (-filter -nil 'a)
  '[-or-filter
    [-not-filter (Value :Black) (:tree) 0]
    [-and-filter
     ; or->and, elim -filter (:Black) (:tree 0)
     [-filter (Value :Black) (:tree) 0]
     [-or-filter
      ;and->or,  elim -filter (:Black) (:tree 0)
      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Black) (:tree) 0]
       [-not-filter (Value :Red) (:left :tree) 0]]

      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Red) (:left :tree) 0]
       [-filter (Value :Black) (:tree) 0]
       [-or-filter
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-not-filter (Value :Red) (:right :tree) 0]]
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-filter (Value :Red) (:right :tree) 0]
         [-not-filter (Value :Red) (:right :left :tree) 0]]]]]
     ]
    ]
  )

(declare atomic-filter?)

(defn -or [& args]
           ; flatten internal OrFilters
  (let [fs (-> (apply concat
                      (for [a (set args)]
                        (if (OrFilter? a)
                          (:fs a)
                          [a])))
             set (disj -bot))]
    (cond
      (empty? fs) -bot
      (fs -top) -top
      (= 1 (count fs)) (first fs)
      :else (->OrFilter fs))))

;(defn -or [& args]
;  {:pre [(every? Filter? args)]
;   :post [(Filter? %)]}
;  (letfn [(mk [& fs]
;            {:pre [(every? Filter? fs)]
;             :post [(Filter? %)]}
;            (cond
;              (empty? fs) -bot
;              (= 1 (count fs)) (first fs)
;              :else (->OrFilter fs)))
;          (distribute [args]
;            (let [{ands true others false} (group-by AndFilter? args)]
;              (if (empty? ands)
;                (apply mk others)
;                (let [{elems :fs} (first ands)] ;an AndFilter
;                  (apply -and (for [a elems]
;                                (apply -or a (concat (next ands) others))))))))]
;    (loop [fs args
;           result nil]
;      (assert (every? Filter? fs))
;      (assert (every? Filter? result))
;      (if (empty? fs)
;        (cond
;          (empty? result) -bot
;          (= 1 (count result)) (first result)
;          :else (distribute (compact result true)))
;        (cond
;          (Top? (first fs)) (first fs)
;          (OrFilter? (first fs)) (let [fs* (:fs (first fs))]
;                                   (recur (concat fs* (next fs)) result))
;          (BotFilter? (first fs)) (recur (next fs) result)
;          :else (let [t (first fs)]
;                  (assert (Filter? t))
;                  (cond 
;                    (some (fn [f] (opposite? f t)) (concat (rest fs) result))
;                    -top
;                    (some (fn [f] (or (= f t)
;                                      (implied-atomic? f t)))
;                          result)
;                    (recur (next fs) result)
;                    :else
;                    (recur (next fs) (cons t result)))))))))

(declare atomic-filter? combine-props ->ImpFilter)

(defn -imp [a c]
  {:pre [(Filter? a)
         (Filter? c)]
   :post [(Filter? %)]}
  (cond
    (BotFilter? a) -top
    (TopFilter? a) c
    :else (->ImpFilter a c)))



;  A ^ (B v ...) -> (simplify A (B v ...))
(defn -and [& args]
             ;flatten direct internal AndFilters
  (let [flat (apply concat
                    (for [fl args]
                      (if (AndFilter? fl)
                        (:fs fl)
                        [fl])))
        fs (set flat)]
    (cond
      (empty? fs) -bot
      (fs -bot) -bot
      (or (= 1 (count fs))
          (= 1 (count (disj fs -top)))) (or (first (disj fs -top))
                                            (first fs))
      :else (->AndFilter (disj fs -top)))))

;(defn -and [& args]
;  {:pre [(every? Filter? args)]
;   :post [(Filter? %)]}
;  (letfn [(mk [& fs]
;            {:pre [(every? Filter? fs)]
;             :post [(Filter? %)]}
;            (cond
;              (empty? fs) -top
;              (= 1 (count fs)) (first fs)
;              :else (->AndFilter fs)))]
;    (loop [fs (set args)
;           result nil]
;      (if (empty? fs)
;        (cond
;          (empty? result) -top
;          (= 1 (count result)) (first result)
;          ;; don't think this is useful here
;          (= 2 (count result)) (let [[f1 f2] result]
;                                 (if (opposite? f1 f2)
;                                   -bot
;                                   (if (= f1 f2)
;                                     f1
;                                     (apply mk (compact [f1 f2] false)))))
;          :else
;           ;; first, remove anything implied by the atomic propositions
;           ;; We commonly see: (And (Or P Q) (Or P R) (Or P S) ... P), which this fixes
;          (let [{atomic true not-atomic false} (group-by atomic-filter? result)
;                not-atomic* (for [p not-atomic
;                                  :when (some (fn [a] (implied-atomic? p a)) atomic)]
;                              p)]
;             ;; `compact' takes care of implications between atomic props
;            (apply mk (compact (concat not-atomic* atomic) false))))
;        (let [ffs (first fs)]
;          (cond
;            (BotFilter? ffs) ffs
;            (AndFilter? ffs) (let [fs* (:fs ffs)]
;                               (recur (next fs) (concat fs* result)))
;            (TopFilter? ffs) (recur (next fs) result)
;            :else (let [t ffs]
;                    (cond
;                      (some (fn [f] (opposite? f ffs)) (concat (rest fs) result)) 
;                      -bot
;                      (some (fn [f] (or (= f t)
;                                        (implied-atomic? t f))) result) 
;                      (recur (rest fs) result)
;                      :else
;                      (recur (rest fs) (cons t result))))))))))

(defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)])

(defrecord ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)])

(defmulti unparse-filter* class)

(declare FilterSet? unparse-filter)

(defn unparse-filter-set [{:keys [then else] :as fs}]
  {:pre [(FilterSet? fs)]}
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(defn unparse-filter [f]
  (unparse-filter* f))

(defmethod unparse-filter* TopFilter [f] ['top-filter])
(defmethod unparse-filter* BotFilter [f] ['bot-filter])

(declare unparse-type)

(defmethod unparse-filter* TypeFilter
  [{:keys [type path id]}]
  ['-filter (unparse-type type) (map unparse-path-elem path)
   id])

(defmethod unparse-filter* NotTypeFilter
  [{:keys [type path id]}]
  ['-not-filter (unparse-type type) (map unparse-path-elem path)
   id])

(defmethod unparse-filter* AndFilter [{:keys [fs]}] (apply vector '-and-filter (map unparse-filter fs)))
(defmethod unparse-filter* OrFilter [{:keys [fs]}] (apply vector '-or-filter (map unparse-filter fs)))

(defmethod unparse-filter* ImpFilter
  [{:keys [a c]}]
  ['-imp-filter (unparse-filter a) '-> (unparse-filter c)])

(add-default-fold-case ImpFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:a] filter-rec)
                           (update-in [:c] filter-rec))))

(add-default-fold-case AndFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:fs] #(set (map filter-rec %))))))

(add-default-fold-case OrFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:fs] #(set (map filter-rec %))))))

(defrecord FilterSet [then else]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(and (or (BotFilter? then)
            (and (BotFilter? else)
               (TopFilter? then))
            (Filter? then))
        (or (BotFilter? else)
            (and (BotFilter? then)
                 (TopFilter? else))
            (Filter? else)))])

(add-default-fold-case FilterSet
                       (fn [ty _]
                         (-> ty
                           (update-in [:then] filter-rec)
                           (update-in [:else] filter-rec))))

(defn -FS [+ -]
  {:pre [(Filter? +)
         (Filter? -)]
   :post [(FilterSet? %)]}
  (cond
    (BotFilter? +) (->FilterSet -bot -top)
    (BotFilter? -) (->FilterSet -top -bot)
    :else (->FilterSet + -)))

(declare-filter BotFilter)
(declare-filter TopFilter)
(declare-filter NoFilter)
(declare-filter AndFilter)
(declare-filter OrFilter)
(declare-filter TypeFilter)
(declare-filter NotTypeFilter)
(declare-filter ImpFilter)
(declare-filter FilterSet)

(def atomic-filter? (some-fn TypeFilter? NotTypeFilter?
                             TopFilter? BotFilter?))

(def -true-filter (-FS -top -bot))
(def -false-filter (-FS -bot -top))

(def -false (->Value false))
(def -true (->Value true))
(def -nil (->Value nil))

(defn Nil? [a] (= -nil a))
(defn False? [a] (= -false a))
(defn True? [a] (= -true a))

(defn implied-atomic? [f1 f2]
  (if (= f1 f2)
    true
    (cond
      (OrFilter? f1) (boolean (some #(= % f2) (:fs f1)))
      (and (TypeFilter? f1)
           (TypeFilter? f2)) (and (= (:id f1) (:id f2))
                                  (subtype? (:type f2) (:type f1)))
      (and (NotTypeFilter? f1)
           (NotTypeFilter? f2)) (and (= (:id f1) (:id f2))
                                     (subtype? (:type f1) (:type f2)))
      :else false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(def PathElem ::path-elem)

(defn PathElem? [a]
  (isa? (class a) PathElem))

(defn declare-path-elem [c]
  (derive c PathElem))

(defrecord FirstPE []
  "A path calling clojure.core/first"
  [])
(defrecord NextPE []
  "A path calling clojure.core/next"
  [])

(defrecord ClassPE []
  "A path calling clojure.core/class"
  [])

(defrecord CountPE []
  "A path calling clojure.core/count"
  [])

(defrecord KeyPE [val]
  "A key in a hash-map"
  [((some-fn keyword?) val)])

(declare-path-elem FirstPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)

(defmulti unparse-path-elem class)
(defmethod unparse-path-elem KeyPE [t] (:val t))
(defmethod unparse-path-elem CountPE [t] 'CountPE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(def RObject ::r-object)

(defn RObject? [a]
  (isa? (class a) RObject))

(defn declare-robject [c]
  (derive c RObject))

(defrecord EmptyObject []
  "?"
  [])

(def -empty (->EmptyObject))

(defrecord Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first."
  [(or (and (seq path)
            (sequential? path))
       (nil? path))
   (every? PathElem? path)
   (name-ref? id)])

(defrecord NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  [])

;Objects

(declare unparse-path-elem)

(defmulti unparse-object class)
(defmethod unparse-object EmptyObject [_] 'empty-object)
(defmethod unparse-object NoObject [_] 'no-object)
(defmethod unparse-object Path [{:keys [path id]}] [(map unparse-path-elem path) id])

(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty _]
                         (-> ty
                           (update-in [:path] #(doall (map pathelem-rec %))))))
(add-default-fold-case NoObject ret-first)

(declare-robject EmptyObject)
(declare-robject Path)
(declare-robject NoObject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(declare TCResult?)

(defrecord PropEnv [l props]
  "A lexical environment l, props is a list of known propositions"
  [(every? (every-pred symbol? (complement namespace)) (keys l))
   (every? Type? (vals l))
   (every? Filter? props)])

(declare ^:dynamic *lexical-env*)

(defn print-env 
  ([] (print-env *lexical-env*))
  ([e]
   {:pre [(PropEnv? e)]}
   (prn {:env (into {} (for [[k v] (:l e)]
                         [k (unparse-type v)]))
         :props (map unparse-filter (:props e))})))

(defonce VAR-ANNOTATIONS (atom {}))
(def ^:dynamic *lexical-env* (->PropEnv {} []))

(defmacro with-lexical-env [env & body]
  `(binding [*lexical-env* ~env]
     ~@body))

(set-validator! VAR-ANNOTATIONS #(and (every? (every-pred symbol? namespace) (keys %))
                                      (every? Type? (vals %))))
(set-validator! #'*lexical-env* PropEnv?)

(defmacro ann [varsym typesyn]
  `(tc-ignore
  (let [t# (parse-type '~typesyn)
        s# (if (namespace '~varsym)
             '~varsym
             (symbol (-> *ns* ns-name str) (str '~varsym)))]
    (do (add-var-type s# t#)
      [s# (unparse-type t#)]))))

(declare parse-type alter-class*)

(defn parse-field [[n _ t]]
  [n (parse-type t)])

(defn gen-datatype* [local-name fields variances args ancests]
    `(let [local-name# '~local-name
           fs# (apply array-map (apply concat (with-frees (mapv make-F '~args)
                                                (mapv parse-field '~fields))))
           _# (prn '~ancests)
           as# (set (with-frees (mapv make-F '~args)
                      (mapv parse-type '~ancests)))
           s# (symbol (str (munge (-> *ns* ns-name)) \. local-name#))
           _# (add-datatype-ancestors s# as#)
           pos-ctor-name# (symbol (str (-> *ns* ns-name)) (str "->" local-name#))
           args# '~args
           vs# '~variances
           dt# (if args#
                 (with-meta (Poly* args# (repeat (count args#) no-bounds)
                                   (->DataType s# vs# (map make-F args#) fs#))
                            {:actual-frees args#})
                 (->DataType s# nil nil fs#))
           pos-ctor# (if args#
                       (with-meta (Poly* args# (repeat (count args#) no-bounds)
                                         (Fn-Intersection
                                           (make-Function (vec (vals fs#)) (->DataType s# vs# (map make-F args#) fs#))))
                                  {:actual-frees args#})
                       (Fn-Intersection
                         (make-Function (vec (vals fs#)) dt#)))]
       (do 
         (when vs#
           (let [f# (mapv make-F (repeatedly (count vs#) gensym))]
             (alter-class* s# (RClass* (map :name f#) vs# f# s# {}))))
         (add-datatype s# dt#)
         (add-var-type pos-ctor-name# pos-ctor#)
         [[s# (unparse-type dt#)]
          [pos-ctor-name# (unparse-type pos-ctor#)]])))

(defmacro ann-datatype [local-name fields & {ancests :unchecked-ancestors rplc :replace}]
  (assert (not rplc) "Replace NYI")
  (assert (not (or (namespace local-name)
                   (some #(= \. %) (str local-name))))
          (str "Must provide local name: " local-name))
  `(tc-ignore
     ~(gen-datatype* local-name fields nil nil ancests)))

(defmacro ann-pdatatype [local-name vbnd fields & {ancests :unchecked-ancestors rplc :replace}]
  (assert (not rplc) "Replace NYI")
  (assert (not (or (namespace local-name)
                   (some #(= \. %) (str local-name))))
          (str "Must provide local name: " local-name))
  `(tc-ignore
     ~(gen-datatype* local-name fields (map second vbnd) (map first vbnd) ancests)))

(defn gen-protocol* [local-varsym variances args mths]
  `(let [local-vsym# '~local-varsym
         s# (symbol (-> *ns* ns-name str) (str local-vsym#))
         on-class# (symbol (str (munge (namespace s#)) \. local-vsym#))
         ; add a Name so the methods can be parsed
         _# (declare-protocol* s#)
         args# '~args
         fs# (when args# 
               (map make-F args#))
         ms# (into {} (for [[knq# v#] '~mths]
                        (do
                          (assert (not (namespace knq#))
                                  "Protocol method should be unqualified")
                          [knq# (with-frees fs# (parse-type v#))])))
         _# (prn "here")
         t# (if fs#
              (do
                (prn (map :name fs#))
                (prn (repeat (count fs#) no-bounds))
                (prn (->Protocol s# '~variances fs# on-class# ms#))
                (prn "poly" (Poly* (map :name fs#) (repeat (count fs#) no-bounds) 
                                   (->Protocol s# '~variances fs# on-class# ms#)))
                (Poly* (map :name fs#) (repeat (count fs#) no-bounds) 
                     (->Protocol s# '~variances fs# on-class# ms#))
                )
              (->Protocol s# nil nil on-class# ms#))]
     (do
       (prn "after")
       (add-protocol s# t#)
       (doseq [[kuq# mt#] ms#]
         ;qualify method names when adding methods as vars
         (let [kq# (symbol (-> *ns* ns-name str) (str kuq#))]
           (add-var-type kq# mt#)))
       [s# (unparse-type t#)])))

(defmacro ann-protocol [local-varsym & {mths :methods}]
  (assert (not (or (namespace local-varsym)
                   (some #{\.} (str local-varsym))))
          (str "Must provide local var name for protocol: " local-varsym))
  `(tc-ignore
     ~(gen-protocol* local-varsym nil nil mths)))

(defmacro ann-pprotocol [local-varsym vbnd & {mths :methods}]
  (assert (not (or (namespace local-varsym)
                   (some #{\.} (str local-varsym))))
          (str "Must provide local var name for protocol: " local-varsym))
  `(tc-ignore
     ~(gen-protocol* local-varsym (mapv second vbnd) (mapv first vbnd) mths)))

(defmacro override-method [methodsym typesyn]
  `(tc-ignore
  (let [t# (parse-type '~typesyn)
        s# (if (namespace '~methodsym)
             '~methodsym
             (throw (Exception. "Method name must be a qualified symbol")))]
    (do (add-method-override s# t#)
      [s# (unparse-type t#)]))))

(defn add-var-type [sym type]
  (swap! VAR-ANNOTATIONS #(assoc % sym type))
  nil)

(defn lookup-local [sym]
  (-> *lexical-env* :l sym))

(defn var->symbol [var]
  {:pre [(var? var)]
   :post [(symbol? %)
          (namespace %)]}
  (symbol (str (ns-name (.ns ^Var var)))
          (str (.sym ^Var var))))

(defn symbol->Class [sym]
  {:pre [(symbol? sym)]
   :post [(class? %)]}
  (case sym
    long Long/TYPE
    int Integer/TYPE
    boolean Boolean/TYPE
    (Class/forName (str sym))))

(defn Class->symbol [cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

(defn lookup-Var [nsym]
  (assert (contains? @VAR-ANNOTATIONS nsym) 
          (str (when *current-env*
                 (str (:line *current-env*) ": "))
            "Untyped var reference: " nsym))
  (@VAR-ANNOTATIONS nsym))

(defn merge-locals [env new]
  (-> env
    (update-in [:l] #(merge % new))))

(defmacro with-locals [locals & body]
  `(binding [*lexical-env* (merge-locals *lexical-env* ~locals)]
     ~@body))

(declare ^:dynamic *current-env*)

(defn type-of [sym]
  {:pre [(symbol? sym)]
   :post [(or (Type? %)
              (TCResult? %))]}
  (cond
    (not (namespace sym)) (if-let [t (lookup-local sym)]
                            t
                            (throw (Exception. (str (when *current-env*
                                                      (str (:line *current-env*) ": "))
                                                    "Reference to untyped binding: " sym))))
    :else (lookup-Var sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted Variable Environment

;symbol -> F
(def ^:dynamic *dotted-scope* {})
(set-validator! #'*dotted-scope* #(or (fn? %)
                                      (and 
                                        (every? symbol? (keys %))
                                        (every? F? (vals %)))))

(defn bound-index? [n]
  (contains? *dotted-scope* n))

(defmacro with-dotted [dvar & body]
  `(binding [*dotted-scope* (conj *dotted-scope* [(:name ~dvar) ~dvar])]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(defonce DATATYPE-ENV (atom {}))
(set-validator! DATATYPE-ENV (hash-c? (every-pred symbol? 
                                                  (fn [k] (some #(= \. %) (str k)))) 
                                      Type?))

(defn add-datatype [sym t]
  (swap! DATATYPE-ENV assoc sym t)
  nil)

(defn resolve-datatype [sym]
  (let [d (@DATATYPE-ENV sym)]
    (assert d (str "Could not resolve DataType: " sym))
    d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DataType Ancestor Env

(defonce DATATYPE-ANCESTOR-ENV (atom {}))
(set-validator! DATATYPE-ANCESTOR-ENV (hash-c? (every-pred symbol? #(some #{\.} (str %)))
                                               (set-c? Type?)))

(defn add-datatype-ancestors [sym tset]
  (swap! DATATYPE-ANCESTOR-ENV update-in [sym] #(set/union (or % #{}) tset))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(defonce PROTOCOL-ENV (atom {}))
(set-validator! PROTOCOL-ENV (hash-c? (every-pred symbol? namespace) Type?))

(defn add-protocol [sym t]
  (swap! PROTOCOL-ENV assoc sym t)
  nil)

(defn resolve-protocol [sym]
  (let [p (@PROTOCOL-ENV sym)]
    (assert p (str "Could not resolve Protocol: " sym))
    (assert (not (Poly? p)) (str "Protocol " sym " takes mandatory arguments, none provided"))
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Override Env

(defonce METHOD-OVERRIDE-ENV (atom {}))
(set-validator! METHOD-OVERRIDE-ENV #(and (every? (every-pred namespace symbol?) (keys %))
                                          (every? Fn-Intersection? (vals %))))

(defn add-method-override [sym t]
  (swap! METHOD-OVERRIDE-ENV assoc sym t)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Return non-nilables

(defonce METHOD-RETURN-NONNILABLE-ENV (atom {}))
(set-validator! METHOD-RETURN-NONNILABLE-ENV (hash-c? (every-pred namespace symbol?)
                                                      (some-fn #(= :all %)
                                                               (set-c? nat?))))

(defn add-nonnilable-method-return [sym m]
  (swap! METHOD-RETURN-NONNILABLE-ENV assoc sym m)
  nil)

(defn nonnilable-return? [sym arity]
  (let [as (@METHOD-RETURN-NONNILABLE-ENV sym)]
    (boolean (when as
               (as arity)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defonce METHOD-PARAM-NILABLE-ENV (atom {}))
(set-validator! METHOD-PARAM-NILABLE-ENV (hash-c? (every-pred namespace symbol?)
                                                  (hash-c? (some-fn #(= :all %) nat?)
                                                           (some-fn #(= :all %) (set-c? nat?)))))

(defn add-method-nilable-param [sym a]
  (swap! METHOD-PARAM-NILABLE-ENV assoc sym a)
  nil)

(defn nilable-param? [sym arity param]
  (boolean 
    (when-let [nilables (@METHOD-PARAM-NILABLE-ENV sym)]
      (when-let [params (or (nilables :all)
                            (nilables arity))]
        (or (= :all params)
            (params param))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(def declared-name-type ::declared-name)
(def protocol-name-type ::protocol-name)
(def datatype-name-type ::datatype-name)

(def temp-binding ::temp-binding)

(doseq [k [declared-name-type protocol-name-type datatype-name-type]]
  (derive k temp-binding))

(defonce TYPE-NAME-ENV (atom {}))
(set-validator! TYPE-NAME-ENV #(and (every? (every-pred (some-fn namespace 
                                                                 (fn [k] (some (fn [a] (= \. a)) (str k))))
                                                        symbol?) 
                                            (keys %))
                                    (every? (some-fn Type? (fn [a] (isa? a temp-binding))) 
                                            (vals %))))

(defn add-type-name [sym ty]
  (swap! TYPE-NAME-ENV assoc sym ty)
  nil)

(defn declare-name* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-type-name sym declared-name-type)
  nil)

(defn declare-protocol* [sym]
  {:pre [(symbol? sym)
         (some #(= \. %) (str sym))]}
  (add-type-name sym protocol-name-type)
  nil)

(defn declare-datatype* [sym]
  (add-type-name sym datatype-name-type)
  nil)

(defn- resolve-name* [sym]
  (let [t (@TYPE-NAME-ENV sym)]
    (cond
      (= protocol-name-type t) (resolve-protocol sym)
      (= datatype-name-type t) (resolve-datatype sym)
      (= declared-name-type t) (throw (Exception. (str "Reference to declared but undefined name " sym)))
      (Type? t) (with-meta t {:source-Name sym})
      :else (throw (Exception. (str "Cannot resolve name " sym))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

;Class -> RClass
(defonce RESTRICTED-CLASS (atom {}))
(set-validator! RESTRICTED-CLASS (hash-c? symbol? Type?))

(declare with-frees)

(defn- build-replacement-syntax [m]
  `(into {} (for [[k# v#] '~m]
              [(if-let [c# (resolve k#)] 
                 (and (class? c#) (Class->symbol c#))
                 k#)
               (parse-type v#)])))

(defn parse-RClass-binder [bnds]
  (for [[nme & {:keys [variance]}] bnds]
    [variance (make-F nme)]))

(defn alter-class* [csym type]
  (swap! RESTRICTED-CLASS assoc csym type))

(defmacro alter-class [the-class frees-syn & opts]
  (let [{replacements-syn :replace} (apply hash-map opts)]
     `(let [[variances# frees#] (when-let [fs# (seq '~frees-syn)]
                                  (let [b# (parse-RClass-binder fs#)]
                                    [(map first b#) (map second b#)]))
            csym# (let [cls# (when-let [c# (resolve '~the-class)]
                               (when (class? c#)
                                 c#))]
                    (or (and cls# (Class->symbol cls#))
                        '~the-class))]
        (alter-class* csym# (RClass* (map :name frees#) variances# frees# csym#
                                     (with-frees frees#
                                       ~(build-replacement-syntax replacements-syn))))
        ~the-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type syntax

;(Map Symbol F)
(def ^:dynamic *free-scope* {})
(set-validator! #'*free-scope* #(or (fn? %)
                                    (and (every? symbol? (keys %))
                                         (every? F? (vals %)))))

(defn bound-tvar? [name]
  (contains? *free-scope* name))

(defmacro with-frees [frees & body]
  `(let [m# (zipmap (map :name ~frees) ~frees)]
     (binding [*free-scope* (merge *free-scope* m#)]
       ~@body)))

(defmulti parse-type class)
(defmulti parse-type-list first)

;return a vector of [name bnds]
(defn parse-free [f]
  {:post [(hvector-c? symbol? Bounds?)]}
  (if (symbol? f)
    [f no-bounds]
    (let [[n & opts] f
          {upp :<
           low :>} (apply hash-map opts)]
      [n (->Bounds
           (if upp 
             (parse-type upp)
             (->Top)) 
           (if low
             (parse-type low)
             (Bottom)))])))

(defn check-forbidden-rec [rec tbody]
  (when (or (= rec tbody) 
            (and (Intersection? tbody)
                 (contains? (set (:types tbody)) rec))
            (and (Union? tbody)
                 (contains? (set (:types tbody)) rec)))
    (throw (Exception. "Recursive type not allowed here"))))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [_ (assert (= 1 (count bnder)) "Only one variable in allowed: Rec")
        f (make-F free-symbol)
        body (with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

(defmethod parse-type-list 'CountRange
  [[_ n]]
  (make-CountRange n))

(defmethod parse-type-list 'ExactCount
  [[_ n]]
  (make-ExactCountRange n))

(defmethod parse-type-list 'predicate
  [[_ t-syn]]
  (let [on-type (parse-type t-syn)]
    (Fn-Intersection
      (make-Function [-any] (RClass-of 'boolean nil) nil nil
                     :filter (-FS (-filter on-type 0)
                                  (-not-filter on-type 0))))))

(defmethod parse-type-list 'Rec
  [syn]
  (parse-rec-type syn))

;dispatch on last element of syntax in binder
(defmulti parse-all-type (fn [bnds type] (last bnds)))

;(All [a b ...] type)
(defmethod parse-all-type '...
  [bnds type]
  (let [frees-with-bnds (reduce (fn [fs fsyn]
                                  {:pre [(vector? fs)]
                                   :post [(every? (hvector-c? symbol? Bounds?) %)]}
                                  (conj fs
                                        (with-frees (map (comp make-F first) fs)
                                          (parse-free fsyn))))
                                [] (-> bnds butlast butlast))
        dvar (parse-free (-> bnds butlast last))]
    (-> 
      (PolyDots* (map first (concat frees-with-bnds [dvar]))
                 (map second (concat frees-with-bnds [dvar]))
                 (with-frees (map (comp make-F first) frees-with-bnds)
                   (with-dotted (make-F (first dvar))
                     (parse-type type))))
      (with-meta {:actual-frees (map first frees-with-bnds)
                  :dvar-name (first dvar)}))))

;(All [a b] type)
(defmethod parse-all-type :default
  [bnds type]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (hvector-c? symbol? Bounds?) %)]}
                  (conj fs
                        (with-frees (map (comp make-F first) fs)
                          (parse-free fsyn))))
                [] bnds)]
    (-> 
      (Poly* (map first frees-with-bnds)
             (map second frees-with-bnds)
             (with-frees (map (comp make-F first) frees-with-bnds)
               (parse-type type)))
      (with-meta {:actual-frees (map first frees-with-bnds)}))))

(defmethod parse-type-list 'All
  [[All bnds syn & more]]
  (assert (not more) "Bad All syntax")
  (parse-all-type bnds syn))

(defn parse-union-type [[u & types]]
  (apply Un (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (apply In (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defmethod parse-type-list 'Array
  [[_ syn & none]]
  (assert (empty? none) "Expected 1 argument to Array")
  (let [t (parse-type syn)]
    (->PrimitiveArray (Un) t t)))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (assert (empty? none) "Expected 3 arguments to Array3")
  (->PrimitiveArray (parse-type jsyn) (parse-type isyn) (parse-type osyn)))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply Fn-Intersection (doall (map parse-function types))))

(defmethod parse-type-list 'Fn
  [syn]
  (parse-fn-intersection-type syn))

(defmethod parse-type-list 'Seq* [syn] (->HeterogeneousSeq (mapv parse-type (rest syn))))
(defmethod parse-type-list 'List* [syn] (->HeterogeneousList (mapv parse-type (rest syn))))
(defmethod parse-type-list 'Vector* [syn] (->HeterogeneousVector (mapv parse-type (rest syn))))

(declare constant-type)

(defmethod parse-type-list 'HMap
  [[_ mandatory & {:keys [optional]}]]
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(constant-type k)
                        (parse-type v)])))]
    (let [mandatory (mapt mandatory)
          optional (mapt optional)]
      (make-HMap mandatory optional))))

(defn parse-RClass [cls-sym params-syn]
  (let [cls (resolve cls-sym)
        _ (assert (class? cls) (str cls-sym " cannot be resolved"))
        tparams (doall (map parse-type params-syn))]
    (RClass-of (Class->symbol cls) tparams)))

(defmethod parse-type-list 'Value
  [[Value syn]]
  (constant-type syn))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (assert (= #{}
             (set/intersection (set (keys optional))
                               (set (keys mandatory)))))
  (let [optional (into {} (for [[k v] optional]
                            (do (assert (keyword? k))
                              [(->Value k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (assert (keyword? k))
                               [(->Value k) (parse-type v)])))]
    (apply Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (->HeterogeneousSeq (mapcat #(find m %) ks)))))))))

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (let [res (resolve n)
        rsym (cond 
               (class? res) (Class->symbol res)
               (var? res) (var->symbol res))]
    (if-let [t ((some-fn @DATATYPE-ENV @PROTOCOL-ENV @TYPE-NAME-ENV) rsym)]
      (resolve-app* t (mapv parse-type args))
      (cond
        ;a Class that's not a DataType
        (class? res) (RClass-of (Class->symbol res) (mapv parse-type args))
        :else
        ;unqualified declared protocols and datatypes
        (if-let [s (let [svar (symbol (name (ns-name *ns*)) (name n))
                         scls (symbol (munge (str (ns-name *ns*) \. (name n))))]
                     (some #(and (@TYPE-NAME-ENV %)
                                 %)
                           [svar scls]))]
          (->App (->Name s) (mapv parse-type args))
          (throw (Exception. (str "Cannot parse list: " syn))))))))

(defmethod parse-type Cons [l] (parse-type-list l))
(defmethod parse-type IPersistentList [l] (parse-type-list l))

(defmulti parse-type-symbol identity)
(defmethod parse-type-symbol 'Any [_] (->Top))
(defmethod parse-type-symbol 'Nothing [_] (Bottom))

;Symbol -> Class
(def primitives
  {'long (RClass-of 'long nil)
   'int (RClass-of 'int nil)
   'boolean (RClass-of 'boolean nil)
   'void -nil})

(defmethod parse-type-symbol :default
  [sym]
  (if-let [f (sym *free-scope*)]
    f
    (let [qsym (if (namespace sym)
                 sym
                 (symbol (-> *ns* ns-name name) (name sym)))
          clssym (if (some #(= \. %) (str sym))
                   sym
                   (symbol (str (munge (-> *ns* ns-name name)) \. (name sym))))]
      (cond
        (primitives sym) (primitives sym)
        (@TYPE-NAME-ENV qsym) (->Name qsym)
        (@TYPE-NAME-ENV clssym) (->Name clssym)
        ;Datatypes that are annotated in this namespace, but not yet defined
        (@DATATYPE-ENV clssym) (@DATATYPE-ENV clssym)
        (@PROTOCOL-ENV qsym) (resolve-protocol qsym)
        :else (let [res (resolve sym)]
                ;(prn *ns* "res" sym "->" res)
                (cond 
                  (class? res) (or (@DATATYPE-ENV (symbol (.getName ^Class res)))
                                   (RClass-of (Class->symbol res) nil))
                  :else (if-let [t (and (var? res) 
                                        (@TYPE-NAME-ENV (var->symbol res)))]
                          t
                          (throw (Exception. (str "Cannot resolve type: " sym))))))))))

(defmethod parse-type Symbol [l] (parse-type-symbol l))
(defmethod parse-type Boolean [v] (if v -true -false)) 
(defmethod parse-type nil [_] -nil)

(defn parse-function [f]
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
        _ (assert (<= (count chk) 2) (str "Missing range in " f))

        {ellipsis-pos '...
         asterix-pos '*}
        (into {} (map vector all-dom (range)))

        _ (assert (not (and asterix-pos ellipsis-pos))
                  "Cannot provide both rest type and dotted rest type")

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos) nil))
        [drest-type _ drest-bnd] (when ellipsis-pos
                                   (drop (dec ellipsis-pos) all-dom))]
    (make-Function (doall (map parse-type fixed-dom))
                   (parse-type rng)
                   (when rest-type
                     (parse-type rest-type))
                   (when drest-type
                     (->DottedPretype
                       (with-frees [(*dotted-scope* drest-bnd)] ;with dotted bound in scope as free
                         (parse-type drest-type))
                       (:name (*dotted-scope* drest-bnd)))))))

(defmethod parse-type IPersistentVector
  [f]
  (apply Fn-Intersection [(parse-function f)]))

(def ^:dynamic *next-nme* 0) ;stupid readable variables

(defmulti unparse-type class)
(defn unp [t] (prn (unparse-type t)))

(defmethod unparse-type Top [_] 'Any)
(defmethod unparse-type Name [{:keys [id]}] id)
(defmethod unparse-type AnyValue [_] 'AnyValue)

(defmethod unparse-type CountRange [{:keys [lower upper]}]
  (cond
    (= lower upper) (list 'ExactCount lower)
    :else (list* 'CountRange lower (when upper [upper]))))

(defmethod unparse-type App 
  [{:keys [rator rands]}]
  (list* 'App (unparse-type rator) (mapv unparse-type rands)))

(defmethod unparse-type Result
  [{:keys [t]}]
  (unparse-type t))

(defmethod unparse-type F
  [{:keys [name]}]
  name)

(defmethod unparse-type PrimitiveArray
  [{:keys [input-type output-type]}]
  (list 'Array (unparse-type input-type) (unparse-type output-type)))

(defmethod unparse-type B
  [{:keys [idx]}]
  (list 'B idx))

(defmethod unparse-type Union
  [{types :types}]
  (if (seq types)
    (list* 'U (doall (map unparse-type types)))
    'Nothing))

(defmethod unparse-type Intersection
  [{types :types}]
  (list* (if (and (seq types)
                  (every? Function? types))
           'Fn
           'I)
         (doall (map unparse-type types))))

(defmethod unparse-type Function
  [{:keys [dom rng rest drest]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 [(unparse-type rest) '*])
               (when drest
                 (let [{:keys [pre-type name]} drest]
                   [(unparse-type pre-type) '... name]))
               (let [{:keys [t fl o]} rng]
                 (concat ['-> (unparse-type t)]
                         (when (not (and ((some-fn TopFilter? BotFilter?) (:then fl))
                                         ((some-fn TopFilter? BotFilter?) (:else fl))))
                           [(unparse-filter-set fl)])
                         (when (not ((some-fn NoObject? EmptyObject?) o))
                           [(unparse-object o)]))))))

(defmethod unparse-type Protocol
  [{:keys [the-var poly?]}]
  (if poly?
    (list* the-var (mapv unparse-type poly?))
    the-var))

(defmethod unparse-type DataType
  [{:keys [the-class poly?]}]
  (if poly?
    (list* the-class (mapv unparse-type poly?))
    the-class))

(defmethod unparse-type RClass
  [{:keys [the-class poly?]}]
  (if (empty? poly?)
    the-class
    (list* the-class
           (doall (map unparse-type poly?)))))

(defmethod unparse-type Mu
  [m]
  (let [nme (gensym "Mu")
        body (Mu-body* nme m)]
    (list 'Rec [nme] (unparse-type body))))

(defmethod unparse-type PolyDots
  [{:keys [nbound] :as p}]
  (let [{:keys [actual-frees dvar-name]} (meta p)
        free-names actual-frees
        given-names? (and free-names dvar-name)
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs (if given-names?
             (vec (concat free-names [dvar-name]))
             (vec 
               (for [x (range *next-nme* end-nme)]
                 (symbol (str "v" x)))))
        body (PolyDots-body* fs p)]
    (binding [*next-nme* end-nme]
      (list 'All (vec (concat (butlast fs) [(last fs) '...])) (unparse-type body)))))

(defmethod unparse-type Poly
  [{:keys [nbound] :as p}]
  (let [free-names (-> p meta :actual-frees)
        given-names? free-names
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs-names (or (and given-names? free-names)
                     (vec
                       (for [x (range *next-nme* end-nme)]
                         (symbol (str "v" x)))))
        bbnds (Poly-bbnds* fs-names p)
        fs (if given-names?
             (vec
               (for [[name {:keys [upper-bound lower-bound]}] (map vector 
                                                                   (-> p meta :actual-frees)
                                                                   bbnds)]
                 (let [u (when-not (Top? upper-bound)
                           (unparse-type upper-bound))
                       l (when-not (Bottom? lower-bound)
                           (unparse-type lower-bound))]
                   (or (and u l [name :< u :> l])
                       (and u [name :< u])
                       (and l [name :> l])
                       name))))
             fs-names)
        body (Poly-body* fs-names p)]
    (binding [*next-nme* end-nme]
      (list 'All fs (unparse-type body)))))

(defmethod unparse-type Value
  [v]
  (list 'Value (:val v)))

(defmethod unparse-type HeterogeneousMap
  [v]
  (list 'HMap (into {} (map (fn [[k v]]
                              (assert (Value? k))
                              (vector (:val k)
                                      (unparse-type v)))
                            (:types v)))))

(defmethod unparse-type HeterogeneousSeq
  [v]
  (list* 'Seq* (doall (map unparse-type (:types v)))))

(defmethod unparse-type HeterogeneousVector
  [v]
  (list* 'Vector* (doall (map unparse-type (:types v)))))

(defmethod unparse-type HeterogeneousList
  [v]
  (list* 'List* (doall (map unparse-type (:types v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(def variance-map? (hash-c? symbol? variance?))

(declare ^:dynamic *frees-mode* frees-in)

(defn fv-variances 
  "Map of frees to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::frees]
    (frees-in t)))

(defn idx-variances 
  "Map of indexes to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::idxs]
    (frees-in t)))

(defn fv 
  "All frees in type"
  [t]
  {:post [((set-c? symbol?) %)]}
  (set (keys (fv-variances t))))

(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t]
  {:post [((set-c? symbol?) %)]}
  (set (keys (idx-variances t))))

(defn flip-variances [vs]
  {:pre [(variance-map? vs)]}
  (into {} (for [[k vari] vs]
             [k (case vari
                  :covariant :contravariant
                  :contravariant :covariant
                  vari)])))

(defn combine-frees [& frees]
  {:pre [(every? variance-map? frees)]
   :post [(variance-map? %)]}
  (into {}
        (apply merge-with (fn [old-vari new-vari]
                            (cond 
                              (= old-vari new-vari) old-vari
                              (= old-vari :dotted) new-vari
                              (= new-vari :dotted) old-vari
                              (= old-vari :constant) new-vari
                              (= new-vari :constant) old-vari
                              :else :invariant))
               frees)))

(derive ::frees ::any-var)
(derive ::idxs ::any-var)

(def ^:dynamic *frees-mode* nil)
(set-validator! #'*frees-mode* #(or (= ::frees %)
                                    (= ::idxs %)
                                    (nil? %)))

(declare frees)

(defn frees-in [t]
  {:post [(variance-map? %)]}
  (frees t))

(defmulti frees (fn [t] [*frees-mode* (class t)]))

(defmethod frees [::any-var Result]
  [{:keys [t fl o]}]
  (combine-frees (frees t)
                 (frees fl)
                 (frees o)))

(defmethod frees [::any-var FilterSet]
  [{:keys [then else]}]
  (combine-frees (frees then)
                 (frees else)))

(defmethod frees [::any-var TypeFilter]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var NotTypeFilter]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var AndFilter]
  [{:keys [a c]}] 
  (combine-frees (frees a)
                 (frees c)))

(defmethod frees [::frees F]
  [{:keys [name] :as t}]
  {name :covariant})

(defmethod frees [::idxs F] [t] {})

(defmethod frees [::any-var Value] [t] {})
(defmethod frees [::any-var AnyValue] [t] {})
(defmethod frees [::any-var Top] [t] {})
(defmethod frees [::any-var TopFilter] [t] {})
(defmethod frees [::any-var BotFilter] [t] {})
(defmethod frees [::any-var NoObject] [t] {})

(defmethod frees [::any-var DataType]
  [{:keys [fields poly?]}]
  (apply combine-frees 
         (mapv frees (concat (vals fields) poly?))))

(defmethod frees [::any-var HeterogeneousList]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var PrimitiveArray]
  [{:keys [input-type output-type]}] 
  (combine-frees (frees input-type)
                 (frees output-type)))

(defmethod frees [::any-var HeterogeneousSeq]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var HeterogeneousMap]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees (concat (keys types) (vals types)))))

(defmethod frees [::any-var HeterogeneousVector]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(defmethod frees [::frees Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               [(vals kws)])))
                               [(frees rng)]
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees flip-variances)
                                          (:name drest))]))))

(defmethod frees [::idxs Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               (vals kws))))
                               [(frees rng)]
                               (when drest
                                 (let [{:keys [name pre-type]} drest]
                                   [{name :contravariant}
                                    (-> pre-type
                                      frees flip-variances)])))))

(defmethod frees [::any-var RClass]
  [t]
  (let [varis (:variances t)
        args (:poly? t)]
    (assert (= (count args) (count varis)))
    (apply combine-frees (for [[arg va] (map vector args varis)]
                           (case va
                             :covariant (frees arg)
                             :contravariant (flip-variances (frees arg))
                             :invariant (let [fvs (frees arg)]
                                          (into {}
                                                (for [[k _] fvs]
                                                  [k :invariant]))))))))

(defmethod frees [::any-var Poly]
  [{:keys [nbound scope]}]
  (frees scope))

(defmethod frees [::any-var PolyDots]
  [{:keys [nbound scope]}]
  (frees scope))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elim

(declare promote demote)

(defn promote-var [T V]
  {:pre [(Type? T)
         (set? V)
         (every? symbol? V)]
   :post [(Type? %)]}
  (promote T V))

(defn demote-var [T V]
  {:pre [(Type? T)
         (set? V)
         (every? symbol? V)]
   :post [(Type? %)]}
  (demote T V))

(defmulti promote 
  "Eliminate all variables V in t by promotion"
  (fn [T V] 
    {:pre [(Type? T)
           (set? V)
           (every? symbol? V)]}
    (class T)))

(defmulti demote 
  "Eliminate all variables V in T by demotion"
  (fn [T V]
    {:pre [(Type? T)
           (set? V)
           (every? symbol? V)]}
    (class T)))

(defmethod promote PrimitiveArray
  [T V]
  (-> T
    (update-in [:input-type] #(promote % V))
    (update-in [:output-type] #(promote % V))))

(defmethod demote PrimitiveArray
  [T V]
  (-> T
    (update-in [:input-type] #(demote % V))
    (update-in [:output-type] #(demote % V))))

(defmethod promote F
  [{:keys [name] :as T} V]
  (if (V name)
    -any
    T))

(defmethod demote F
  [{:keys [name] :as T} V]
  (if (V name)
    (Bottom)
    T))

(defmethod promote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] #(into {}
                               (for [[k v] %]
                                 [k (promote v V)])))))

(defmethod demote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] #(into {}
                               (for [[k v] %]
                                 [k (demote v V)])))))

(defmethod promote HeterogeneousVector
  [T V]
  (-> T
    (update-in [:types] #(apply list (map promote % (repeat V))))))

(defmethod demote HeterogeneousVector
  [T V]
  (-> T
    (update-in [:types] #(apply list (map demote % (repeat V))))))

(defmethod promote HeterogeneousList
  [T V]
  (-> T
    (update-in [:types] #(apply list (map promote % (repeat V))))))

(defmethod demote HeterogeneousList
  [T V]
  (-> T
    (update-in [:types] #(apply list (map demote % (repeat V))))))

(defmethod promote Value [T V] T)
(defmethod demote Value [T V] T)

(defmethod promote DataType [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv promote %)))
    (update-in [:fields] #(apply array-map
                                 (apply concat
                                        (for [[k v] %]
                                          [k (promote v)]))))))
(defmethod demote DataType [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv demote %)))
    (update-in [:fields] #(apply array-map
                                 (apply concat
                                        (for [[k v] %]
                                          [k (demote v)]))))))

(defmethod promote Name [T V] T)
(defmethod demote Name [T V] T)

(defmethod promote Top [T V] T)
(defmethod demote Top [T V] T)

(defmethod promote Union 
  [T V] 
  (-> T
    (update-in [:types] #(set (map promote % (repeat V))))))

(defmethod demote Union 
  [T V] 
  (-> T
    (update-in [:types] #(set (mapv demote % (repeat V))))))

(defmethod promote Intersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(defmethod demote Intersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv demote % (repeat V)))))

(defmethod promote RClass
  [T V]
  (let [pmt #(promote % V)]
    (-> T
      (update-in [:poly?] #(when %
                             (mapv pmt %)))
      (update-in [:replacements] #(into {} (for [[k v] %]
                                             [k (pmt v)]))))))

(defmethod demote RClass
  [T V]
  (let [dmt #(demote % V)]
    (-> T
      (update-in [:poly?] #(when %
                             (mapv dmt %)))
      (update-in [:replacements] #(into {} (for [[k v] %]
                                             [k (dmt v)]))))))

(defmethod promote Poly
  [{:keys [nbound] :as T} V]
  (let [names (repeatedly nbound gensym)
        pmt-body (promote (Poly-body* names T) V)]
    (Poly* names 
           (Poly-bbnds* names T)
           pmt-body)))

(defmethod demote Poly
  [{:keys [nbound] :as T} V]
  (let [names (repeatedly nbound gensym)
        dem-body (demote (Poly-body* names T) V)]
    (Poly* names 
           (Poly-bbnds* names T)
           dem-body)))

(defmethod promote Function
  [{:keys [dom rng rest drest kws] :as T} V]
  (let [pmt #(promote % V)
        dmt #(demote % V)
        dmt-kw #(into {} (for [[k v] %]
                           [k (dmt v)]))]
    (cond 
      ;if filter contains V, give up
      (seq (set/intersection V (:fl rng))) (->TopFunction)

      ;if dotted bound is in V, transfer to rest args
      (and drest (V (:name drest)))
      (-> T
        (update-in [:dom] #(mapv dmt %))
        (update-in [:rng] pmt)
        (assoc :rest (dmt (:pre-type drest)))
        (assoc :drest nil)
        (assoc :kws (when kws
                      (-> kws
                        (update-in [:mandatory] dmt-kw)
                        (update-in [:optional] dmt-kw)))))

      :else
      (-> T
        (update-in [:dom] #(mapv dmt %))
        (update-in [:rng] pmt)
        (update-in [:rest] #(when %
                              (dmt %)))
        (update-in [:drest] #(when %
                               (-> %
                                 (update-in [:pre-type] dmt))))
        (update-in [:kws] #(when %
                             (-> %
                               (update-in [:mandatory] dmt-kw)
                               (update-in [:optional] dmt-kw))))))))

(defmethod demote Function
  [{:keys [dom rng rest drest kws] :as T} V]
  (let [pmt #(promote % V)
        dmt #(demote % V)
        pmt-kw #(into {} (for [[k v] %]
                           [k (pmt v)]))]
    (cond 
      ;if filter contains V, give up
      (seq (set/intersection V (:fl rng))) (->TopFunction)

      ;if dotted bound is in V, transfer to rest args
      (and drest (V (:name drest)))
      (-> T
        (update-in [:dom] #(mapv pmt %))
        (update-in [:rng] dmt)
        (assoc :rest (pmt (:pre-type drest)))
        (assoc :drest nil)
        (assoc :kws (when kws
                      (-> kws
                        (update-in [:mandatory] pmt-kw)
                        (update-in [:optional] pmt-kw)))))

      :else
      (-> T
        (update-in [:dom] #(mapv pmt %))
        (update-in [:rng] dmt)
        (update-in [:rest] #(when %
                              (pmt %)))
        (update-in [:drest] #(when %
                               (-> %
                                 (update-in [:pre-type] pmt))))
        (update-in [:kws] #(when %
                             (-> %
                               (update-in [:mandatory] pmt-kw)
                               (update-in [:optional] pmt-kw))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation

(defrecord t-subst [type bnds]
  ""
  [(Type? type)
   (Bounds? bnds)])

(defrecord i-subst [types]
  ""
  [(every? Type? types)])

(defrecord i-subst-starred [types starred]
  ""
  [(every? Type? types)
   (Type? starred)])

(defrecord i-subst-dotted [types dty dbound]
  ""
  [(or (nil? types)
       (every? Type? types))
   (Type? dty)
   (F? dbound)])

(def subst-rhs? (some-fn t-subst? i-subst? i-subst-starred? i-subst-dotted?))

(def substitution-c? (every-pred map? 
                                 #(every? symbol? (keys %)) 
                                 #(every? subst-rhs? (vals %))))

(defrecord c [S X T bnds]
  "A type constraint on a variable within an upper and lower bound"
  [(Type? S)
   (symbol? X)
   (Type? T)
   (Bounds? bnds)])

;; fixed : Listof[c]
;; rest : option[c]
;; a constraint on an index variable
;; the index variable must be instantiated with |fixed| arguments, each meeting the appropriate constraint
;; and further instantions of the index variable must respect the rest constraint, if it exists
(defrecord dcon [fixed rest]
  ""
  [(every? c? fixed)
   (or (nil? rest)
       (c? rest))])

(defrecord dcon-exact [fixed rest]
  ""
  [(every? c? fixed)
   (c? rest)])

(defrecord dcon-dotted [fixed dc dbound]
  ""
  [(every? c? fixed)
   (c? dc)
   (F? dbound)])

(def dcon-c? (some-fn dcon? dcon-exact? dcon-dotted?))

;; map : hash mapping index variables to dcons
(defrecord dmap [map]
  ""
  [((hash-c? symbol? dcon-c?) map)])

(defrecord cset-entry [fixed dmap]
  ""
  [((hash-c? symbol? c?) fixed)
   (dmap? dmap)])

;; maps is a list of cset-entries, consisting of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(defrecord cset [maps]
  ""
  [(every? cset-entry? maps)])


;widest constraint possible
(defn no-constraint [v bnds]
  {:pre [(symbol? v)
         (Bounds? bnds)]}
  (->c (Un) v (->Top) bnds))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(defn empty-cset [X Y]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {}))]))

(defn meet [s t] (In s t))
(defn join [s t] (Un s t))

(declare subtype type-error)

(defn c-meet [{S  :S X  :X T  :T bnds  :bnds :as c1}
              {S* :S X* :X T* :T bnds* :bnds :as c2}
              & [var]]
  (prn "c-meet" c1 c2)
  (when-not (or var (= X X*))
    (throw (Exception. (str "Non-matching vars in c-meet:" X X*))))
  (when-not (= bnds bnds*)
    (throw (Exception. (str "Non-matching bounds in c-meet:" bnds bnds*))))
  (let [S (join S S*)
        T (meet T T*)]
    (when-not (subtype? S T)
      (type-error S T))
    (->c S (or var X) T bnds)))

(declare dmap-meet)

(defn cset-meet [{maps1 :maps :as x} {maps2 :maps :as y}]
  {:pre [(cset? x)
         (cset? y)]}
  (let [maps (doall (for [[{map1 :fixed dmap1 :dmap} {map2 :fixed dmap2 :dmap}] (map vector maps1 maps2)]
                      (->cset-entry (merge-with c-meet map1 map2)
                                    (dmap-meet dmap1 dmap2))))]
    (when (empty? maps)
      (throw (Exception. (str "No meet found for csets"))))
    (->cset maps)))

(defn cset-meet* [args]
  {:pre [(every? cset? args)]
   :post [(cset? %)]}
  (reduce (fn [a c] (cset-meet a c))
          (->cset [(->cset-entry {} (->dmap {}))])
          args))

(defn cset-combine [l]
  {:pre [(every? cset? l)]}
  (let [mapss (map :maps l)]
    (->cset (apply concat mapss))))

;add new constraint to existing cset
(defn insert-constraint [cs var S T bnds]
  {:pre [(cset? cs)
         (symbol? var)
         (Type? S)
         (Type? T)
         (Bounds? bnds)]
   :post [(cset? %)]}
  (->cset (doall
            (for [{fmap :fixed dmap :dmap} (:maps cs)]
              (->cset-entry (assoc fmap var (->c S var T bnds))
                            dmap)))))

(defn dcon-meet [dc1 dc2]
  {:pre [(dcon-c? dc1)
         (dcon-c? dc2)]
   :post [(dcon-c? %)]}
  (cond
    (and (dcon-exact? dc1)
         ((some-fn dcon? dcon-exact?) dc2))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2]
      (when-not (and rest2 (= (count fixed1) (count fixed2)))
        (type-error fixed1 fixed2))
      (->dcon-exact
        (doall
          (for [[c1 c2] (map vector fixed1 fixed2)]
            (c-meet c1 c2 (:X c1))))
        (c-meet rest1 rest2 (:X rest1))))
    ;; redo in the other order to call the first case
    (and (dcon? dc1)
         (dcon-exact? dc2))
    (dcon-meet dc2 dc1)

    (and (dcon? dc1)
         (not (:rest dc1))
         (dcon? dc2)
         (not (:rest dc2)))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed} dc2]
      (when-not (= (count fixed1) (count fixed2))
        (throw (Exception. (prn-str "Don't match: " fixed1 fixed2))))
      (->dcon
        (doall
          (for [[c1 c2] (map vector fixed1 fixed2)]
            (c-meet c1 c2 (:X c1))))
        nil))

    (and (dcon? dc1)
         (not (:rest dc1))
         (dcon? dc2))
    (let [{fixed1 :fixed} dc1
          {fixed2 :fixed rest :rest} dc2]
      (when-not (>= (count fixed1) (count fixed2))
        (throw (Exception. (prn-str "Don't match: " fixed1 fixed2))))
      (->dcon
        (doall
          (for [[c1 c2] (map vector fixed1 (concat fixed2 (repeat rest)))]
            (c-meet c1 c2 (:X c1))))
        nil))

    (and (dcon? dc1)
         (dcon? dc2)
         (not (:rest dc2)))
    (dcon-meet dc2 dc1)

    (and (dcon? dc1)
         (dcon? dc2))
    (let [{fixed1 :fixed rest1 :rest} dc1
          {fixed2 :fixed rest2 :rest} dc2
          [shorter longer srest lrest]
          (if (< (count fixed1) (count fixed2))
            [fixed1 fixed2 rest1 rest2]
            [fixed2 fixed1 rest2 rest1])]
      (->dcon
        (doall
          (for [[c1 c2] (map vector longer (concat shorter (repeat srest)))]
            (c-meet c1 c2 (:X c1))))
        (c-meet lrest srest (:X lrest))))

    (and (dcon-dotted? dc1)
         (dcon-dotted? dc2))
    (let [{fixed1 :fixed c1 :dc {bound1 :name} :dbound} dc1
          {fixed2 :fixed c2 :dc {bound2 :name} :dbound} dc2]
      (when-not (and (= (count fixed1) (count fixed2))
                     (= bound1 bound2))
        (throw (Exception. (prn-str "Don't match: " bound1 bound2))))
      (->dcon-dotted (doall (for [[c1 c2] (map vector fixed1 fixed2)]
                              (c-meet c1 c2 (:X c1))))
                     (c-meet c1 c2 bound1) bound1))

    (and (dcon? dc1)
         (dcon-dotted? dc2))
    (throw (Exception. (prn-str "Don't match: " dc1 dc2)))

    (and (dcon-dotted? dc1)
         (dcon? dc2))
    (throw (Exception. (prn-str "Don't match: " dc1 dc2)))

    :else (throw (Exception. (prn-str "Got non-dcons: " dc1 dc2)))))

(defn dmap-meet [dm1 dm2]
  {:pre [(dmap? dm1)
         (dmap? dm2)]
   :post [(dmap? %)]}
  (->dmap (merge-with dcon-meet (:map dm1) (:map dm2))))

;current seen subtype relations, for recursive types
;(Set [Type Type])
(def ^:dynamic *cs-current-seen* #{})

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained
;; Y : the set of index variables to be constrained
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner, extended with
;; the index variables from the TOPLAS paper
(defmulti cs-gen*
  (fn [V X Y S T] 
    {:pre [((set-c? symbol?) V)
           (every? (hash-c? symbol Bounds?) [X Y])
           (AnyType? S)
           (AnyType? T)]}
    [(class S) (class T)]))

; (see cs-gen*)
;cs-gen calls cs-gen*, remembering the current subtype for recursive types
; Add methods to cs-gen*, but always call cs-gen

(defn cs-gen [V X Y S T]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (AnyType? S)
         (AnyType? T)]
   :post [(cset? %)]}
  (if (or (*cs-current-seen* [S T]) 
          (subtype? S T))
    ;already been around this loop, is a subtype
    (empty-cset X Y)
    (binding [*cs-current-seen* (conj *cs-current-seen* [S T])]
      (cond
        (Top? T)
        (empty-cset X Y)

        ;constrain *each* element of S to be below T, and then combine the constraints
        (Union? S)
        (cset-meet*
          (cons (empty-cset X Y)
                (mapv #(cs-gen V X Y % T) (:types S))))

        ;; find *an* element of T which can be made a supertype of S
        (Union? T)
        (if-let [cs (seq (filter identity (mapv #(try (cs-gen V X Y S %)
                                                   (catch IllegalArgumentException e
                                                     (throw e))
                                                   (catch Exception e)) 
                                                (:types T))))]
          (cset-combine cs)
          (type-error S T))

        (and (Intersection? S)
             (Intersection? T))
        (cset-meet*
          (doall
            ; for each element of T, we need at least one element of S that works
            (for [t* (:types T)]
              (if-let [results (seq (filter identity
                                            (map #(try 
                                                    (cs-gen V X Y % t*)
                                                    (catch IllegalArgumentException e
                                                      (throw e))
                                                    (catch Exception e))
                                                 (:types S))))]
                (cset-combine results)
                (type-error S T)))))

        ;; find *an* element of S which can be made a subtype of T
        (Intersection? S)
        (if-let [cs (some #(try (cs-gen V X Y % T)
                             (catch IllegalArgumentException e
                               (throw e))
                             (catch Exception e)) ;TODO specialised data Exceptions
                          (:types S))]
          cs
          (throw (Exception. (str "Could not constrain "
                                  (unparse-type S) " to be under "
                                  (unparse-type T)))))

        ;constrain *every* element of T to be above S, and then meet the constraints
        ;FIXME Should this combine csets instead?
        (Intersection? T)
        (cset-meet*
          (cons (empty-cset X Y)
                (mapv #(cs-gen V X Y S %) (:types T))))

        (App? S)
        (cs-gen V X Y (resolve-App S) T)

        (App? T)
        (cs-gen V X Y S (resolve-App T))

        :else
        (cs-gen* V X Y S T)))))

(defmethod cs-gen* :default
  [V X Y S T]
  (assert (subtype? S T) (type-error S T))
  (empty-cset X Y))

(defmethod cs-gen* [Result Result] 
  [V X Y S T] 
  (cs-gen V X Y (:t S) (:t T)))

(defmethod cs-gen* [Value AnyValue] 
  [V X Y S T] 
  (empty-cset X Y))

(defmethod cs-gen* [Type Top] 
  [V X Y S T] 
  (empty-cset X Y))

(defmethod cs-gen* [HeterogeneousVector RClass] 
  [V X Y S T]
  (cs-gen V X Y 
          (In (RClass-of IPersistentVector [(apply Un (:types S))]) 
              (make-ExactCountRange (count (:types S))))
          T))

(declare cs-gen-list)

(defmethod cs-gen* [DataType DataType] 
  [V X Y S T]
  (assert (= (:the-class S) (:the-class T)) (type-error S T))
  (if (seq (:poly? S))
    (cs-gen-list V X Y (:poly? S) (:poly? T))
    (empty-cset X Y)))

(defmethod cs-gen* [HeterogeneousVector HeterogeneousVector] 
  [V X Y S T]
  (cs-gen-list V X Y (:types S) (:types T)))

(defmethod cs-gen* [HeterogeneousMap HeterogeneousMap]
  [V X Y S T]
  (let [Skeys (set (keys (:types S)))
        Tkeys (set (keys (:types T)))]
    ; All keys must be values
    (when-not (every? Value? (set/union Skeys Tkeys))
      (type-error S T))
    ; All keys on the left must appear on the right
    (when-not (empty? (set/difference Skeys Tkeys))
      (type-error S T))
    (let [nocheck-keys (set/difference Tkeys Skeys)
          STvals (vals (merge-with vector (:types S) (apply dissoc (:types T) nocheck-keys)))
          Svals (map first STvals)
          Tvals (map second STvals)]
      (cs-gen-list V X Y Svals Tvals))))

(defmethod cs-gen* [HeterogeneousMap RClass] 
  [V X Y S T]
  (let [[ks vs] [(apply Un (keys (:types S)))
                 (apply Un (vals (:types S)))]]
    (cs-gen V X Y (RClass-of (Class->symbol IPersistentMap) [ks vs]) T)))

(defmethod cs-gen* [RClass RClass] 
  [V X Y S T]
  (let [relevant-S (some #(and (= (:the-class %) (:the-class T))
                               %)
                         (conj (RClass-supers* S) S))]
    (cond
      relevant-S
      (cset-meet*
        (cons (empty-cset X Y)
              (doall
                (for [[vari si ti] (map vector
                                        (:variances T)
                                        (:poly? relevant-S)
                                        (:poly? T))]
                  (case vari
                    (:covariant :constant) (cs-gen V X Y si ti)
                    :contravariant (cs-gen V X Y ti si)
                    :invariant (cset-meet (cs-gen V X Y si ti)
                                          (cs-gen V X Y ti si)))))))
      :else (type-error S T))))

(prefer-method cs-gen* [F Type] [Type F])

(defn demote-F [V X Y {:keys [name bnds] :as S} T]
  {:pre [(F? S)]}
  ;constrain T to be below S (but don't mention V)
  (assert (contains? X name) (str X name))
  (when (and (F? T)
             (bound-index? (:name T))
             (not (bound-tvar? (:name T))))
    (type-error S T))
  (let [dt (demote-var T V)]
    (-> (empty-cset X Y)
      (insert-constraint name (Bottom) dt (X name)))))

(defn promote-F [V X Y S {:keys [name] :as T}]
  {:pre [(F? T)]}
  ;T is an F
  ;S is any Type
  ;constrain T to be above S (but don't mention V)
  (assert (contains? X name) (str X T))
  (when (and (F? S)
             (bound-index? (:name S))
             (not (bound-tvar? (:name S))))
    (type-error S T))
  (let [ps (promote-var S V)]
    (-> (empty-cset X Y)
      (insert-constraint name ps -any (X name)))))

(defmethod cs-gen* [F Type]
  [V X Y S T]
  (prn "cs-gen* [F Type]" S T)
  (cond
    (contains? X (:name S))
    (demote-F V X Y S T)

    (and (F? T)
         (contains? X (:name T)))
    (promote-F V X Y S T)

    :else (type-error S T)))

(defmethod cs-gen* [Type F]
  [V X Y S T]
  ;(prn "cs-gen* [Type F]" S T X)
  (cond
    (contains? X (:name T))
    (promote-F V X Y S T)

    (and (F? S)
         (contains? X (:name S)))
    (demote-F V X Y S T)

    :else (type-error S T)))

(defn singleton-dmap [dbound dcon]
  (->dmap {dbound dcon}))

(defn mover [cset dbound vars f]
  {:pre [(cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cset? %)]}
  (->cset (map
            (fn [{cmap :fixed dmap :dmap}]
              (->cset-entry (apply dissoc cmap dbound vars)
                            (dmap-meet 
                              (singleton-dmap 
                                dbound
                                (f cmap dmap))
                              (->dmap (dissoc (:map dmap) dbound)))))
            (:maps cset))))

;; dbound : index variable
;; cset : the constraints being manipulated
(defn move-rest-to-dmap [cset dbound & {:keys [exact]}]
  {:pre [(cset? cset)
         (symbol? dbound)
         ((some-fn nil? true?) exact)]
   :post [(cset? %)]}
  (mover cset dbound nil
         (fn [cmap dmap]
           ((if exact ->dcon-exact ->dcon)
              nil
              (if-let [c (cmap dbound)]
                c
                (throw (Exception. (str "No constraint for bound " dbound))))))))


;; dbound : index variable
;; vars : listof[type variable] - temporary variables
;; cset : the constraints being manipulated
;; takes the constraints on vars and creates a dmap entry contstraining dbound to be |vars|
;; with the constraints that cset places on vars
(defn move-vars-to-dmap [cset dbound vars]
  {:pre [(cset? cset)
         (symbol? dbound)
         (every? symbol? vars)]
   :post [(cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           (->dcon (doall (for [v vars]
                            (if-let [c (cmap v)]
                              c
                              (throw (Exception. (str "No constraint for new var " v))))))
                   nil))))

;; This one's weird, because the way we set it up, the rest is already in the dmap.
;; This is because we create all the vars, then recall cgen/arr with the new vars
;; in place, and the "simple" case will then call move-rest-to-dmap.  This means
;; we need to extract that result from the dmap and merge it with the fixed vars
;; we now handled.  So I've extended the mover to give access to the dmap, which we use here.
(defn move-vars+rest-to-dmap [cset dbound vars & {:keys [exact]}]
  {:pre [(cset? cset)
         (symbol? dbound)
         ((set-c? symbol?) vars)
         ((some-fn nil? true?) exact)]
   :post [(cset? %)]}
  (mover cset dbound vars
         (fn [cmap dmap]
           ((if exact ->dcon-exact ->dcon)
              (doall
                (for [v vars]
                  (if-let [c (cmap v)]
                    c
                    (throw (Exception. (str "No constraint for new var " v))))))
              (if-let [c ((:map dmap) dbound)]
                (cond
                  (and (dcon? c)
                       (not (:fixed c))) (:rest c)
                  (and (dcon-exact? c)
                       (not (:fixed c))) (:rest c)
                  :else (throw (Exception. (str "did not a get a rest-only dcon when moving to the dmap"))))
                (throw (Exception. (str "No constraint for bound " dbound))))))))

;; Maps dotted vars (combined with dotted types, to ensure global uniqueness)
;; to "fresh" symbols.
;; That way, we can share the same "fresh" variables between the elements of a
;; cset if they're talking about the same dotted variable.
;; This makes it possible to reduce the size of the csets, since we can detect
;; identical elements that would otherwise differ only by these fresh vars.
;; The domain of this map is pairs (var . dotted-type).
;; The range is this map is a list of symbols generated on demand, as we need
;; more dots.
(def ^:private DOTTED-VAR-STORE (atom {}))

;; Take (generate as needed) n symbols that correspond to variable var used in
;; the context of type t.
(defn- var-store-take [var t n]
  (let [key [t n]
        res (@DOTTED-VAR-STORE key)]
    (if (>= (count res) n)
      ;; there are enough symbols already, take n
      (take n res)
      ;; we need to generate more
      (let [new (take (- n (count res))
                      (repeatedly #(gensym var)))
            all (concat res new)]
        (swap! DOTTED-VAR-STORE assoc key all)
        all))))

(declare cs-gen-list)

(defn cs-gen-Function
  [V X Y S T]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (Function? S)
         (Function? T)]
   :post [(cset? %)]}
  (letfn [(cg [S T] (cs-gen V X Y S T))]
    (cond
      ;easy case - no rests, drests, kws
      (and (not (:rest S))
           (not (:rest T))
           (not (:drest S))
           (not (:drest T))
           (not (:kws S))
           (not (:kws T)))
      ; contravariant
      (cset-meet* [(cs-gen-list V X Y (:dom T) (:dom S))
                   ; covariant
                   (cg (:rng S) (:rng T))])

      ;just a rest arg, no drest, no keywords
      (and (or (:rest S)
               (:rest T))
           (not (:drest S))
           (not (:drest T))
           (not (:kws S))
           (not (:kws T)))
      (let [arg-mapping (cond
                          ;both rest args are present, so make them the same length
                          (and (:rest S) (:rest T))
                          (cs-gen-list V X Y 
                                       (cons (:rest T) (concat (:dom T) (repeat (- (count (:dom S))
                                                                                   (count (:dom T)))
                                                                                (:rest T))))
                                       (cons (:rest S) (concat (:dom S) (repeat (- (count (:dom T))
                                                                                   (count (:dom S)))
                                                                                (:rest S)))))
                          ;no rest arg on the right, so just pad left and forget the rest arg
                          (and (:rest S) (not (:rest T)))
                          (cs-gen-list V X Y
                                       (:dom T)
                                       (concat (:dom S) (repeat (- (count (:dom T))
                                                                   (count (:dom S)))
                                                                (:rest S))))
                          ;no rest arg on left, or wrong number = fail
                          :else (type-error S T))
            ret-mapping (cs-gen V X Y (:rng S) (:rng T))]
        (cset-meet* [arg-mapping ret-mapping]))

      ;; dotted on the left, nothing on the right
      (and (not (:rest S))
           (not (:rest T))
           (:drest S)
           (not (:drest T))
           (not (:kws S))
           (not (:kws T)))
      (let [{dty :pre-type dbound :name} (:drest S)]
        (when-not (Y dbound)
          (type-error S T))
        (when-not (<= (count (:dom S)) (count (:dom T)))
          (type-error S T))
        (let [vars (var-store-take dbound dty (- (count (:dom S))
                                                 (count (:dom T))))
              new-tys (doall (for [var vars]
                               (substitute (make-F var) dbound dty)))
              new-t-fun (make-Function (concat (:dom T) new-tys) (:rng T))
              new-cset (cs-gen-Function V 
                                        ;move dotted lower/upper bounds to vars
                                        (merge X (zipmap vars (repeat (Y dbound)))) Y S new-t-fun)]
          (move-vars-to-dmap new-cset dbound vars)))

      ;; dotted on the right, nothing on the left
      (and (not ((some-fn :rest :drest) S))
           (:drest T))
      (let [{dty :pre-type dbound :name} (:drest T)]
        (when-not (Y dbound)
          (type-error S T))
        (when-not (<= (count (:dom T)) (count (:dom S)))
          (type-error S T))
        (let [vars (var-store-take dbound dty (- (count (:dom S)) (count (:dom T))))
              new-tys (doall
                        (for [var vars]
                          (substitute (make-F var) dbound dty)))
              new-t-arr (->Function (concat (:dom T) new-tys) (:rng T) nil nil nil)
              new-cset (cs-gen-Function V 
                                        ;move dotted lower/upper bounds to vars
                                        (merge X (zipmap vars (repeat (Y dbound)))) Y S new-t-arr)]
          (move-vars-to-dmap new-cset dbound vars)))

      ;; * <: ...
      (and (:rest S)
           (:drest T))
      (let [{t-dty :pre-type dbound :name} (-> T :drest)]
        (when-not (Y dbound)
          (type-error S T))
        (if (<= (count (:dom S)) (count (:dom T)))
          ;; the simple case
          (let [arg-mapping (cs-gen-list V X Y (:dom T) (concat (:dom S) (repeat (- (count (:dom T)) (count (:dom S))) (:rest S))))
                darg-mapping (move-rest-to-dmap (cs-gen V (merge X {dbound (Y dbound)}) Y t-dty (:rest S)) dbound)
                ret-mapping (cg (:rng S) (:rng T))]
            (cset-meet* [arg-mapping darg-mapping ret-mapping]))
          ;; the hard case
          (let [vars (var-store-take dbound t-dty (- (count (:dom S)) (count (:dom T))))
                new-tys (doall (for [var vars]
                                 (substitute (make-F var) dbound t-dty)))
                new-t-arr (->Function (concat (:dom T) new-tys) (:rng T) nil (->DottedPretype t-dty dbound) nil)
                new-cset (cs-gen-Function V (merge X (zipmap vars (repeat (Y dbound))) X) Y S new-t-arr)]
            (move-vars+rest-to-dmap new-cset dbound vars))))

:else 
(throw (IllegalArgumentException. (pr-str "NYI Function inference " (unparse-type S) (unparse-type T)))))))

(defmethod cs-gen* [Function Function]
  [V X Y S T]
  (cs-gen-Function V X Y S T))

;; C : cset? - set of constraints found by the inference engine
;; Y : (setof symbol?) - index variables that must have entries
;; R : Type? - result type into which we will be substituting
(defn subst-gen [C Y R]
  {:pre [(cset? C)
         ((set-c? symbol?) Y)
         (Type? R)]
   :post [((some-fn nil? substitution-c?) %)]}
  (let [var-hash (fv-variances R)
        idx-hash (idx-variances R)]
    (letfn [
            ;; v : Symbol - variable for which to check variance
            ;; h : (Hash F Variance) - hash to check variance in (either var or idx hash)
            ;; variable: Symbol - variable to use instead, if v was a temp var for idx extension
            (constraint->type [{{:keys [upper-bound lower-bound]} :bnds :keys [S X T] :as v} h & {:keys [variable]}]
              {:pre [(c? v)
                     (variance-map? h)
                     ((some-fn nil? symbol?) variable)]}
              (assert (subtype? S T) (type-error S T))
              (let [var (h (or variable X) :constant)
                    inferred (case var
                               (:constant :covariant) S
                               :contravariant T
                               :invariant S)]
                inferred))
            ;TODO implement generalize
            ;                  (let [gS (generalize S)]
            ;                    (if (subtype? gS T)
            ;                      gS
            ;                      S))

            ;; Since we don't add entries to the empty cset for index variables (since there is no
            ;; widest constraint, due to dcon-exacts), we must add substitutions here if no constraint
            ;; was found.  If we're at this point and had no other constraints, then adding the
            ;; equivalent of the constraint (dcon null (c Bot X Top)) is okay.
            (extend-idxs [S]
              {:pre [(substitution-c? S)]}
              (let [fi-R (fi R)] ;free indices in R
                ;; If the index variable v is not used in the type, then
                ;; we allow it to be replaced with the empty list of types;
                ;; otherwise we error, as we do not yet know what an appropriate
                ;; lower bound is.
                (letfn [(demote-check-free [v]
                          {:pre [(symbol? v)]}
                          (if (fi-R v)
                            (throw (Exception. "attempted to demote dotted variable"))
                            (->i-subst nil)))]
                  ;; absent-entries is false if there's an error in the substitution, otherwise
                  ;; it's a list of variables that don't appear in the substitution
                  (let [absent-entries
                        (reduce (fn [no-entry v]
                                  {:pre [(symbol? v)]}
                                  (let [entry (S v)]
                                    ;; Make sure we got a subst entry for an index var
                                    ;; (i.e. a list of types for the fixed portion
                                    ;;  and a type for the starred portion)
                                    (cond
                                      (false? no-entry) no-entry
                                      (not entry) (cons v no-entry)
                                      (or (i-subst? entry) 
                                          (i-subst-starred? entry)
                                          (i-subst-dotted? entry)) no-entry
                                      :else false)))
                                [] Y)]
                    (and absent-entries
                         (merge (into {}
                                      (for [missing absent-entries]
                                        (let [var (idx-hash missing :constant)]
                                          [missing
                                           (case var
                                             (:constant :covariant :invariant) (demote-check-free missing)
                                             :contravariant (->i-subst-starred nil (->Top)))])))
                                S))))))]

      (let [{cmap :fixed dmap* :dmap} (-> C :maps first)
            _ (assert (= 1 (count (:maps C))) "More than one constraint set found")
            dm (:map dmap*)
            subst (merge 
                    (into {}
                      (for [[k dc] dm]
                        (cond
                          (and (dcon? dc) (not (:rest dc)))
                          [k (->i-subst (doall
                                          (for [f (:fixed dc)]
                                            (constraint->type f idx-hash :variable k))))]
                          (and (dcon? dc) (:rest dc))
                          [k (->i-subst-starred (doall
                                                  (for [f (:fixed dc)]
                                                    (constraint->type f idx-hash :variable k)))
                                                (constraint->type (:rest dc) idx-hash))]
                          (dcon-exact? dc)
                          [k (->i-subst-starred (doall
                                                  (for [f (:fixed dc)]
                                                    (constraint->type f idx-hash :variable k)))
                                                (constraint->type (:rest dc) idx-hash))]
                          (dcon-dotted? dc)
                          [k (->i-subst-dotted (doall
                                                 (for [f (:fixed dc)]
                                                   (constraint->type f idx-hash :variable k)))
                                               (constraint->type (:dc dc) idx-hash :variable k)
                                               (:dbound dc))]
                          :else (throw (Exception. (prn-str "What is this? " dc))))))

                    (into {}
                      (for [[k v] cmap]
                        [k (->t-subst (constraint->type v var-hash)
                                      (:bnds v))])))
            ;check bounds
            _ (let [t-substs (into {} (filter (fn [[_ v]] (t-subst? v)) subst))
                    [names images] (let [s (seq t-substs)]
                                     [(map first s)
                                      (map (comp :type second) s)])]
                (doseq [[nme {inferred :type :keys [bnds]}] t-substs]
                  (let [lower-bound (substitute-many (:lower-bound bnds) images names)
                        upper-bound (substitute-many (:upper-bound bnds) images names)]
                    (assert (subtype? lower-bound upper-bound)
                            (str "Lower-bound " (unparse-type lower-bound)
                                 " is not below upper-bound " (unparse-type upper-bound)))
                    (assert (and (subtype? inferred upper-bound)
                                 (subtype? lower-bound inferred))
                            (str "Inferred type " (unparse-type inferred)
                                 " is not between bounds " (unparse-type lower-bound)
                                 " and " (unparse-type upper-bound))))))]
        ;; verify that we got all the important variables
        (and (every? identity
                     (for [v (fv R)]
                       (let [entry (subst v)]
                         (and entry (t-subst? entry)))))
             (extend-idxs subst))))))

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained
;; Y : the set of index variables to be constrained
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; expected-cset : a cset representing the expected type, to meet early and
;;  keep the number of constraints in check. (empty by default)
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(defn cs-gen-list [V X Y S T & {:keys [expected-cset] :or {expected-cset (empty-cset {} {})}}]
  {:pre [((set-c? symbol?) V)
         (every? (hash-c? symbol? Bounds?) [X Y])
         (every? Type? (concat S T))
         (seq S)
         (seq T)
         (cset? expected-cset)]
   :post [(cset? %)]}
;  (prn "cs-gen-list" 
;       V X Y
;       (map unparse-type S)
;       (map unparse-type T))
  (assert (= (count S) (count T))
          (pr-str "S:" (map unparse-type S)
                  "T:" (map unparse-type T)))
  (cset-meet*
    ;; We meet early to prune the csets to a reasonable size.
    ;; This weakens the inference a bit, but sometimes avoids
    ;; constraint explosion.
    (doall 
      (for [[s t] (map vector S T)]
        (let [c (cs-gen V X Y s t)]
          ;(prn "c" c)
          (cset-meet c expected-cset))))))

(declare sub-f sub-o sub-pe)

(derive ::substitute-dots fold-rhs-default)

(add-fold-case ::substitute-dots
               Function
               (fn [{:keys [dom rng rest drest kws] :as ftype} {{:keys [name sb images rimage]} :locals}]
                 (assert (not kws) "TODO substitute keyword args")
                 (if (and drest
                          (= name (:name drest)))
                   (->Function (concat (map sb dom)
                                       ;; We need to recur first, just to expand out any dotted usages of this.
                                       (let [expanded (sb (:pre-type drest))]
                                         ;(prn "expanded" (unparse-type expanded))
                                         (map (fn [img] (substitute img name expanded)) images)))
                               (sb rng)
                               rimage nil nil)
                   (->Function (map sb dom)
                               (sb rng)
                               (and rest (sb rest))
                               (and drest (->DottedPretype (sb (:pre-type drest))
                                                           (:name drest)))
                               nil))))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(defn substitute-dots [images rimage name target]
  {:pre [(every? AnyType? images)
         ((some-fn nil? AnyType?) rimage)
         (symbol? name)
         (AnyType? target)]}
  ;(prn "substitute-dots" (unparse-type target) name "->" (map unparse-type images))
  (letfn [(sb [t] (substitute-dots images rimage name t))]
    (if (or ((fi target) name)
            ((fv target) name))
      (fold-rhs ::substitute-dots 
                {:type-rec sb
                 :filter-rec (sub-f sb ::substitute-dots)
                 :locals {:name name
                          :sb sb
                          :images images
                          :rimage rimage}}
                target)
      target)))

(derive ::substitute-dotted fold-rhs-default)

(add-fold-case ::substitute-dotted
               F
               (fn [{name* :name :as t} {{:keys [name image]} :locals}]
                 (if (= name* name)
                   image
                   t)))

(add-fold-case ::substitute-dotted
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [sb name image]} :locals}]
                 (assert (not kws))
                 (->Function (map sb dom)
                             (sb rng)
                             (and rest (sb rest))
                             (and drest
                                  (->DottedPretype (substitute image (:name drest) (sb (:pretype drest)))
                                                   (if (= name (:name drest))
                                                     name
                                                     (:name drest))))
                             nil)))

;; implements curly brace substitution from the formalism
;; substitute-dotted : Type Name Name Type -> Type
(defn substitute-dotted [image image-bound name target]
  {:pre [(AnyType? image)
         (symbol? image-bound)
         (symbol? name)
         (AnyType? target)]
   :post [(AnyType? %)]}
  (letfn [(sb [t] (substitute-dotted image image-bound name t))]
    (if ((fi target) name)
      (fold-rhs ::substitute-dotted
                {:type-rec sb 
                 :filter-rec (sub-f sb ::substitute-dotted)
                 :locals {:name name
                          :sb sb
                          :image image}}
                target
                target))))


;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(defn infer-dots [X dotted-var dotted-bnd S T T-dotted R must-vars & {:keys [expected]}]
  {:pre [((hash-c? symbol? Bounds?) X)
         (symbol? dotted-var)
         (Bounds? dotted-bnd)
         (every? #(every? Type? %) [S T])
         (every? Type? [T-dotted R])
         ((set-c? symbol?) must-vars)
         ((some-fn nil? Type?) expected)]
   :post [(substitution-c? %)]}
  (let [[short-S rest-S] (split-at (count T) S)
;        _ (prn "short-S" (map unparse-type short-S))
;        _ (prn "rest-S" (map unparse-type rest-S))
        expected-cset (if expected
                        (cs-gen #{} X {dotted-var dotted-bnd} R expected)
                        (empty-cset {} {}))
;        _ (prn "expected-cset" expected-cset)
        cs-short (cs-gen-list #{} X {dotted-var dotted-bnd} short-S T
                              :expected-cset expected-cset)
        ;_ (prn "cs-short" cs-short)
        new-vars (var-store-take dotted-var T-dotted (count rest-S))
        new-Ts (doall
                 (for [v new-vars]
                   (let [target (substitute-dots (map make-F new-vars) nil dotted-var T-dotted)]
                     (prn "replace" v "with" dotted-var "in" (unparse-type target))
                     (substitute (make-F v) dotted-var target))))
        ;_ (prn "new-Ts" new-Ts)
        cs-dotted (cs-gen-list #{} (merge X (zipmap new-vars (repeat dotted-bnd))) {dotted-var dotted-bnd} rest-S new-Ts
                               :expected-cset expected-cset)
        ;_ (prn "cs-dotted" cs-dotted)
        cs-dotted (move-vars-to-dmap cs-dotted dotted-var new-vars)
        ;_ (prn "cs-dotted" cs-dotted)
        cs (cset-meet cs-short cs-dotted)
        ;_ (prn "cs" cs)
        ]
    (subst-gen (cset-meet cs expected-cset) #{dotted-var} R)))

;; like infer, but T-var is the vararg type:
(defn infer-vararg [X Y S T T-var R & [expected]]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])
         (every? #(every? Type? %) [S T])
         ((some-fn nil? Type?) T-var)
         (Type? R)
         ((some-fn nil? Type?) expected)]
   :post [((some-fn nil? substitution-c?) %)]}
  ;(prn "infer-vararg" "X:" X)
  (let [new-T (if T-var
                ;Pad out T
                (concat T (repeat (- (count S) (count T)) T-var))
                T)]
    (and (>= (count S) (count T))
         (infer X Y S new-T R expected))))

;; X : variables to infer
;; Y : indices to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : #f or the expected type
;; returns a substitution
;; if R is nil, we don't care about the substituion
;; just return a boolean result
(defn infer [X Y S T R & [expected]]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])
         (every? Type? S)
         (every? Type? T)
         ((some-fn nil? Type?) R)
         ((some-fn nil? Type?) expected)]
   :post [((some-fn nil? true? substitution-c?) %)]}
  (let [expected-cset (if expected
                        (cs-gen #{} X Y R expected)
                        (empty-cset {} {}))
        cs (cs-gen-list #{} X Y S T :expected-cset expected-cset)
        cs* (cset-meet cs expected-cset)]
    ;(prn "final cs" cs*)
    (if R
      (subst-gen cs* (set (keys Y)) R)
      true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(defn add-scopes [n t]
  "Wrap type in n Scopes"
  {:pre [(nat? n)
         (Type? t)]}
  (doall
    (last 
      (take (inc n) (iterate ->Scope t)))))

(defn remove-scopes 
  "Unwrap n Scopes"
  [n sc]
  {:pre [(nat? n)
         (or (zero? n)
             (Scope? sc))]
   :post [(or (Scope? %) (Type? %))]}
  (doall
    (last
      (take (inc n) (iterate (fn [t]
                               (assert (Scope? t) "Tried to remove too many Scopes")
                               (:body t))
                             sc)))))

(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (iterate dec (dec (count c))) c))

(derive ::abstract-many fold-rhs-default)

(add-fold-case ::abstract-many
               F
               (fn [{name* :name :as t} {{:keys [name count outer sb]} :locals}]
                 (if (= name name*)
                   (->B (+ count outer))
                   t)))

(add-fold-case ::abstract-many
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [name count outer sb]} :locals}]
                 (assert (not kws))
                 (->Function (map sb dom)
                             (sb rng)
                             (when rest (sb rest))
                             (when drest
                               (->DottedPretype (sb (:pre-type drest))
                                                (if (= (:name drest) name)
                                                  (+ count outer)
                                                  (:name drest))))
                             nil)))

(add-fold-case ::abstract-many
               Mu
               (fn [{:keys [scope]} {{:keys [name count type outer name-to]} :locals}]
                 (let [body (remove-scopes 1 scope)]
                   (->Mu (->Scope (name-to name count type (inc outer) body))))))

(add-fold-case ::abstract-many
               PolyDots
               (fn [{bbnds* :bbnds n :nbound body* :scope} {{:keys [name count type outer name-to]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(-> %
                                      (update-in [:upper-bound] rs)
                                      (update-in [:lower-bound] rs))
                                   bbnds*)
                       as #(add-scopes n (name-to name count type (+ n outer) %))]
                   (->PolyDots n 
                               (mapv #(-> %
                                        (update-in [:upper-bound] as)
                                        (update-in [:lower-bound] as))
                                     bbnds)
                               (as body)))))

(add-fold-case ::abstract-many
               Poly
               (fn [{bbnds* :bbnds n :nbound body* :scope} {{:keys [name count type outer name-to]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(-> %
                                      (update-in [:upper-bound] rs)
                                      (update-in [:lower-bound] rs))
                                   bbnds*)
                       as #(add-scopes n (name-to name count type (+ n outer) %))]
                   (->Poly n 
                           (mapv #(-> %
                                    (update-in [:upper-bound] as)
                                    (update-in [:lower-bound] as))
                                 bbnds)
                           (as body)))))

(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         (Type? ty)]}
  (letfn [(name-to 
            ([name count type] (name-to name count type 0 type))
            ([name count type outer ty]
             (letfn [(sb [t] (name-to name count type outer t))]
               (fold-rhs ::abstract-many
                 {:type-rec sb
                  :filter-rec (sub-f sb ::abstract-many)
                  :object-rec (sub-o sb ::abstract-many)
                  :locals {:name name
                           :count count
                           :outer outer
                           :sb sb
                           :name-to name-to}}
                 ty))))]
    (if (empty? names)
      ty
      (let [n (count names)]
        (loop [ty ty
               names names
               count (dec n)]
          (if (zero? count)
            (add-scopes n (name-to (first names) 0 ty))
            (recur (name-to (first names) count ty)
                   (next names)
                   (dec count))))))))

(derive ::instantiate-many fold-rhs-default)

(add-fold-case ::instantiate-many
               B
               (fn [{:keys [idx] :as t} {{:keys [count outer image sb]} :locals}]
                 (if (= (+ count outer) idx)
                   (->F image)
                   t)))

(add-fold-case ::instantiate-many
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [count outer image sb]} :locals}]
                 (assert (not kws))
                 (->Function (map sb dom)
                             (sb rng)
                             (when rest
                               (sb rest))
                             (when drest
                               (->DottedPretype (sb (:pre-type drest))
                                                (let [{:keys [name]} drest]
                                                  (assert (nat? name))
                                                  (if (= (+ count outer) name)
                                                    image
                                                    name))))
                             nil)))

(add-fold-case ::instantiate-many
               Mu
               (fn [{:keys [scope]} {{:keys [replace count outer image sb type]} :locals}]
                 (let [body (remove-scopes 1 scope)]
                   (->Mu (->Scope (replace image count type (inc outer) body))))))

(add-fold-case ::instantiate-many
               PolyDots
               (fn [{bbnds* :bbnds n :nbound body* :scope} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(-> %
                                      (update-in [:upper-bound] rs)
                                      (update-in [:lower-bound] rs))
                                   bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (->PolyDots n 
                               (mapv #(-> %
                                        (update-in [:upper-bound] as)
                                        (update-in [:lower-bound] as))
                                     bbnds)
                               (as body)))))

(add-fold-case ::instantiate-many
               Poly
               (fn [{bbnds* :bbnds n :nbound body* :scope} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(-> %
                                      (update-in [:upper-bound] rs)
                                      (update-in [:lower-bound] rs))
                                   bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (->Poly n 
                           (mapv #(-> %
                                    (update-in [:upper-bound] as)
                                    (update-in [:lower-bound] as))
                                 bbnds)
                           (as body)))))

(defn instantiate-many 
  "instantiate-many : List[Symbols] Scope^n -> Type
  Instantiate de Bruijn indices in sc to frees named by
  images, preserving upper/lower bounds"
  [images sc]
  {:pre [(every? symbol? images)
         (or (Scope? sc)
             (empty? images))]
   :post [(Type? %)]}
  (letfn [(replace 
            ([image count type] (replace image count type 0 type))
            ([image count type outer ty]
             (letfn [(sb [t] (replace image count type outer t))]
               (let [sf (sub-f sb ::instantiate-many)]
                 (fold-rhs ::instantiate-many
                   {:type-rec sb 
                    :filter-rec sf 
                    :object-rec (sub-o sb ::instantiate-many)
                    :locals {:count count
                             :outer outer
                             :image image
                             :sb sb
                             :type type
                             :replace replace}}
                   ty)))))]
    (if (empty? images)
      sc
      (let [n (count images)]
        (loop [ty (remove-scopes n sc)
               images images
               count (dec n)]
          (if (zero? count)
            (replace (first images) 0 ty)
            (recur (replace (first images) count ty)
                   (next images)
                   (dec count))))))))

(defn abstract [name ty]
  "Make free name bound"
  {:pre [(symbol? name)
         (Type? ty)]}
  (abstract-many [name] ty))

(defn instantiate [f sc]
  "Instantiate bound name to free"
  {:pre [(symbol? f)
         (Scope? sc)]}
  (instantiate-many [f] sc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(declare subtype)

(derive ::substitute fold-rhs-default)

(add-fold-case ::substitute
               F
               (fn [{name* :name :as f} {{:keys [name image]} :locals}]
                 (if (= name* name)
                   image
                   f)))

(defn substitute [image name target]
  {:pre [(AnyType? image)
         (symbol? name)
         (AnyType? target)]
   :post [(AnyType? %)]}
  (fold-rhs ::substitute
            {:locals {:name name
                      :image image}}
            target))

(defn substitute-many [target images names]
  (reduce (fn [t [im nme]] (substitute im nme t))
          target
          (map vector images names)))

(defn subst-all [s t]
  {:pre [(substitution-c? s)
         (AnyType? t)]
   :post [(AnyType? %)]}
  (reduce (fn [t [v r]]
            (cond
              (t-subst? r) (substitute (:type r) v t)
              (i-subst? r) (substitute-dots (:types r) nil v t)
              (i-subst-starred? r) (substitute-dots (:types r) (:starred r) v t)
              (and (i-subst-dotted? r)
                   (empty? (:types r))) (substitute-dotted (:dty r) (:name (:dbound r)) v t)
              (i-subst-dotted? r) (throw (Exception. "i-subst-dotted nyi"))
              :else (throw (Exception. "Other substitutions NYI"))))
          t s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted pre-type expansion

;tdr from Practical Variable-Arity Polymorphism paper
; Expand out dotted pretypes to fixed domain, using types bm, if (:name bound) = b
(defmulti trans-dots (fn [t b bm]
                       {:pre [(AnyType? t)
                              (symbol? b)
                              (every? Type? bm)]}
                       (class t)))

(defmethod trans-dots F [t b bm] t)
(defmethod trans-dots Value [t b bm] t)
(defmethod trans-dots RClass [t b bm] t)

(defmethod trans-dots Union
  [t b bm]
  (let [tfn #(trans-dots % b bm)]
    (apply Un (doall (map tfn (:types t))))))

(defmethod trans-dots Intersection
  [t b bm]
  (let [tfn #(trans-dots % b bm)]
    (apply In (doall (map tfn (:types t))))))

(defmethod trans-dots Function
  [t b bm]
  ;TODO how to handle filters?
;  (assert (NoFilter? (-> t :rng :fl)))
;  (assert (NoObject? (-> t :rng :o)))
  (let [tfn #(trans-dots % b bm)]
    (cond
      (:drest t)
      (let [{:keys [pre-type name]} (:drest t)]
        (assert (symbol? name))
        (if (= b name) ;identical bounds
          (let [dom (concat 
                        ;keep fixed domain
                        (doall (map tfn (:dom t)))
                        ;expand dotted type to fixed domain
                        (doall (map (fn [bk]
                                      {:post [(Type? %)]}
                                      ;replace free occurences of bound with bk
                                      (-> (substitute bk b pre-type)
                                        tfn))
                                    bm)))]
            (->Function dom
                        (update-in (:rng t) [:t] tfn)
                        nil
                        nil ;dotted pretype now expanded to fixed domain
                        nil))
          (-> t
            (update-in [:dom] #(doall (map tfn %)))
            (update-in [:rng :t] tfn)
            (update-in [:drest] (fn [drest]
                                  (when drest
                                    (-> drest
                                      (update-in [:pre-type] tfn)))))))) ;translate pre-type
      :else
      (-> t
        (update-in [:dom] #(doall (map tfn %)))
        (update-in [:rng] tfn)
        (update-in [:rest] #(when %
                              (tfn %)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys]
  {:pre [((some-fn Poly? PolyDots?) ptype)
         (every? Type? argtys)]}
  (cond
    (Poly? ptype)
    (let [_ (assert (= (:nbound ptype) (count argtys)) "Wrong number of arguments to instantiate polymorphic type")
          names (repeatedly (:nbound ptype) gensym)
          body (Poly-body* names ptype)]
      (substitute-many body argtys names))

    (PolyDots? ptype)
    (let [nrequired-types (dec (:nbound ptype))
          _ (assert (<= nrequired-types (count argtys)) "Insufficient arguments to instantiate dotted polymorphic type")
          names (repeatedly (:nbound ptype) gensym)
          body (PolyDots-body* names ptype)]
      (-> body
        ; expand dotted pre-types in body
        (trans-dots (last names) ;the bound
                    (drop (dec (:nbound ptype)) argtys)) ;the types to expand pre-type with
        ; substitute normal variables
        (substitute-many (take nrequired-types argtys) (butlast names))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

(def ^:dynamic *current-env* nil)

(defn type-error [s t]
  (throw (Exception. (str "Type Error"
                          (when *current-env*
                            (str ", " (:source *current-env*) ":" (:line *current-env*)))
                          " - "
                          (or (-> s meta :source-Name)
                              (with-out-str (pr (unparse-type s))))
                          " is not a subtype of: " 
                          (or (-> t meta :source-Name)
                              (with-out-str (pr (unparse-type t))))))))

;keeps track of currently seen subtype relations for recursive types.
;(Set [Type Type])
(def ^:dynamic *sub-current-seen* #{})

(declare subtypes*-varargs)

(defn subtypes-varargs?
  "True if argtys are under dom"
  [argtys dom rst]
  (try 
    (subtypes*-varargs #{} argtys dom rst)
    true
    (catch Exception e
      false)))


;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument (the current-seen subtypes), called by subtype
;subtype* shouldn't be called directly, is called by subtypeA*
;
; In short, only call subtype (or subtype?)

(defmulti subtype* (fn [s t] [(class s) (class t)]))

(defn subtype? [s t]
  (try 
    (subtype s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare subtypeA*)

(defn subtypeA*? [A s t]
  (try (subtypeA* A s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare supertype-of-one-arr)

(defn subtypeA* [A s t]
  (if (or (contains? A [s t])
          (= s t)
          (Top? t)
          (Bottom? s))
    A
    (binding [*sub-current-seen* (conj A [s t])]
      (cond
        (Name? s)
        (subtypeA* *sub-current-seen* (resolve-Name s) t)

        (Name? t)
        (subtypeA* *sub-current-seen* s (resolve-Name t))

        (App? s)
        (subtypeA* *sub-current-seen* (resolve-App s) t)

        (App? t)
        (subtypeA* *sub-current-seen* s (resolve-App t))

        (Union? s)
        (if (every? #(subtype? % t) (:types s))
          *sub-current-seen*
          (type-error s t))

        (Union? t)
        (if (some #(subtype? s %) (:types t))
          *sub-current-seen*
          (type-error s t))

        (and (Fn-Intersection? s)
             (Fn-Intersection? t))
        (loop [A* *sub-current-seen*
               arr2 (:types t)]
          (let [arr1 (:types s)]
            (if (empty? arr2) 
              A*
              (if-let [A (supertype-of-one-arr A* (first arr2) arr1)]
                (recur A (next arr2))
                (type-error s t)))))

        (and (Intersection? s)
             (Intersection? t))
        (if (every? (fn [s*]
                      (some #(subtype? s* %) (:types t)))
                    (:types s))
          *sub-current-seen*
          (type-error s t))

        (Intersection? s)
        (if (some #(subtype? % t) (:types s))
          *sub-current-seen*
          (type-error s t))

        (Intersection? t)
        (if (every? #(subtype? s %) (:types t))
          *sub-current-seen*
          (type-error s t))

        :else (subtype* s t)))))

(defn subtype [s t]
  (subtypeA* *sub-current-seen* s t))

(defn subtypes*-varargs [A0 argtys dom rst]
  (loop [dom dom
         argtys argtys
         A A0]
    (cond
      (and (empty? dom) (empty? argtys)) A
      (empty? argtys) (throw (Exception. (prn-str "Expected arguments: " (map unparse-type dom)
                                                  " Actual: "(map unparse-type argtys))))
      (and (empty? dom) rst)
      (if-let [A (subtypeA* A (first argtys) rst)]
        (recur dom (next argtys) A)
        (type-error (first argtys) rst))

      (empty? dom) (throw (Exception. (prn-str "Expected arguments: " (map unparse-type dom)
                                               " Actual: "(map unparse-type argtys))))
      :else
      (if-let [A (subtypeA* A0 (first argtys) (first dom))]
        (recur (next dom) (next argtys) A)
        (type-error (first argtys) (first dom))))))

;; simple co/contra-variance for ->
(defn arr-subtype-nofail [A0 s t]
  {:pre [(Function? s)
         (Function? t)]}
  (assert (not (some :kws [s t])))
  ;; top for functions is above everything
  (cond
    ;; top for functions is above everything
    (TopFunction? t) A0
    ;; the really simple case
    (and (not ((some-fn :rest :drest :kws) s))
         (not ((some-fn :rest :drest :kws) t)))
    (let [{s1 :dom s2 :rng} s
          {t1 :dom t2 :rng} t]
      (when-not (= (count s1)
                   (count t1))
        (type-error s t))
      (-> *sub-current-seen*
        ((fn [A0]
           (reduce (fn [A* [s t]]
                     (subtypeA* A* s t))
                   A0
                   (map vector t1 s1))))
        (subtypeA* (:rng s) (:rng t)))

      (and (:rest s1)
           (not ((some-fn :rest :drest) t1)))
      (-> *sub-current-seen*
        (subtypes*-varargs (:dom t) (:dom s) (:rest s))
        (subtypeA* (:rng s) (:rng t)))

      (and (not ((some-fn :rest :drest) s))
           (:rest t))
      (type-error s t)

      (and (:rest s)
           (:rest t))
      (-> *sub-current-seen*
        (subtypes*-varargs (:dom t) (:dom s) (:rest s))
        (subtypeA* (:rest t) (:rest s))
        (subtypeA* (:rng s) (:rng t)))

      ;; handle ... varargs when the bounds are the same
      (and (:drest s)
           (:drest t)
           (= (-> s :drest :name)
              (-> t :drest :name)))
      (-> *sub-current-seen*
        (subtypeA* (-> t :drest :pre-type) (-> s :drest :pre-type))
        ((fn [A0] 
           (reduce (fn [A* [s t]]
                     (subtypeA* A* s t))
                   A0 (map vector (:dom t) (:dom s)))))
        (subtypeA* (:rng s) (:rng t))))
    :else (type-error s t)))

(defn supertype-of-one-arr [A s ts]
  (some #(arr-subtype-nofail A % s) ts))

(defmethod subtype* [Protocol Type]
  [s t]
  (if (= (RClass-of (Class->symbol Object) nil) t)
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [Protocol Protocol]
  [{var1 :the-var variances* :variances poly1 :poly? :as s}
   {var2 :the-var poly2 :poly? :as t}]
  (if (and (= var1 var2)
           (every? (fn [[v l r]]
                     (case v
                       :covariant (subtypeA* *sub-current-seen* l r)
                       :contravariant (subtypeA* *sub-current-seen* r l)
                       :invariant (and (subtypeA* *sub-current-seen* l r)
                                       (subtypeA* *sub-current-seen* r l))))
                   (map vector variances* poly1 poly2)))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [Value AnyValue]
  [_ _]
  *sub-current-seen*)

(defmethod subtype* [PrimitiveArray Type]
  [_ t]
  (subtype (->PrimitiveArray Object -any -any) t))

;Not quite correct, datatypes have other implicit ancestors (?)
(defmethod subtype* [DataType Type]
  [{:keys [the-class] :as s} t]
  (if (some #(subtype? % t) (set/union #{(RClass-of (Class->symbol Object) nil)} 
                                       (or (@DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [Type DataType]
  [s {:keys [the-class] :as t}]
  (if (some #(subtype? s %) (set/union #{(RClass-of (Class->symbol Object) nil)} 
                                       (or (@DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [DataType DataType]
  [{cls1 :the-class poly1 :poly? :as s} 
   {cls2 :the-class poly2 :poly? :as t}]
  (if (and (= cls1 cls2)
           (every? (fn [[v l r]]
                     (case v
                       :covariant (subtypeA* *sub-current-seen* l r)
                       :contravariant (subtypeA* *sub-current-seen* r l)
                       :invariant (and (subtypeA* *sub-current-seen* l r)
                                       (subtypeA* *sub-current-seen* r l))))
                   (map vector (:variances s) poly1 poly2)))
    *sub-current-seen*
    (type-error s t)))

(defn- subtype-rclass
  [{variancesl :variances classl :the-class replacementsl :replacements :as s}
   {variancesr :variances classr :the-class replacementsr :replacements :as t}]
  (cond
    ;easy case
    (and (empty? variancesl)
         (empty? variancesr)
         (empty? replacementsl)
         (empty? replacementsr))
    (if (isa? classl classr)
      *sub-current-seen*
      (type-error s t))))

; (Cons Integer) <: (Seqable Integer)
; (ancestors (Seqable Integer)

(defmethod subtype* [Value RClass]
  [{val :val :as s} t]
  (cond
    (nil? val) (type-error s t)
    :else (let [cls (class val)]
            (subtype (RClass-of (Class->symbol cls) nil) t))))


(defn- subtype-RClass-common-base 
  [{polyl? :poly? lcls-sym :the-class :as s}
   {polyr? :poly? rcls-sym :the-class :as t}]
  (let [{variances :variances} s]
    (and (= lcls-sym rcls-sym)
         (or (and (empty? polyl?) (empty? polyr?))
             (and (seq polyl?)
                  (seq polyr?)
                  (every? identity
                          (doall (map #(case %1
                                         :covariant (subtype? %2 %3)
                                         :contravariant (subtype? %3 %2)
                                         (= %2 %3))
                                      variances
                                      polyl?
                                      polyr?))))))))

; Class -> Class
(def primitive-coersions
  {Integer/TYPE #{Short Integer Long}
   Boolean/TYPE #{Boolean}})

(defn coerse-RClass-primitive
  [s t]
  (let [scls (symbol->Class (:the-class s))
        tcls (symbol->Class (:the-class t))]
    (cond
      (.isPrimitive ^Class scls)
      ((primitive-coersions scls) tcls)

      (.isPrimitive ^Class tcls)
      ((primitive-coersions tcls) scls))))

(defmethod subtype* [RClass RClass]
  [{polyl? :poly? :as s}
   {polyr? :poly? :as t}]
  (let [scls (symbol->Class (:the-class s))
        tcls (symbol->Class (:the-class t))]
    (cond
      (or
        ; use java subclassing
        (and (empty? polyl?)
             (empty? polyr?)
             (empty? (:replacements s))
             (empty? (:replacements t))
             (isa? scls tcls))

        ;same base class
        (and (= scls tcls)
             (subtype-RClass-common-base s t))

        ;one is a primitive, coerse
        (and (or (.isPrimitive ^Class scls)
                 (.isPrimitive ^Class tcls))
             (coerse-RClass-primitive s t))

        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(and (= (:the-class t) (:the-class %))
                    (subtype-RClass-common-base % t))
              (RClass-supers* s)))
      *sub-current-seen*

      ;try each ancestor

      :else (type-error s t))))

(prefer-method subtype* 
               [Type Mu]
               [HeterogeneousMap Type])

(defmethod subtype* [HeterogeneousMap Type]
  [s t]
  (let [sk (apply Un (map first (:types s)))
        sv (apply Un (map second (:types s)))]
    (subtype (RClass-of (Class->symbol IPersistentMap) [sk sv])
             t)))

;every rtype entry must be in ltypes
;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
(defmethod subtype* [HeterogeneousMap HeterogeneousMap]
  [{ltypes :types :as s}
   {rtypes :types :as t}]
  (last (doall (map (fn [[k v]]
                      (subtype (ltypes k) v))
                    rtypes))))

(defmethod subtype* [HeterogeneousVector HeterogeneousVector]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

(defmethod subtype* [HeterogeneousVector Type]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (In (RClass-of (Class->symbol IPersistentVector) [ss])
                 (make-ExactCountRange (count (:types s))))
             t)))

(defmethod subtype* [HeterogeneousList HeterogeneousList]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

(defmethod subtype* [HeterogeneousList Type]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (RClass-of (Class->symbol PersistentList) [ss])
             t)))

(defmethod subtype* [HeterogeneousSeq HeterogeneousSeq]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

(defmethod subtype* [HeterogeneousSeq Type]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (RClass-of (Class->symbol ASeq) [ss])
             t)))

(defmethod subtype* [Mu Type]
  [s t]
  (let [s* (unfold s)]
    (subtype s* t)))

(defmethod subtype* [Type Mu]
  [s t]
  (let [t* (unfold t)]
    (subtype s t*)))

(defmethod subtype* [Poly Poly]
  [{n1 :nbound :as s}
   {n2 :nbound :as t}]
  (when-not (= n1 n2)
    (type-error s t))
  (let [names (repeatedly n1 gensym)
        b1 (Poly-body* names s)
        b2 (Poly-body* names t)]
    (subtype b1 b2)))

;subtype if t includes all of s. 
;tl <= sl, su <= tu
(defmethod subtype* [CountRange CountRange]
  [{supper :upper slower :lower :as s}
   {tupper :upper tlower :lower :as t}]
  (if (and (<= tlower slower)
           (if tupper
             (and supper (<= supper tupper))
             true))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* :default
  [s t]
  (if (Top? t)
    *sub-current-seen*
    (type-error s t)))

(defmacro sub [s t]
  `(subtype (parse-type '~s)
            (parse-type '~t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

(alter-class Seqable [[a :variance :covariant]])

(alter-class IMeta [[a :variance :covariant]])

(alter-class IPersistentCollection [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)})

(alter-class ISeq [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              IPersistentCollection (IPersistentCollection a)})

(alter-class ILookup [[a :variance :covariant]
                      [b :variance :covariant]])

(alter-class IPersistentSet [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class APersistentSet [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              IFn [Any -> (U a nil)]
              AFn [Any -> (U a nil)]
              IPersistentCollection (IPersistentCollection a)
              IPersistentSet (IPersistentSet a)})

(alter-class PersistentHashSet [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              APersistentSet (APersistentSet a)
              IFn [Any -> (U a nil)]
              AFn [Any -> (U a nil)]
              IPersistentSet (IPersistentSet a)
              IPersistentCollection (IPersistentCollection a)
              IMeta (IMeta Any)})

(alter-class Associative [[a :variance :covariant]
                          [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection Any)
              Seqable (Seqable Any)
              ILookup (ILookup a b)})

(alter-class IMapEntry [[a :variance :covariant]
                        [b :variance :covariant]])

(alter-class IPersistentMap [[a :variance :covariant]
                             [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection (IMapEntry a b))
              Seqable (Seqable (IMapEntry a b))
              ILookup (ILookup a b)
              Associative (Associative a b)})

(alter-class ASeq [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              ISeq (ISeq a)
              IMeta (IMeta Any)})

(alter-class IPersistentStack [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class IPersistentVector [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class APersistentVector [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentVector (IPersistentVector a)
              IFn [Number -> a]
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class PersistentVector [[a :variance :covariant]]
             :replace
             {APersistentVector (APersistentVector a)
              IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentVector (IPersistentVector a)
              IFn [Number -> a]
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class Cons [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              ASeq (ASeq a)
              Seqable (Seqable a)
              ISeq (ISeq a)
              IMeta (IMeta Any)})

(alter-class IPersistentList [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentStack (IPersistentStack a)})

(alter-class PersistentList [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              ASeq (ASeq a)
              Seqable (Seqable a)
              IPersistentList (IPersistentList a)
              ISeq (ISeq a)
              IPersistentStack (IPersistentStack a)
              IMeta (IMeta Any)})

(alter-class Symbol []
             :replace
             {IMeta (IMeta Any)})

(alter-class IDeref [[r :variance :covariant]])


(alter-class IRef [[w :variance :contravariant]
                   [r :variance :covariant]]
             :replace
             {IDeref (IDeref r)})

(alter-class IReference [[w :variance :contravariant]
                         [r :variance :covariant]]
       :replace
       {IMeta (IMeta Any)})

(alter-class AReference [[w :variance :contravariant]
                         [r :variance :covariant]]
             :replace
             {IMeta (IMeta Any)
              IReference (IReference w r)})

(alter-class ARef [[w :variance :contravariant]
                   [r :variance :covariant]]
             :replace
             {IRef (IRef w r)
              IMeta (IMeta Any)
              AReference (AReference w r)
              IDeref (IDeref r)
              IReference (IReference w r)})

(alter-class Atom [[w :variance :contravariant]
                   [r :variance :covariant]]
             :replace
             {IRef (IRef w r)
              IMeta (IMeta Any)
              AReference (AReference w r)
              ARef (ARef w r)
              IDeref (IDeref r)
              IReference (IReference w r)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

(ann clojure.core/*ns* Namespace)
(ann clojure.core/namespace [(U Symbol String Keyword) -> (U nil String)])
(ann clojure.core/ns-name [Namespace -> Symbol])
(ann clojure.core/name [Named -> String])
(ann clojure.core/in-ns [Symbol -> nil])
(ann clojure.core/import [(IPersistentCollection Symbol) -> nil])
(ann clojure.core/identity (All [x] [x -> x]))

(ann clojure.core/list (All [x] [x * -> (PersistentList x)]))
(ann clojure.core/vector (All [x] [x * -> (IPersistentVector x)]))

(ann clojure.core/not [Any -> boolean])
(ann clojure.core/constantly (All [x y] [x -> [y * -> x]]))

(ann clojure.core/take-while 
     (All [x] 
          [[x -> Any] (U nil (Seqable x)) -> (Seqable x)]))

(ann clojure.core/drop-while
     (All [x] 
          [[x -> Any] (U nil (Seqable x)) -> (Seqable x)]))

(ann clojure.core/disj
     (All [x]
          (Fn [(I (APersistentSet x) Sorted) Any Any * -> (I (APersistentSet x) Sorted)]
              [(APersistentSet x) Any Any * -> (APersistentSet x)]
              [(I (APersistentSet x) Sorted) Any Any * -> (I (IPersistentSet x) Sorted)]
              [(IPersistentSet x) Any Any * -> (IPersistentSet x)])))

(comment
  (aget my-array 0 1 2)
  (aget (aget my-array 0) 1 2)
  (aget (aget (aget my-array 0) 1) 2)

  (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
       (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
            (App [(Associative a b) c d -> (Associative (U a c) (U b d))]
                 (Associative Keyword Number)
                 :a 1)
            :b 2)
       :c 3)

  (assoc my-map :a 1 :b 2 :c 3)
  (assoc (assoc my-map :a 1) :b 2 :c 3)
  (assoc (assoc (assoc my-map :a 1) :b 2) :c 3)

  (ann clojure.core/aset
       (Label [rec]
              (All [w [v :< w] :dotted [b]]
                   [(Array w _) AnyInteger v -> v]
                   [(Array _ r) AnyInteger b ... b
                    :recur (rec r b ... b)])))

  (ann clojure.core/aget 
       (Label [rec]
              (All [x :dotted [b]] 
                   (Fn [(Array _ x) AnyInteger -> x]
                       [(Array _ x) AnyInteger b ... b
                        :recur 
                        (rec x b ... b)]))))

  (ann clojure.core/assoc 
       (Label [rec]
              (All [[h :< (HMap {})] x y [k :< (I AnyValue Keyword)] [e :< k] :dotted [b]]
                   [h k v -> (I h (HMap k v))]
                   [(Associative y x) y x -> (Associative y x)]
                   [h k v b ... b
                    :recur (rec (I h (HMap {k v})) b ... b)]
                   [(Associative y x) y x b ... b
                    :recur (rec (Associative y x) b ... b)]
                   )))

  (ann clojure.core/dissoc
       (Label [rec]
              (All [[m :< (Associative _ _)] :dotted [b]]
                   [nil Any * -> nil]
                   [m -> m]
                   [m k b ... b
                    :recur
                    (rec (I m (HMap {} :without [k])) b ... b)])))

  (update-in {:a {:b 1}} [:a :b] inc)
  (update-in 
    (update-in {:a {:b 1}} [:a] inc) 
    [:b] 
    inc)

  (ann clojure.core/update-in
       (FixedPoint
         (All [[x :< (U nil (Associative Any Any))] k [l :< k] v r e
               :dotted [a b]]
              (Fn [(HMap {l v}) (Vector* k) [v a ... a -> r] a ... a -> (I x (HMap {l r}))]
                  [(HMap {l r}) (Vector* k b ... b) [v a ... a -> e] a ... a
                   :recur
                   [r (Vector* b ... b) [v a ... a -> e] a ... a]]))))

  ;(ann clojure.core/get-in 
  ;     (Label [rec]
  ;       (All [[x :< (U nil (Associative Any Any))] k :dotted [b]]
  ;            (Fn [x (Vector*) -> x]
  ;                [x (Vector*) _ -> x]
  ;                [(U nil (Associative _ y) (Vector* k b ... b) a -> x
  ;                ;TODO
  ;                [(U nil (Associative Any y)) (Vector* k) -> (U nil x)]
  ;                    )))))

  (ann clojure.core/partial 
       (Label [rec]
              (All [x [a :< x] r :dotted [b c]]
                   (Fn [[x c ... c -> r] a -> [c ... c -> r]]
                       [[x c ... c -> r] a b ... b
                        :recur
                        (rec [c ... c -> r] b ... b)]))))

  ;                                [[y -> x] [b ... b -> y] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [b ... b -> z] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [k -> z] [b ... b -> k] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [b ... b -> l] -> [b ... b -> x]]
  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [m -> l] [b ... b -> m] -> [b ... b -> x]]
  )

(ann clojure.core/str [Any * -> String])
(ann clojure.core/prn-str [Any * -> String])

(ann clojure.core/print [Any * -> Any])
(ann clojure.core/println [Any * -> Any])
(ann clojure.core/pr [Any * -> Any])
(ann clojure.core/prn [Any * -> Any])


(ann clojure.core/atom (All [x] [x -> (Atom x x)]))
(ann clojure.core/deref (All [x] [(IDeref x) -> x]))
(ann clojure.core/reset! (All [w r]
                              [(Atom w r) w -> r]))

(ann clojure.core/swap! (All [w r b ...] 
                             [(Atom w r) [r b ... b -> w] b ... b -> r]))

(ann clojure.core/symbol
     (Fn [(U Symbol String) -> Symbol]
         [String String -> Symbol]))

(ann clojure.core/seq? (predicate (ISeq Any)))

(ann clojure.core/meta (All [x]
                            (Fn [(IMeta x) -> x]
                                [Any -> nil])))

(ann clojure.core/string? (predicate String))

(add-var-type 'clojure.core/class
              (Fn-Intersection
                (make-Function [-any]
                               (Un (RClass-of (Class->symbol Class) nil) -nil)
                               nil nil
                               :object (->Path [(->ClassPE)] 0))))

(add-var-type 'clojure.core/seq
              (let [x (make-F 'x)]
                (with-meta (Poly* [(:name x)]
                                  [no-bounds]
                                  (Fn-Intersection (make-Function [(Un (RClass-of (Class->symbol Seqable) [x])
                                                                       -nil)]
                                                                  (Un -nil (RClass-of (Class->symbol ASeq) [x]))
                                                                  nil nil
                                                                  :filter (-FS (-filter (make-CountRange 1) 0)
                                                                               (-or (-filter -nil 0)
                                                                                    (-filter (make-ExactCountRange 0) 0)))
                                                                  :object -empty))) 
                           {:actual-frees '[x]})))

(add-var-type 'clojure.core/empty?
              (let [x (make-F 'x)]
                (with-meta (Poly* [(:name x)]
                                  [no-bounds]
                                  (Fn-Intersection (make-Function [(Un (RClass-of (Class->symbol Seqable) [x]) -nil)]
                                                                  (Un -true -false)
                                                                  nil nil
                                                                  :filter (-FS (-or (-filter (make-ExactCountRange 0) 0)
                                                                                    (-filter -nil 0))
                                                                               (-filter (make-CountRange 1) 0))
                                                                  :object -empty)))
                           {:actual-frees '[x]})))

(ann clojure.core/map
     (All [c a b ...]
          [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (Seqable c)]))

(ann clojure.core/merge-with
     (All [k a b ...]
          [[a a -> a] (U nil (IPersistentMap k a)) ... b -> (IPersistentMap k a)]))

(ann clojure.core/reduce
     (All [a c d]
          (Fn 
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
            [(Fn [c c -> c] [-> c]) (U nil (Seqable c)) -> c]
            ;With accumulator
            ; empty coll, f not called, returns accumulator
            ; (reduce + 3 []) => 3
            [Any a (U nil (I (ExactCount 0) (Seqable Any))) -> a]
            ; default
            ; (reduce + 3 my-coll)
            [[a c -> a] a (U nil (Seqable c)) -> a])))

(ann clojure.core/first
     (All [x]
          (Fn [(U (I (Seqable x) (ExactCount 0)) nil) -> nil]
              [(I (Seqable x) (CountRange 1)) -> x]
              [(U (Seqable x) nil) -> (U nil x)])))

(ann clojure.core/rest
     (All [x]
          [(U (Seqable x) nil) -> (ISeq x)]))

(ann clojure.core/conj
     (All [x y]
          (Fn [(IPersistentVector x) x x * -> (IPersistentVector x)]
              [(IPersistentMap x y)
               (U nil (IMapEntry x y) (Vector* x y))
               (U nil (IMapEntry x y) (Vector* x y)) * -> (IPersistentMap x y)]
              [(IPersistentSet x) x x * -> (IPersistentSet x)]
              [(ISeq x) x x * -> (ASeq x)]
              [(IPersistentCollection Any) Any Any * -> (IPersistentCollection Any)])))

(ann clojure.core/find
     (All [x y]
          [(IPersistentMap x y) Any -> (U (Vector* x y) nil)]))

(ann clojure.core/get
     (All [x]
          (Fn [(IPersistentSet x) Any -> (U nil x)]
              [java.util.Map Any -> (U nil Any)]
              [String Any -> (U nil Character)]
              [nil Any -> nil]
              [(U nil (ILookup Any x)) Any -> (U nil x)])))

(ann clojure.core/= [Any Any * -> (U true false)])

(def-alias AnyInteger (U Integer Long clojure.lang.BigInt BigInteger Short Byte))

(ann clojure.core/integer? (predicate AnyInteger))
(ann clojure.core/number? (predicate Number))

(ann clojure.core/+ (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number]))
(ann clojure.core/- (Fn [AnyInteger AnyInteger * -> AnyInteger]
                        [Number Number * -> Number]))
(ann clojure.core/* (Fn [AnyInteger * -> AnyInteger]
                        [Number * -> Number]))
(ann clojure.core// [Number Number * -> Number])

(ann clojure.core/inc (Fn [AnyInteger * -> AnyInteger]
                          [Number * -> Number]))
(ann clojure.core/dec (Fn [AnyInteger * -> AnyInteger]
                          [Number * -> Number]))

(ann clojure.core/even? [AnyInteger -> boolean])
(ann clojure.core/odd? [AnyInteger -> boolean])

(override-method clojure.lang.Numbers/add (Fn [AnyInteger AnyInteger -> AnyInteger]
                                              [Number Number -> Number]))
(override-method clojure.lang.Numbers/inc (Fn [AnyInteger -> AnyInteger]
                                              [Number -> Number]))
(override-method clojure.lang.Numbers/dec (Fn [AnyInteger -> AnyInteger]
                                              [Number -> Number]))
(override-method clojure.lang.Numbers/minus (Fn [AnyInteger AnyInteger -> AnyInteger]
                                                [Number Number -> Number]))
(override-method clojure.lang.Numbers/multiply (Fn [AnyInteger AnyInteger -> AnyInteger]
                                                   [Number Number -> Number]))
(override-method clojure.lang.Numbers/divide [Number Number -> Number])

(override-method clojure.lang.Numbers/lt [Number Number -> boolean])
(override-method clojure.lang.Numbers/gt [Number Number -> boolean])

(let [t (Fn-Intersection
          (make-Function 
            [(Un -nil (RClass-of Seqable [-any]))]
            (parse-type 'AnyInteger)
            nil nil
            :object (->Path [(->CountPE)] 0)))]
  (add-var-type 'clojure.core/count t)
  (add-method-override 'clojure.lang.RT/count t))

(non-nil-return java.lang.Object/getClass #{0})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(defrecord TCResult [t fl o]
  "This record represents the result of typechecking an expression"
  [(Type? t)
   (FilterSet? fl)
   (RObject? o)])

(add-default-fold-case TCResult
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec))))

(declare ret-t ret-f ret-o)

(defn unparse-TCResult [r]
  [(unparse-type (ret-t r))
   (unparse-filter-set (ret-f r))
   (unparse-object (ret-o r))])

(defn ret
  "Convenience function for returning the type of an expression"
  ([t] (ret t (-FS -top -top) (->EmptyObject)))
  ([t f] (ret t f (->EmptyObject)))
  ([t f o]
   {:pre [(Type? t)
          (FilterSet? f)
          (RObject? o)]
    :post [(TCResult? %)]}
   (->TCResult t f o)))

(defn ret-t [r]
  {:pre [(TCResult? r)]
   :post [(Type? %)]}
  (:t r))

(defn ret-f [r]
  {:pre [(TCResult? r)]
   :post [(FilterSet? %)]}
  (:fl r))

(defn ret-o [r]
  {:pre [(TCResult? r)]
   :post [(RObject? %)]}
  (:o r))

(def expr-type ::expr-type)

(defmulti check (fn [expr & [expected]]
                  {:pre [((some-fn nil? TCResult?) expected)]}
                  (:op expr)))

(defn check-top-level [nsym form]
  (let [ast (analyze/analyze-form-in-ns nsym form)]
    (check ast)))

(defmacro tc-t [form]
  `(-> (check-top-level (symbol (ns-name *ns*))
                        '~form)
     expr-type))

(defmacro tc [form]
  `(-> (check-top-level (symbol (ns-name *ns*))
                        '~form)
     expr-type unparse-type))

(defmulti constant-type class)

(defmethod constant-type nil [_] -nil)
(defmethod constant-type Class [v] (->Value v))
(defmethod constant-type Symbol [v] (->Value v))
(defmethod constant-type Long [v] (->Value v))
(defmethod constant-type Double [v] (->Value v))
(defmethod constant-type Integer [v] (->Value v))
(defmethod constant-type java.math.BigDecimal [v] (->Value v))
(defmethod constant-type clojure.lang.BigInt [v] (->Value v))
(defmethod constant-type String [v] (->Value v))
(defmethod constant-type Character [v] (->Value v))
(defmethod constant-type clojure.lang.Keyword [v] (->Value v))
(defmethod constant-type Boolean [v] (if v -true -false))
(defmethod constant-type PersistentHashSet [v] (RClass-of (Class->symbol PersistentHashSet) [(apply Un (map constant-type v))]))

(defmethod constant-type IPersistentList
  [clist]
  (->HeterogeneousList (apply list (map constant-type clist))))

(defmethod constant-type IPersistentVector
  [cvec]
  (->HeterogeneousVector (mapv constant-type cvec)))

(defmethod constant-type IPersistentMap
  [cmap]
  (->HeterogeneousMap (into {} (map #(vector (constant-type (first %))
                                             (constant-type (second %)))
                                    cmap))))

(defn check-value
  [{:keys [val] :as expr} & [expected]]
  (let [actual-type (constant-type val)
        _ (when expected
            (subtype actual-type (ret-t expected)))]
    (assoc expr
           expr-type (if val
                       (ret actual-type
                            (-FS -top -bot)
                            -empty)
                       (ret actual-type
                            (-FS -bot -top)
                            -empty)))))

(defmethod check :constant [& args] (apply check-value args))
(defmethod check :number [& args] (apply check-value args))
(defmethod check :string [& args] (apply check-value args))
(defmethod check :keyword [& args] (apply check-value args))

(defmethod check :boolean
  [{:keys [val] :as expr} & [expected]]
  (assoc expr
         expr-type (if val
                     (ret -true
                          (-FS -top -bot))
                     (ret -false
                          (-FS -bot -top)))))

(defmethod check :nil 
  [expr & [expected]]
  (assoc expr
         expr-type (ret -nil (-FS -bot -top) -empty)))

(defmethod check :map
  [{:keys [keyvals] :as expr} & [expected]]
  (let [expected (ret-t expected)
        _ (when expected
            (assert (or (HeterogeneousMap? expected)
                        (and (Union? expected)
                             (every? HeterogeneousMap? (:types expected))))
                    (str "Expected HeterogeneousMap, found " (unparse-type expected))))
        actual (if (not expected)
                 (->HeterogeneousMap (apply hash-map (map (comp ret-t expr-type) (mapv check keyvals))))
                 (let [keyvals-map (apply hash-map keyvals)
                       ckeys-to-uvals (into {} (for [[k v] keyvals-map]
                                                 [(check k) v]))
                       actual-keys (set (map (comp ret-t expr-type) (keys ckeys-to-uvals)))
                       relevant-type (first
                                       (filter #(empty? (set/difference actual-keys (set (keys (:types %)))))
                                               (or (and (HeterogeneousMap? expected)
                                                        [expected])
                                                   (and (Union? expected)
                                                        (:types expected)))))
                       _ (assert (HeterogeneousMap? relevant-type)
                                 (str "Expected " (unparse-type expected)
                                      ", found HMap with keys: " 
                                      (map (comp unparse-type ret-t expr-type (keys ckeys-to-uvals)))))
                       _ (doseq [[ck uv] ckeys-to-uvals]
                           (let [kt (ret-t (expr-type ck))
                                 _ (assert (and (Value? kt)
                                                (keyword? (:val kt)))
                                           "Can only check keyword keys to maps")
                                 expected-val ((:types relevant-type) kt)
                                 _ (assert expected-val (str "Undeclared key " (unparse-type kt)))
                                 cval (check uv (ret expected-val))]
                             (assert (subtype (ret-t (expr-type cval)) expected-val))))]
                   expected))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :vector
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)]
    (assoc expr
           expr-type (ret (->HeterogeneousVector (mapv (comp ret-t expr-type) cargs))))))

(defmethod check :empty-expr 
  [{coll :coll :as expr} & [expected]]
  (assoc expr
         expr-type (ret (constant-type coll)
                        (-FS -top -bot)
                        (->EmptyObject))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** START PORT **
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION INFERENCE START

;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [((some-fn TCResult? Type?) tr1)
         ((some-fn TCResult? Type?) expected)]
   :post [((some-fn TCResult? Type?) %)]}
  (letfn [(filter-better? [{f1+ :then f1- :else :as f1}
                           {f2+ :then f2- :else :as f2}]
            {:pre [(Filter? f1)
                   (Filter? f2)]
             :post [(boolean? %)]}
            (cond
              (= f1 f2) true
              (and (implied-atomic? f2+ f1+)
                   (implied-atomic? f2- f1-)) true
              :else false))
          (object-better? [o1 o2]
            {:pre [(RObject? o1)
                   (RObject? o2)]
             :post [(boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn NoObject? EmptyObject?) o2) true
              :else false))]
    ;tr1 = arg
    ;expected = dom
    ; Omitted some cases dealing with multiple return values
    (cond
      (and (TCResult? tr1)
           (TCResult? expected)
           (= (Un) (:t tr1))
           (NoFilter? (:fl expected))
           (NoObject? (:o expected)))
      (let [ts2 (:t tr1)]
        (ret ts2))

      (and (TCResult? tr1)
           (= (Un) (:t tr1)))
      expected

      (and (TCResult? tr1)
           (TCResult? expected)
           (= (-FS -top -top)
              (:fl expected))
           (EmptyObject? (:o expected)))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t} expected]
        (when-not (subtype? t1 t2)
          (type-error t1 t2))
        expected)

      (and (TCResult? tr1)
           (TCResult? expected))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t f2 :fl o2 :o} expected]
        (cond
          (not (subtype? t1 t2)) (type-error t1 t2)

          (and (not (filter-better? f1 f2))
               (object-better? o1 o2))
          (throw (Exception. (str "Expected result with filter " f2 ", got filter"  f1)))

          (and (filter-better? f1 f2)
               (not (object-better? o1 o2)))
          (throw (Exception. (str "Expected result with object " o2 ", got object"  o1)))

          (and (not (filter-better? f1 f2))
               (not (object-better? o1 o2)))
          (throw (Exception. (str "Expected result with object " o2 ", got object"  o1 " and filter "
                                  f2 " got filter " f1))))
        expected)

      (and (TCResult? tr1)
           (Type? expected))
      (let [{t1 :t f :fl o :o} tr1
            t2 expected]
        (when-not (subtype? t1 t2)
          (type-error t1 t2))
        (ret t2 f o))

      ;; erm.. ? What is (FilterSet: (list) (list))
      ;; TODO this case goes here, but not sure what it means 
      ;
      ;[((? Type? t1) (tc-result1: t2 (FilterSet: (list) (list)) (Empty:)))
      ; (unless (subtype t1 t2)
      ;   (tc-error/expr "Expected ~a, but got ~a" t2 t1))
      ; t1]

      (and (Type? tr1)
           (TCResult? expected))
      (let [t1 tr1
            {t2 :t f :fl o :o} expected]
        (if (subtype? t1 t2)
          (throw (Exception. (str "Expected result with filter " f " and " o ", got " t1)))
          (type-error t1 t2))
        t1)

      (and (Type? tr1)
           (Type? expected))
      (let [t1 tr1
            t2 expected]
        (when-not (subtype? t1 t2)
          (type-error t1 t2))
        expected)

      :else (let [a tr1
                  b expected]
              (throw (Exception. (str "Unexpected input for check-below " a b)))))))

(derive ::free-in-for-object fold-rhs-default)

(add-fold-case ::free-in-for-object
               Path
               (fn [{p :path i :id :as o} {{:keys [free-in? k]} :locals}]
                 (if (= i k)
                   (reset! free-in? true)
                   o)))

(derive ::free-in-for-filter fold-rhs-default)

(add-fold-case ::free-in-for-filter
               NotTypeFilter
               (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                 (if (= i k)
                   (reset! free-in? true)
                   t)))

(add-fold-case ::free-in-for-filter
               TypeFilter
               (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                 (if (= i k)
                   (reset! free-in? true)
                   t)))

(derive ::free-in-for-type fold-rhs-default)

(declare index-free-in?)

(add-fold-case ::free-in-for-type
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [k free-in? for-type]} :locals}]
                 ;; here we have to increment the count for the domain, where the new bindings are in scope
                 (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (concat (:mandatory kws)
                                                                                            (:optional kws))))
                       st* (fn [t] (index-free-in? (if (number? k) (+ arg-count k) k) t))]
                   (doseq [d dom]
                     (for-type d))
                   (st* rng)
                   (and rest (for-type rest))
                   (and rest (for-type (:pre-type drest)))
                   (doseq [[_ v] (concat (:mandatory kws)
                                         (:optional kws))]
                     (for-type v))
                   ;dummy return value
                   (make-Function [] -any))))

(defn index-free-in? [k type]
  (let [free-in? (atom false)]
    (letfn [(for-object [o]
              (fold-rhs ::free-in-for-object
                        {:type-rec for-type
                         :locals {:free-in? free-in?
                                  :k k}}
                        o))
            (for-filter [o]
              (fold-rhs ::free-in-for-filter
                        {:type-rec for-type
                         :filter-rec for-filter
                         :locals {:free-in? free-in?
                                  :k k}}
                         o))
            (for-type [t]
              (fold-rhs ::free-in-for-type
                        {:type-rec for-type
                         :filter-rec for-filter
                         :object-rec for-object
                         :locals {:free-in? free-in?
                                  :k k
                                  :for-type for-type}}
                        t))]
      (for-type type)
      @free-in?)))

(defn subst-filter [f k o polarity]
  {:pre [(Filter? f)
         (name-ref? k)
         (RObject? o)
         (boolean? polarity)]
   :post [(Filter? %)]}
  (letfn [(ap [f] (subst-filter f k o polarity))
          (tf-matcher [t p i k o polarity maker]
            {:pre [(Type? t)
                   ((some-fn EmptyObject? NoObject? Path?) o)]
             :post [(Filter? %)]}
            (cond
              ((some-fn EmptyObject? NoObject?)
                 o)
              (cond 
                (= i k) (if polarity -top -bot)
                (index-free-in? k t) (if polarity -top -bot)
                :else f)

              (Path? o) (let [{p* :path i* :id} o]
                          (cond
                            (= i k) (maker 
                                      (subst-type t k o polarity)
                                      i*
                                      (concat p p*))
                            (index-free-in? k t) (if polarity -top -bot)
                            :else f))
              :else (throw (Exception. (str "what is this? " o)))))]
    (cond
      (ImpFilter? f) (let [{ant :a consq :c} f]
                       (-imp (subst-filter ant k o (not polarity)) (ap consq)))
      (AndFilter? f) (let [fs (:fs f)] 
                       (apply -and (map ap fs)))
      (OrFilter? f) (let [fs (:fs f)]
                       (apply -or (map ap fs)))
      (BotFilter? f) -bot
      (TopFilter? f) -top

      (TypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity -filter))

      (NotTypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity -not-filter)))))

(defn subst-filter-set [fs k o polarity & [t]]
  {:pre [((some-fn FilterSet? NoFilter?) fs)
         (name-ref? k)
         (RObject? o)
         ((some-fn nil? Type?) t)]
   :post [(FilterSet? %)]}
  (let [extra-filter (if t (->TypeFilter t nil k) -top)]
    (letfn [(add-extra-filter [f]
              {:pre [(Filter? f)]
               :post [(Filter? %)]}
              (let [f* (-and extra-filter f)]
                (if (BotFilter? f*)
                  f*
                  f)))]
      (cond
        (FilterSet? fs) (-FS (subst-filter (add-extra-filter (:then fs)) k o polarity)
                             (subst-filter (add-extra-filter (:else fs)) k o polarity))
        :else (-FS -top -top)))))

(defn subst-object [t k o polarity]
  {:pre [(RObject? t)
         (name-ref? k)
         (RObject? o)
         (boolean? polarity)]
   :post [(RObject? %)]}
  (cond
    ((some-fn NoObject? EmptyObject?) t) t
    (Path? t) (let [{p :path i :id} t]
                (if (= i k)
                  (cond
                    (EmptyObject? o) (->EmptyObject)
                    ;; the result is not from an annotation, so it isn't a NoObject
                    (NoObject? o) (->EmptyObject)
                    (Path? o) (let [{p* :path i* :id} o]
                                (->Path (seq (concat p p*)) i*)))
                  t))))

(derive ::subst-type fold-rhs-default)

(add-fold-case ::subst-type
               Function
               (fn [{:keys [dom rng rest drest kws] :as ty} {{:keys [st k o polarity]} :locals}]
                 ;; here we have to increment the count for the domain, where the new bindings are in scope
                 (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (:mandatory kws)) (count (:optional kws)))
                       st* (if (integer? k)
                             (fn [t] 
                               {:pre [(AnyType? t)]}
                               (subst-type t (if (number? k) (+ arg-count k) k) o polarity))
                             st)]
                   (->Function (map st dom)
                               (st* rng)
                               (and rest (st rest))
                               (when drest
                                 (-> drest
                                   (update-in [:pre-type] st)))
                               (when kws
                                 (-> kws
                                   (update-in [:mandatory] #(into {} (for [[k v] %]
                                                                       [(st k) (st v)])))
                                   (update-in [:optional] #(into {} (for [[k v] %]
                                                                      [(st k) (st v)])))))))))


(defn subst-type [t k o polarity]
  {:pre [(AnyType? t)
         (name-ref? k)
         (RObject? o)
         ((some-fn true? false?) polarity)]
   :post [(AnyType? %)]}
  (letfn [(st [t*]
            (subst-type t* k o polarity))
          (sf [fs] 
            {:pre [(FilterSet? fs)] 
             :post [(FilterSet? %)]}
            (subst-filter-set fs k o polarity))]
    (fold-rhs ::subst-type
      {:type-rec st
       :filter-rec sf
       :object-rec (fn [f] (subst-object f k o polarity))
       :locals {:st st
                :k k
                :o o
                :polarity polarity}}
      t)))

(defn open-Result 
  "Substitute ids for objs in Result t"
  [{t :t fs :fl old-obj :o :as r} objs & [ts]]
  {:pre [(Result? r)
         (every? RObject? objs)
         ((some-fn FilterSet? NoFilter?) fs)
         (RObject? old-obj)
         ((some-fn nil? #(every? Type? %)) ts)]
   :post [(let [[t fs r] %]
            (and (Type? t)
                 (FilterSet? fs)
                 (RObject? r)))]}
  (reduce (fn [[t fs old-obj] [[o k] arg-ty]]
            {:pre [(Type? t)
                   ((some-fn FilterSet? NoFilter?) fs)
                   (RObject? old-obj)
                   (integer? k)
                   (RObject? o)
                   (or (false? arg-ty)
                       (Type? arg-ty))]
             :post [(let [[t fs r] %]
                      (and (Type? t)
                           (FilterSet? fs)
                           (RObject? r)))]}
            [(subst-type t k o true)
             (subst-filter-set fs k o true arg-ty)
             (subst-object old-obj k o true)])
          [t fs old-obj]
          (map vector 
               (map-indexed #(vector %2 %1) ;racket's is opposite..
                            objs)
               (if ts
                 ts
                 (repeat false)))))


;Function TCResult^n (or nil TCResult) -> TCResult
(defn check-funapp1 [{:keys [dom rng rest drest kws] :as ftype0} argtys expected & {:keys [check?] :or {check? true}}]
  {:pre [(Function? ftype0)
         (every? TCResult? argtys)
         ((some-fn nil? TCResult?) expected)
         (boolean? check?)]
   :post [(TCResult? %)]}
  (assert (not drest) "funapp with drest args NYI")
  (assert (empty? (:mandatory kws)) "funapp with mandatory keyword args NYI")
  ;checking
  (when check?
    (when (or (and (not rest) (not (= (count dom) (count argtys))))
              (and rest (< (count argtys) (count dom))))
      (throw (Exception. (str "Wrong number of arguments, expected " (count dom) " and got "(count argtys)))))
    (doseq [[arg-t dom-t] (map vector (map ret-t argtys) (concat dom (when rest (repeat rest))))]
      (check-below arg-t dom-t)))
  (let [dom-count (count dom)
        arg-count (+ dom-count (if rest 1 0) (count (:optional kws)))
        o-a (map :o argtys)
        _ (assert (every? RObject? o-a))
        t-a (map :t argtys)
        _ (assert (every? Type? t-a))
        [o-a t-a] (let [rs (for [[nm oa ta] (map vector 
                                                 (range arg-count) 
                                                 (concat o-a (repeatedly ->EmptyObject))
                                                 (concat t-a (repeatedly Un)))]
                             [(if (>= nm dom-count) (->EmptyObject) oa)
                              ta])]
                    [(map first rs) (map second rs)])
        [t-r f-r o-r] (open-Result rng o-a t-a)]
    (ret t-r f-r o-r)))

; TCResult TCResult^n (U nil TCResult) -> TCResult
(defn check-funapp [fexpr-ret-type arg-ret-types expected]
  {:pre [(TCResult? fexpr-ret-type)
         (every? TCResult? arg-ret-types)
         ((some-fn nil? TCResult?) expected)]
   :post [(TCResult? %)]}
  (let [fexpr-type (ret-t fexpr-ret-type)
        arg-types (doall (map ret-t arg-ret-types))]
    (prn "check-funapp" (unparse-type fexpr-type) (map unparse-type arg-types))
    (cond
      ;ordinary Function, single case, special cased for improved error msgs
      (and (Fn-Intersection? fexpr-type)
           (= 1 (count (:types fexpr-type))))
      (let [argtys arg-ret-types
            {[t] :types} fexpr-type]
        (check-funapp1 t argtys expected))

      ;ordinary Function, multiple cases
      (Fn-Intersection? fexpr-type)
      (let [ftypes (:types fexpr-type)
            success-ret-type (some #(check-funapp1 % arg-ret-types expected :check? false)
                                   (filter (fn [{:keys [dom rest] :as f}]
                                             {:pre [(Function? f)]}
                                             (subtypes-varargs? arg-types dom rest))
                                           ftypes))]
        (if success-ret-type
          success-ret-type
          (throw (Exception. "Arguments did not match function"))))

      ;ordinary polymorphic function without dotted rest
      (and (Poly? fexpr-type)
           (let [body (Poly-body* (repeatedly (:nbound fexpr-type) gensym) fexpr-type)]
             (and (Fn-Intersection? body)
                  (every? (complement :drest) (:types body)))))
      (let [fs-names (repeatedly (:nbound fexpr-type) gensym)
            _ (assert (every? symbol? fs-names))
            body (Poly-body* fs-names fexpr-type)
            bbnds (Poly-bbnds* fs-names fexpr-type)
            _ (assert (Fn-Intersection? body))
            ret-type (loop [[{:keys [dom rng rest drest kws] :as ftype} & ftypes] (:types body)]
                       (when ftype
                         ;; only try inference if argument types are appropriate and no kws
                         (if-let [substitution (try
                                                 (and (not (or drest kws))
                                                      ((if rest <= =) (count dom) (count arg-types))
                                                      (infer-vararg (zipmap fs-names bbnds) {} arg-types dom rest (Result-type* rng)
                                                                    (and expected (ret-t expected))))
                                                 (catch IllegalArgumentException e
                                                   (throw e))
                                                 (catch Exception e))]
                           (do ;(prn "subst:" substitution)
                             (ret (subst-all substitution (Result-type* rng))))
                           (if (or rest drest kws)
                             (throw (Exception. "Cannot infer arguments to polymorphic functions with rest types"))
                             (recur ftypes)))))]
        (if ret-type
          ret-type
          (throw (Exception. (str (when *current-env*
                                    (str (:line *current-env*) ":"))
                                  "Could not infer result to polymorphic function")))))

      :else ;; any kind of dotted polymorphic function without mandatory keyword args
      (if-let [[pbody fixed-vars fixed-bnds dotted-var dotted-bnd]
               (and (PolyDots? fexpr-type)
                    (let [vars (vec (repeatedly (:nbound fexpr-type) gensym))
                          bbnds (PolyDots-bbnds* vars fexpr-type)
                          [fixed-bnds dotted-bnd] [(butlast bbnds) (last bbnds)]
                          [fixed-vars dotted-var] [(butlast vars) (last vars)]
                          pbody (PolyDots-body* vars fexpr-type)]
                      (and (Fn-Intersection? pbody)
                           (seq (:types pbody))
                           (not (some :kws (:types pbody)))
                           [pbody fixed-vars fixed-bnds dotted-var dotted-bnd])))]
        (let [inferred-rng (some identity
                                 (for [{:keys [dom rest drest rng] :as ftype} (:types pbody)
                                       ;only try inference if argument types match
                                       :when (cond
                                               rest (<= (count dom) (count arg-types))
                                               drest (and (<= (count dom) (count arg-types))
                                                          (= dotted-var (-> drest :name)))
                                               :else (= (count dom) (count arg-types)))]
                                   (do
                                     (prn "Inferring dotted fn" (unparse-type ftype))
                                     ;; Only try to infer the free vars of the rng (which includes the vars
                                     ;; in filters/objects).
                                     (let [substitution (cond
                                                          drest (infer-dots (zipmap fixed-vars fixed-bnds) dotted-var dotted-bnd
                                                                            arg-types dom (:pre-type drest) (Result-type* rng) (fv rng)
                                                                            :expected (and expected (ret-t expected)))
                                                          rest (infer-vararg (zipmap fixed-vars fixed-bnds) {dotted-var dotted-bnd}
                                                                             arg-types dom rest rng
                                                                             (and expected (ret-t expected)))
                                                          :else (infer (zipmap fixed-vars fixed-bnds) {dotted-var dotted-bnd} 
                                                                       arg-types dom (Result-type* rng)
                                                                       (and expected (ret-t expected))))
                                           _ (prn "substitution:" substitution)
                                           substituted-type (subst-all substitution ftype)
                                           _ (prn "substituted-type" (unparse-type substituted-type))
                                           _ (prn "args" (map unparse-type arg-types))]
                                       (or (and substitution
                                                (check-funapp1 substituted-type (map ret arg-types) expected :check? false))
                                           (throw (Exception. "Error applying dotted type")))))))]
          (prn "inferred-rng"inferred-rng)
          (if inferred-rng
            inferred-rng
            (throw (Exception. (pr-str "Could not apply dotted function " (unparse-type fexpr-type)
                                       " to arguments " (map unparse-type arg-types))))))

        (throw (Exception. (str (when *current-env*
                                  (str (:line *current-env*) ": "))
                             "Cannot invoke type: " (unparse-type fexpr-type))))))))

(defmethod check :var
  [{:keys [var] :as expr} & [expected]]
  (let [id (var->symbol var)]
    (assoc expr
           expr-type (ret (lookup-Var (var->symbol var))
                          (-FS -top -top)
                          -empty))))

(defmethod check :the-var
  [{:keys [var] :as expr} & [expected]]
  (assoc expr
         expr-type (ret (RClass-of (Class->symbol Var) nil)
                        (-FS -top -bot)
                        -empty)))

(defn tc-equiv [comparator & vs]
  {:pre [(every? TCResult? vs)]
   :post [(TCResult? %)]}
  (let [thn-fls (set (apply concat
                            (for [[{t1 :t fl1 :fl o1 :o}
                                   {t2 :t fl2 :fl o2 :o}]
                                  (comb/combinations vs 2)]
                              (concat (when (Path? o2)
                                        [(-filter t1 (:id o2) (:path o2))])
                                      (when (Path? o1)
                                        [(-filter t2 (:id o1) (:path o1))])))))
        els-fls (set (apply concat
                            (for [[{t1 :t fl1 :fl o1 :o}
                                   {t2 :t fl2 :fl o2 :o}]
                                  (comb/combinations vs 2)]
                              (concat (when (Path? o2)
                                        [(-not-filter t1 (:id o2) (:path o2))])
                                      (when (Path? o1)
                                        [(-not-filter t2 (:id o1) (:path o1))])))))]
  (ret (Un -false -true)
       (-FS (if (empty? thn-fls)
              -top
              (apply -and thn-fls))
            (if (empty? els-fls)
              -top
              (apply -or els-fls)))
       -empty)))

(defmulti invoke-special (fn [expr & args] (-> expr :fexpr :var)))
(defmulti invoke-apply (fn [expr & args] (-> expr :args first :var)))
(defmulti static-method-special (fn [{{:keys [declaring-class name]} :method} & args]
                                  (symbol (str declaring-class) (str name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups

(declare invoke-keyword)

(defn- extend-method-expected 
  "Returns the expected type with target-type intersected with the first argument"
  [target-type expected]
  {:pre [(Type? target-type)
         (Type? expected)]
   :post [(Type? %)]}
  (cond
    (Fn-Intersection? expected)
    (-> expected
      (update-in [:types] #(for [ftype %]
                             (do
                               (assert (<= 1 (count (:dom ftype))))
                               (-> ftype
                                 (update-in [:dom] (fn [dom] 
                                                     (update-in (vec dom) [0] (partial In target-type)))))))))

    (Poly? expected)
    (let [names (repeat (:nbound expected) gensym)
          body (Poly-body* names expected)
          body (extend-method-expected target-type body)]
      (Poly* names 
             (Poly-bbnds* names expected)
             body))

    (PolyDots? expected)
    (let [names (repeat (:nbound expected) gensym)
          body (PolyDots-body* names expected)
          body (extend-method-expected target-type body)]
      (PolyDots* names 
                 (PolyDots-bbnds* names expected)
                 body))
    :else (throw (Exception. (str "Expected Function type, found " (unparse-type expected))))))

(defmethod invoke-special #'clojure.core/extend
  [{[atype & protos] :args :as expr} & [expected]]
  (assert (and atype (even? (count protos))) "Wrong arguments to extend")
  (let [catype (check atype)
        target-type (ret-t (expr-type catype))
        _ (assert (and (Value? target-type)
                       (class? (:val target-type)))
                  (str "Must provide Class as first argument to extend, "
                       "got" (unparse-type target-type)))
        ; build expected types for each method map
        extends (into {}
                      (for [[prcl-expr mmap-expr] (apply hash-map protos)]
                        (let [protocol (do (assert (= :var (:op prcl-expr)) "Must reference protocol directly with var in extend")
                                         (resolve-protocol (var->symbol (:var prcl-expr))))
                              expected-mmap (make-HMap {}
                                                       ;get all combinations
                                                       (into {}
                                                             (for [[msym mtype] (:methods protocol)]
                                                               [(-val (keyword (name msym))) 
                                                                (extend-method-expected target-type mtype)])))]
                          [protocol [mmap-expr expected-mmap]])))
        _ (doseq [[protocol [mmap-expr expected-hmap]] extends]
            (check mmap-expr (ret expected-hmap)))]
    (assoc expr
           expr-type (ret -nil))))

;into-array
(defmethod invoke-special #'clojure.core/into-array
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (= 2 (count args))
          "Only 2 arg arity supported for into-array")
  (let [ccls-expr (check (first args))
        array-cls-type (-> ccls-expr expr-type ret-t)
        _ (assert (and (Value? array-cls-type)
                       (class? (:val array-cls-type)))
                  "Must provide literal Class as first argument to into-array")
        array-cls (:val array-cls-type)
        ccoll-expr (check (second args)
                          (ret (RClass-of (Class->symbol Seqable) [(Un -nil (RClass-of (Class->symbol array-cls) nil))])))
        coll-type (-> ccoll-expr expr-type ret-t)
        _ (assert (subtype? coll-type (RClass-of (Class->symbol Seqable) [(Un -nil (RClass-of (Class->symbol array-cls) nil))]))
                  (str "Cannot populate " array-cls " array with " (unparse-type coll-type)))]
    (assoc expr 
           expr-type (ret (->PrimitiveArray array-cls
                                            (Un -nil (RClass-of (Class->symbol array-cls) nil))
                                            (Un -nil (RClass-of (Class->symbol array-cls) nil)))))))

;not
(defmethod invoke-special #'clojure.core/not
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (= 1 (count args)) "Wrong number of args to clojure.core/not")
  (let [ctarget (check (first args))
        {fs+ :then fs- :else} (-> ctarget expr-type ret-f)]
    (assoc expr
           expr-type (ret (Un -true -false)
                          ;flip filters
                          (-FS fs- fs+)
                          -empty))))

;get
(defmethod invoke-special #'clojure.core/get
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (<= 2 (count args) 3) "Wrong number of args to clojure.core/get")
  (let [[target kw default] args
        kwr (expr-type (check kw))]
    (cond
      (Value? (ret-t kwr))
      (assoc expr
             expr-type (invoke-keyword kwr 
                                       (expr-type (check target)) 
                                       (when default
                                         (expr-type (check target))) 
                                       expected))
      
      :else (do ;(prn "Non-special 'get'")
              ::not-special))))

(defmethod static-method-special 'clojure.lang.RT/get
  [{:keys [args] :as expr} & [expected]]
  (assert (<= 2 (count args) 3) "Wrong number of args to clojure.core/get")
  (let [[target kw default] args
        kwr (expr-type (check kw))]
    (cond
      (Value? (ret-t kwr))
      (assoc expr
             expr-type (invoke-keyword kwr 
                                       (expr-type (check target)) 
                                       (when default
                                         (expr-type (check target))) 
                                       expected))
      
      :else ::not-special)))

(defmethod check :keyword-invoke
  [{:keys [kw target] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (assoc expr
         expr-type (invoke-keyword (expr-type (check kw))
                                   (expr-type (check target))
                                   nil 
                                   expected)))

(defn find-val-type [t k]
  {:pre [(Type? t)
         (Type? k)]
   :post [(Type? %)]}
  (let [t (-resolve t)]
    (cond
      (HeterogeneousMap? t) (if-let [v (get (:types t) k)]
                              v
                              (throw (Exception. (str "Map type " (unparse-type t)
                                                      " does not have entry "
                                                      (unparse-type k)))))

      (Intersection? t) (apply In 
                               (for [t* (:types t)]
                                 (find-val-type t* k)))
      (Union? t) (apply Un
                        (for [t* (:types t)]
                          (find-val-type t* k)))
      :else (throw (Exception. (str "Can't get key " (unparse-type k) 
                                    "  from type " (unparse-type t)))))))

(defn invoke-keyword [kw-ret target-ret default-ret expected-ret]
  {:pre [(TCResult? kw-ret)
         (TCResult? target-ret)
         ((some-fn nil? TCResult?) default-ret)
         ((some-fn nil? TCResult?) expected-ret)]
   :post [(TCResult? %)]}
  (let [targett (-resolve (ret-t target-ret))
        kwt (ret-t kw-ret)]
    (cond
      ;Keyword must be a singleton with no default
      (and (Value? kwt)
           (keyword? (:val kwt))
           (not default-ret))
      (let [{{path-hm :path id-hm :id :as o} :o} target-ret
            this-pelem (->KeyPE (:val kwt))
            val-type (find-val-type targett kwt)]
        (if (not= (Un) val-type)
          (ret val-type
               (-FS (if (Path? o)
                      (-filter val-type id-hm (concat path-hm [this-pelem]))
                      (-filter-at val-type (->EmptyObject)))
                    (if (and (not (subtype? -false val-type))
                             (not (subtype? -nil val-type)))
                      -bot
                      -top))
               (if (Path? o)
                 (update-in o [:path] #(seq (concat % [this-pelem])))
                 o))
          (throw (Exception. "Keyword lookup gave bottom type"))))

      :else (throw (Exception. "keyword-invoke only supports keyword lookup, no default")))))

;=
(defmethod invoke-special #'clojure.core/= 
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;identical
(defmethod static-method-special 'clojure.lang.Util/identical
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;equiv
(defmethod static-method-special 'clojure.lang.Util/equiv
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;apply
(defmethod invoke-special #'clojure.core/apply
  [expr & [expected]]
  (prn "special apply:")
  (let [e (invoke-apply expr expected)]
    (when (= e ::not-special)
      (throw (Exception. (str "apply must be special:" (-> expr :args first :var)))))
    e))

;manual instantiation
(defmethod invoke-special #'inst-poly
  [{[pexpr targs-exprs] :args :as expr} & [expected]]
  (let [ptype (-> (check pexpr) expr-type ret-t)
        _ (assert ((some-fn Poly? PolyDots?) ptype))
        targs (doall (map parse-type (:val targs-exprs)))]
    (assoc expr
           expr-type (ret (manual-inst ptype targs)))))

(declare check-anon-fn)

;debug printing
(defmethod invoke-special #'tc-pr-env
  [{[debug-string] :args :as expr} & [expected]]
  (assert (= :string (:op debug-string)))
  ;DO NOT REMOVE
  (pr (:val debug-string))
  (print-env)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (ret -nil -false-filter -empty)))

;filter printing
(defmethod invoke-special #'tc-pr-filters
  [{[debug-string form] :args :as expr} & [expected]]
  (assert (and debug-string form) "Wrong arguments to tc-pr-filters")
  (let [cform (check form expected)
        t (expr-type cform)]
    (assert (= :string (:op debug-string)))
    ;DO NOT REMOVE
    (prn (:val debug-string))
    (prn (:fl t))
    (if (FilterSet? (:fl t))
      (do (pprint (unparse-filter-set (:fl t)))
        (flush))
      (prn (:fl t)))
    (prn (unparse-object (:o t)))
    ;DO NOT REMOVE
    (assoc expr
           expr-type t)))

;form annotation
(defmethod invoke-special #'ann-form*
  [{[frm {tsyn :val}] :args :as expr} & [expected]]
  (let [ty (parse-type tsyn)
        cty (check frm (ret ty))
        checked-type (expr-type cty)
        _ (assert (subtype (ret-t checked-type) ty))
        _ (when expected
            (assert (subtype (ret-t checked-type) (ret-t expected))))]
    (assoc expr
           expr-type (ret ty))))

;fn literal
(defmethod invoke-special #'fn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[fexpr {method-doms-syn :val}] args
        method-param-types (doall (map #(doall (map parse-type %)) method-doms-syn))]
    (check-anon-fn fexpr method-param-types)))

;polymorphic fn literal
(defmethod invoke-special #'pfn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[fexpr {poly-decl :val} {methods-params-syns :val}] args
        frees-with-bounds (map parse-free poly-decl)
        method-params-types (with-frees (map (comp make-F first) frees-with-bounds)
                              (doall (map #(doall (map parse-type %)) methods-params-syns)))
        cexpr (-> (check-anon-fn fexpr method-params-types)
                (update-in [expr-type :t] (fn [fin] (with-meta (Poly* (map first frees-with-bounds) 
                                                                      (map second frees-with-bounds)
                                                                      fin)
                                                               {:actual-frees (map first frees-with-bounds)}))))]
    cexpr))

(declare check-let)

(def ^:dynamic *loop-bnd-anns* nil)
(set-validator! #'*loop-bnd-anns* #(or (nil? %)
                                       (every? Type? %)))

;loop
(defmethod invoke-special #'loop>-ann
  [{:keys [args env] :as expr} & [expected]]
  (let [[expr {expected-bnds-syn :val}] args
        expected-bnds (binding [*ns* (or (-> env :ns :name find-ns)
                                         *ns*)]
                        (doall (map parse-type expected-bnds-syn)))]
    ;loop may be nested, type the first loop found
    (binding [*loop-bnd-anns* expected-bnds]
      (check expr expected))))

;don't type check
(defmethod invoke-special #'tc-ignore-forms*
  [{:keys [fexpr args] :as expr} & [expected]]
  (assoc (first args)
         expr-type (ret (->Top))))

;seq
(defmethod invoke-special #'clojure.core/seq
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[ccoll] (doall (map check args))]
    (cond
      ((some-fn HeterogeneousVector? 
                HeterogeneousList? 
                HeterogeneousSeq?)
         (expr-type ccoll))
      (assoc expr
             expr-type (ret (if-let [ts (seq (:types (expr-type ccoll)))]
                              (->HeterogeneousSeq ts)
                              -nil)))
      :else ::not-special)))

;make vector
(defmethod invoke-special #'clojure.core/vector
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (ret (->HeterogeneousVector
                            (mapv expr-type cargs))))))

;make hash-map
(defmethod invoke-special #'clojure.core/hash-map
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (every? Value? (keys (apply hash-map (map expr-type cargs))))
      (assoc expr
             expr-type (ret (->HeterogeneousMap
                              (apply hash-map (map expr-type cargs)))))
      :else ::not-special)))

;apply hash-map
(defmethod invoke-apply #'clojure.core/hash-map
  [{[_ & args] :args :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (and ((some-fn HeterogeneousVector? HeterogeneousList? HeterogeneousSeq?) 
              (expr-type (last cargs)))
           ;; every key must be a Value
           (every? Value? (keys (apply hash-map (concat (map expr-type (butlast cargs))
                                                        (mapcat vector (:types (expr-type (last cargs)))))))))
      (assoc expr
             expr-type (ret (->HeterogeneousMap
                              (apply hash-map (concat (map expr-type (butlast cargs))
                                                      (mapcat vector (:types (expr-type (last cargs)))))))))
      :else ::not-special)))

;for map destructuring
(defmethod invoke-special #'clojure.core/seq?
  [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (= 1 (count args)) "Wrong number of args to seq?")
        cargs (doall (map check args))
        obj (-> (expr-type (first cargs)) ret-o)
        ;_ (prn "seq?: expr" (first args))
        targett (-resolve (ret-t (expr-type (first cargs))))
        tys (cond
                (Union? targett) (into #{}
                                       (apply concat
                                              (for [t (set (map -resolve (:types targett)))]
                                                (if (Union? t)
                                                  (map -resolve (:types t))
                                                  [t]))))
                :else #{targett})
        special? (every? (some-fn HeterogeneousSeq?
                                  HeterogeneousList?
                                  HeterogeneousVector?
                                  HeterogeneousMap?)
                         tys)
        ;_ (prn "specials:" (map unparse-type tys))
        sub? (when special?
               (subtype? targett
                         (RClass-of (Class->symbol ISeq) [-any])))]
    (cond
      (and special? sub?)
      (assoc expr
             expr-type (ret -true 
                            (-FS (-filter-at (RClass-of (Class->symbol ISeq) [-any]) obj)
                                 -bot)
                            -empty))

      (and special? (not sub?))
      (assoc expr
             expr-type (ret -false 
                            (-FS -bot
                                 (-not-filter-at (RClass-of (Class->symbol ISeq) [-any]) obj))
                            -empty))

      :else (do ;(prn "seq? not special")
              ;(prn (unparse-type targett))
              ::not-special))))
;nth
(defmethod static-method-special 'clojure.lang.RT/nth
  [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (<= 2 (count args) 3))
        [te ne de :as cargs] (doall (map check args))
        types (let [ts (-resolve (ret-t (expr-type te)))]
                (if (Union? ts)
                  (:types ts)
                  [ts]))
        num-t (ret-t (expr-type ne))
        default-t (when de
                    (ret-t (expr-type de)))]
    (cond
      (and (Value? num-t)
           (integer? (:val num-t))
           (every? (some-fn Nil?
                            HeterogeneousVector?
                            HeterogeneousList?
                            HeterogeneousSeq?)
                   types))
      (assoc expr
             expr-type (ret (apply Un
                                   (doall
                                     (for [t types]
                                       (let [res-t (cond
                                                     (Nil? t) (or default-t -nil)
                                                     :else (apply nth 
                                                                  (:types t)
                                                                  (:val num-t) 
                                                                  (when default-t
                                                                    [default-t])))]
                                         (if res-t
                                           res-t
                                           (throw (Exception. (str "Cannot get index " (:val num-t)
                                                                   " from type " (unparse-type t)))))))))
                            (let [nnth (:val num-t)
                                  target-o (ret-o (expr-type te))
                                  default-o (when de
                                              (ret-o (expr-type de)))
                                  ;; We handle filters for both arities of nth here, with and without default
                                  ;;
                                  ;;With default:
                                  ;; if this is a true value either:
                                  ;;  * target is nil or seq and default is true
                                  ;;  * target is seqable, default is false
                                  ;;    and target is at least (inc nnth) count
                                  default-fs+ (-or (-and (-filter-at (Un -nil (RClass-of (Class->symbol ISeq) [-any])) 
                                                                     target-o)
                                                         (-not-filter-at (Un -false -nil) 
                                                                         default-o))
                                                   (-and (-filter-at (In (RClass-of (Class->symbol Seqable) [-any])
                                                                         (make-CountRange (inc nnth)))
                                                                     target-o)
                                                         (-filter-at (Un -false -nil) 
                                                                     default-o)))
                                  ;;Without default:
                                  ;; if this is a true value: 
                                  ;;  * target is seqable of at least nnth count
                                  nodefault-fs+ (-filter-at (In (RClass-of (Class->symbol Seqable) [-any])
                                                                (make-CountRange (inc nnth)))
                                                            target-o)]
                              (-FS (if default-t
                                     default-fs+
                                     nodefault-fs+)
                                   ; not sure if there's anything worth encoding here
                                   -top))))
      :else ::not-special)))

;assoc
(defmethod invoke-special #'clojure.core/assoc
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [[target & keyvals] args

        _ (assert (<= 3 (count args))
                  (str "assoc accepts at least 3 arguments, found "
                       (count args)))
        _ (assert (even? (count keyvals))
                  "assoc accepts an even number of keyvals")

        targett (-resolve (-> target check expr-type ret-t))
        hmaps (cond
                (HeterogeneousMap? targett) #{targett}
                ;look for maps 2 unions
                (Union? targett) (into #{}
                                       (apply concat
                                              (for [t (set (map -resolve (:types targett)))]
                                                (if (Union? t)
                                                  (map -resolve (:types t))
                                                  [t]))))
                :else #{})

        _ (assert (and (seq hmaps)
                       (every? HeterogeneousMap? hmaps))
                  (str "Must assoc with a singular or union of Heterogeneous Maps "
                       (with-out-str (pr (unparse-type targett))
                         (pr (map unparse-type hmaps)))))

        ; we must already have an even number of keyvals if we've got this far
        ckeyvals (doall (map check keyvals))
        keypair-types (partition 2 (map (comp ret-t expr-type) ckeyvals))

        _ (assert (every? Value? (map first keypair-types))
                  (str "assoc keys must be values, found "
                       (map (comp unparse-type first) keypair-types)))

        new-hmaps (map #(reduce (fn [hmap [kt vt]]
                                  {:pre [(HeterogeneousMap? hmap)]}
                                  (assoc-in hmap [:types kt] vt))
                                % keypair-types)
                       hmaps)]
    (assoc expr
           expr-type (ret (apply Un new-hmaps)
                          (-FS -top -bot)
                          -empty))))


;conj
(defmethod invoke-special #'clojure.core/conj
  [{[t & args] :args :keys [fexpr] :as expr} & [expected]]
  (let [t (check t)
        args (doall (map check args))]
    (cond
      ;(conj {...} [a b]) => (merge {...} {a b})
      (and (HeterogeneousMap? (expr-type t))
           (HeterogeneousVector? (expr-type (first args))))
      (let [m (expr-type t)
            arg1 (expr-type (first args))
            _ (assert (= 2 (count (:types arg1)))
                      "Need vector of length 2 to conj to map")
            _ (assert (every? Value? (:types arg1))
                      "Vector must be of Values for now")
            res (->HeterogeneousMap
                  (assoc (:types m)
                         (-> arg1 :types first)
                         (-> arg1 :types second)))]
        (assoc expr
               expr-type (ret res)))

      ;(conj {...} nil) => {...}
      (and (HeterogeneousMap? (expr-type t))
           (Nil? (expr-type (first args))))
      (assoc expr
             expr-type (ret (expr-type t)))

      ;[...]
      (HeterogeneousVector? (expr-type t))
      (assoc expr
             expr-type (ret (->HeterogeneousVector
                              ;vectors conj onto end
                              (concat (:types (expr-type t)) 
                                      [(expr-type (first args))]))))

      :else ::not-special)))

(defmethod invoke-special :default [& args] ::not-special)
(defmethod static-method-special :default [& args] ::not-special)

(defn check-apply
  [{[fexpr & args] :args :as expr} expected]
  {:post [(TCResult? %)]}
  (let [ftype (ret-t (expr-type (check fexpr)))
        [fixed-args tail] [(butlast args) (last args)]]
    (cond
      ;apply of a simple function
      (Fn-Intersection? ftype)
      (do 
        (when (empty? (:types ftype))
          (throw (Exception. "Empty function intersection given as argument to apply")))
        (let [arg-tres (mapv check fixed-args)
              arg-tys (mapv (comp ret-t expr-type) arg-tres)
              tail-ty (ret-t (expr-type (check tail)))]
          (loop [[{:keys [dom rng rest drest]} :as fs] (:types ftype)]
            (cond
              ;we've run out of cases to try, so error out
              (empty? fs)
              (throw (Exception. (str "Bad arguments to function in apply: " 
                                      (unparse-type ftype) (mapv unparse-type (concat arg-tys [tail-ty])))))

              ;this case of the function type has a rest argument
              (and rest
                   ;; check that the tail expression is a subtype of the rest argument
                   (subtype? tail-ty (Un -nil (RClass-of Seqable [rest])))
                   (subtypes-varargs? arg-tys dom rest))
              (ret (Result-type* rng)
                   (Result-filter* rng)
                   (Result-object* rng))

              ;other cases go here

              ;next case
              :else (recur (next fs))))))

      ;; apply of a simple polymorphic function
      (Poly? ftype)
      (let [vars (repeatedly (:nbound ftype) gensym)
            bbnds (Poly-bbnds* vars ftype)
            body (Poly-body* vars ftype)
            _ (assert (Fn-Intersection? body))
            arg-tres (mapv check fixed-args)
            arg-tys (mapv (comp ret-t expr-type) arg-tres)
            tail-bound nil
            tail-ty (ret-t (expr-type (check tail)))]
        (loop [[{:keys [dom rng rest drest]} :as fs] (:types body)]
          (cond
            (empty? fs) (throw (Exception. "Bad arguments to polymorphic function in apply"))
            ;the actual work, when we have a * function and a list final argument
            :else 
            (if-let [substitution (and rest (not tail-bound) 
                                       (<= (count dom)
                                           (count arg-tys))
                                       (infer-vararg (zipmap vars bbnds) #{}
                                                     (cons tail-ty arg-tys)
                                                     (cons (RClass-of (Class->symbol Seqable) [rest])
                                                           dom)
                                                     rest
                                                     rng))]
              (ret (subst-all substitution rng))
              (recur (next fs))))))

      :else ::not-special)))

;convert apply to normal function application
(defmethod invoke-apply :default 
  [expr & [expected]]
  (let [t (check-apply expr expected)]
    (if (= t ::not-special)
      t
      (assoc expr
             expr-type t))))

(defmethod check :invoke
  [{:keys [fexpr args env] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (prn "invoke:" ((some-fn :var :keyword :op) fexpr))
  (binding [*current-env* env]
    (let [e (invoke-special expr expected)]
      (cond 
        (not= ::not-special e) e

        (let [fexprt (ret-t (expr-type (check fexpr)))]
          (and (Value? fexprt)
               (keyword? (:val fexprt))))
        (let [[target default] args]
          (assert (<= 1 (count args) 2))
          (assoc expr
                 expr-type (invoke-keyword (expr-type (check fexpr))
                                           (expr-type (check target))
                                           (when default
                                             (expr-type (check default))) 
                                           expected)))

        :else
        (let [cfexpr (check fexpr)
              cargs (doall (map check args))
              ftype (expr-type cfexpr)
              argtys (map expr-type cargs)
              actual (check-funapp ftype argtys expected)]
          (assoc expr
                 :fexpr cfexpr
                 :args cargs
                 expr-type actual))))))

;args :- [symbol Type]
;kws ?
(defrecord FnResult [args kws rest drest body]
  "Results of checking a fn method"
  [(every? symbol? (map first args))
   (every? Type? (map second args))
   (nil? kws)
   (or (nil? rest)
       (and (symbol? (first rest))
            (Type? (second rest))))
   (nil? drest)
   (TCResult? body)])

(defn relevant-Fns
  "Given a set of required-param exprs, rest-param expr, and a Fn-Intersection,
  returns an (ordered) seq of Functions that contains function types
  whos arities match the fixed and rest parameters given"
  [required-params rest-param fin]
  {:pre [(Fn-Intersection? fin)]
   :post [(every? Function? %)]}
  (assert (not (some :drest (:types fin))))
  (let [nreq (count required-params)]
    (letfn [(relevant-rest?
              [{:keys [dom rest drest] :as ftype}]
              "Returns a true value if ftype matches the
              number of required and variable parameters"
              (let [ndom (count dom)]
                (and (= ndom nreq)
                     rest)))
            (relevant-fixed?
              [{:keys [dom rest] :as ftype}]
              "Returns a true value if the ftype matches
              exactly the number of required parameters. 
              ie. has no rest parameters"
              (let [ndom (count dom)]
                (and (= ndom nreq)
                     (not rest))))]
      (let [relevant? (if rest-param
                        relevant-rest?
                        relevant-fixed?)]
        (filter relevant? (:types fin))))))

(declare check-fn-expr check-fn-method)

(defmethod check :fn-expr
  [{:keys [methods] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (check-fn-expr expr expected))

(declare check-anon-fn-method abstract-filter abo abstract-object)

(defn abstract-result [result arg-names]
  {:pre [(TCResult? result)
         (every? symbol? arg-names)]
   :post [(Result? %)]}
  (let [keys (range (count arg-names))]
    (make-Result
      (ret-t result)
      (abstract-filter arg-names keys (ret-f result))
      (abstract-object arg-names keys (ret-o result)))))

(defn abstract-object [ids keys o]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (RObject? o)]
   :post [(RObject? %)]}
  (letfn [(lookup [y]
            {:pre [(symbol? y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector ids keys)))]
    (cond
      (and (Path? o)
           (lookup (:id o))) (update-in o [:id] lookup)
      :else -empty)))

(defn abstract-filter [ids keys fs]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         ((some-fn NoFilter? FilterSet?) fs)]
   :post [((some-fn NoFilter? FilterSet?) %)]}
  (cond
    (FilterSet? fs)
    (let [{fs+ :then fs- :else} fs]
      (-FS (abo ids keys fs+)
           (abo ids keys fs-)))
    (NoFilter? fs) (-FS -top -top)))

(derive ::abo fold-rhs-default)

(add-fold-case ::abo
               TypeFilter
               (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                 ;if variable goes out of scope, replace filter with -top
                 (if (lookup id)
                   (-filter type (lookup id) path)
                   -top)))

(add-fold-case ::abo
               NotTypeFilter
               (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                 ;if variable goes out of scope, replace filter with -top
                 (if (lookup id)
                   (-not-filter type (lookup id)  path)
                   -top)))

(defn abo [xs idxs f]
  {:pre [(every? symbol? xs)
         (every? integer? idxs)
         (Filter? f)]
   :post [(Filter? %)]}
  (letfn [(lookup [y]
            {:pre [(symbol? y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector xs idxs)))
          (rec [f] (abo xs idxs f))
          (sb-t [t] t)]
    (fold-rhs ::abo
      {:type-rec sb-t 
       :filter-rec rec
       :locals {:lookup lookup}}
      f)))

(defn FnResult->Function [{:keys [args kws rest drest body] :as fres}]
  {:pre [(FnResult? fres)]
   :post [(Function? %)]}
  (assert (not (or kws rest drest)))
  (let [arg-names (concat (map first args)
                          (when rest
                            (first rest))
                          (when drest
                            (first drest))) ;TODO kws
                            ]
    (->Function
      (map second args)
      (abstract-result body arg-names)
      (when rest
        (second rest))
      (when drest
        (second drest))
      nil)))

(defn check-anon-fn
  "Check anonymous function, with annotated methods"
  [{:keys [methods] :as expr} methods-param-types]
  {:pre [(every? Type? (apply concat methods-param-types))]
   :post [(TCResult? (expr-type %))]}
  (let [ftype (apply Fn-Intersection (doall (map FnResult->Function 
                                                 (doall (map check-anon-fn-method methods methods-param-types)))))]
    (assoc expr
           expr-type (ret ftype (-FS -top -bot) -empty))))

(declare ^:dynamic *recur-target*)

(defn check-anon-fn-method
  [{:keys [required-params rest-param body] :as expr} method-param-types]
  {:pre [(every? Type? method-param-types)]
   :post [(FnResult? %)]}
  (assert (not rest-param))
  (let [syms (map :sym required-params)
        locals (zipmap syms method-param-types)
        ; update filters that reference bindings that the params shadow
        props (map (fn [oldp]
                     (reduce (fn [p sym]
                               {:pre [(Filter? p)
                                      (symbol? sym)]}
                               (subst-filter p sym -empty true))
                             oldp (keys locals)))
                   (:props *lexical-env*))
        env (-> *lexical-env*
              (assoc-in [:props] props)
              (update-in [:l] merge locals))
        ; erasing references to parameters is handled later
        cbody (with-lexical-env env
                (binding [*recur-target* nil] ;NYI
                  (check body)))]
    (->FnResult
      (map vector (map :sym required-params) method-param-types)
      nil ;kws
      nil ;rest
      nil ;drest
      (expr-type cbody))))

(defn check-fn-expr [{:keys [methods] :as expr} expected]
  (cond
    expected
    (let [fin (cond
                (Poly? (ret-t expected)) (Poly-body* (repeatedly (:nbound (ret-t expected)) gensym) (ret-t expected))
                :else (ret-t expected))
          _ (doseq [{:keys [required-params rest-param] :as method} methods]
              (check-fn-method method (relevant-Fns required-params rest-param fin)))]
      (assoc expr
             expr-type (ret fin (-FS -top -bot) -empty)))
    
    ;if no expected type, parse as anon fn with all parameters as Any
    :else (check-anon-fn expr (for [{:keys [required-params rest-param]} methods]
                                (do (assert (not rest-param))
                                  (repeatedly (count required-params) ->Top))))))

(defn check-fn-method
  "Checks type of the method"
  [{:keys [required-params rest-param body] :as expr} expected-fns]
  {:pre [(sequential? expected-fns)
         (seq expected-fns)
         (every? Function? expected-fns)]}
  #_(prn "check-fn-method:" body)
  (doseq [{:keys [dom rng rest drest] :as ftype} expected-fns]
    (assert (not drest))
    (let [param-locals (let [dom-local (zipmap (map :sym required-params) dom)
                             rest-local (when (or rest-param rest)
                                          (assert (and rest rest-param))
                                          [(:sym rest-param) (Un -nil (RClass-of (Class->symbol ASeq) [rest]))])]
                         (conj dom-local rest-local))
          props (map (fn [oldp]
                       (reduce (fn [p sym]
                                 {:pre [(Filter? p)
                                        (symbol? sym)]}
                                 (subst-filter p sym -empty true))
                               oldp (keys param-locals)))
                     (:props *lexical-env*))
          env (-> *lexical-env*
                (assoc-in [:props] props)
                (update-in [:l] merge param-locals))
          res-expr (with-lexical-env env
                     (binding [*recur-target* nil] ;NYI
                       (check body (ret (Result-type* rng)
                                        (Result-filter* rng)
                                        (Result-object* rng)))))
          res-type (-> res-expr expr-type ret-t)]
      (subtype res-type (Result-type* rng)))))

;; FUNCTION INFERENCE END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** END PORT **
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check :do
  [{:keys [exprs] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (let [cexprs (concat (mapv check (butlast exprs))
                       [(check (last exprs) expected)])]
    (assoc expr
           :exprs cexprs
           expr-type (-> cexprs last expr-type)))) ;should be a ret already

(defmethod check :local-binding-expr
  [{:keys [local-binding] :as expr} & [expected]]
  (let [sym (:sym local-binding)]
    (assoc expr
           expr-type (let [t (type-of sym)]
                       (ret t 
                            (-FS (if (subtype? t (Un -false -nil))
                                   -bot
                                   (-not-filter (Un -nil -false) sym))
                                 (-filter (Un -nil -false) sym))
                            (->Path nil sym))))))


(declare Method-symbol->Type)

(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

(defn symbol->PArray [sym nilable?]
  (let [s (str sym)]
    (when (.endsWith s "<>")
      (let [s-nosuffix (apply str (drop-last 2 s))]
        (assert (not (.contains s-nosuffix "<>")))
        ;Nullable elements
        (let [t (Method-symbol->Type (symbol s-nosuffix) nilable?)]
          (->PrimitiveArray t t))))))

(defn Method-symbol->Type [sym nilable?]
  {:pre [(symbol? sym)]
   :post [(Type? %)]}
  (if-let [typ (or (primitives sym)
                   (symbol->PArray sym nilable?)
                   (when-let [cls (resolve sym)]
                     (apply Un (RClass-of (Class->symbol cls) nil)
                            (when nilable?
                              [-nil]))))]
    typ
    (throw (Exception. (str "Method symbol " sym " does not resolve to a type")))))

(defn- instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(Fn-Intersection? %)]}
  (assert (resolve declaring-class))
  (Fn-Intersection (make-Function (concat [(RClass-of declaring-class nil)]
                                          (doall (map #(Method-symbol->Type % false) parameter-types)))
                                  (Method-symbol->Type return-type true))))

(defn- method->Function [{:keys [parameter-types return-type flags] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(Fn-Intersection? %)]}
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (Fn-Intersection (make-Function (doall (map (fn [[n tsym]] (Method-symbol->Type 
                                                                 tsym (nilable-param? msym nparams n)))
                                                (map-indexed vector
                                                             (if (:varargs flags)
                                                               (butlast parameter-types)
                                                               parameter-types))))
                                    (Method-symbol->Type return-type (not (nonnilable-return? msym nparams)))
                                    (when (:varargs flags)
                                      (Method-symbol->Type (last parameter-types) (nilable-param? msym nparams (dec nparams))))))))

(defn- Constructor->Function [{:keys [declaring-class parameter-types] :as ctor}]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(Fn-Intersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (throw (Exception. (str "Constructor for unresolvable class " (:class ctor)))))]
    (Fn-Intersection (make-Function (doall (map #(Method-symbol->Type % false) parameter-types))
                                    (RClass-of (Class->symbol cls) nil)
                                    nil nil
                                    :filter (-FS -top -bot))))) ;always a true value

(defn check-invoke-method [{:keys [args tag method env] :as expr} expected inst?]
  {:pre [((some-fn nil? TCResult?) expected)]
   :post [(-> % expr-type TCResult?)]}
  (assert method (str "Unresolved method invocation " (:method-name expr) ", insufficient type hints."))
  (prn "invoke method: " (Method->symbol method) inst?)
  (binding [*current-env* env]
    (let [rfin-type (ret (or (@METHOD-OVERRIDE-ENV (Method->symbol method))
                             (method->Function method)))
          _ (when inst?
              (let [ctarget (check (:target expr))]
                (prn "check target" (unparse-type (ret-t (expr-type ctarget)))
                     (unparse-type (RClass-of (Class->symbol (resolve (:declaring-class method))) nil)))
                (assert (subtype (ret-t (expr-type ctarget)) (RClass-of (Class->symbol (resolve (:declaring-class method)))
                                                                        nil)))))
          cargs (doall (map check args))
          result-type (check-funapp rfin-type (map expr-type cargs) expected)]
      (assoc expr
             expr-type result-type))))

(defmethod check :static-method
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (prn "static-method" (-> expr :method :name))
  (let [spec (static-method-special expr expected)]
    (cond
      (not= ::not-special spec) spec
      :else (check-invoke-method expr expected false))))

(defmethod check :instance-method
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (check-invoke-method expr expected true))

(def COMPILE-STUB-PREFIX "compile__stub")

(declare unwrap-datatype)

(defmethod check :instance-field
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (prn "instance-field:" expr)
  (let [; may be prefixed by COMPILE-STUB-PREFIX
        target-class (symbol
                       (str/replace-first (.getName ^Class (:target-class expr))
                                          (str COMPILE-STUB-PREFIX ".")
                                          ""))
        ;_ (prn (:target-class expr))
        ;_ (prn "target class" (str target-class) target-class)
        ;_ (prn (class target-class))
        fsym (symbol (:field-name expr))]
    (if-let [dtp (@DATATYPE-ENV target-class)]
      (let [dt (if (Poly? dtp)
                 ;generate new names
                 (unwrap-datatype dtp (repeatedly (:nbound dtp) gensym))
                 dtp)
            _ (assert (DataType? dt))
            ft (or (-> dt :fields (get fsym))
                   (throw (Exception. (str "No field " fsym " in Datatype " target-class))))]
        (assoc expr
               expr-type (ret ft)))
      (throw (Exception. ":instance-field NYI")))))

(defn DataType-ctor-type [sym]
  (let [dtp (@DATATYPE-ENV sym)]
    (cond
      (DataType? dtp) (let [dt dtp]
                        (Fn-Intersection 
                          (make-Function (-> dt :fields vals) dt)))
      (Poly? dtp) (let [nms (repeatedly (:nbound dtp) gensym)
                        bbnds (Poly-bbnds* nms dtp)
                        dt (unwrap-datatype dtp nms)]
                    (Poly* nms
                           bbnds
                           (Fn-Intersection 
                             (make-Function (-> dt :fields vals) dt))))
      :else (throw (Exception. (str "Cannot get DataType constructor of " sym))))))

(defmethod check :instance-of
  [{cls :class :keys [the-expr] :as expr} & [expected]]
  (let [cls-stub (symbol (.getName ^Class cls))
        clssym (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))
        inst-of (or (@DATATYPE-ENV clssym)
                    (RClass-of (Class->symbol (Class/forName (str clssym)) nil)))
        cexpr (check the-expr)
        expr-tr (expr-type cexpr)]
    (assoc expr
           expr-type (ret (Un -true -false)
                          (-FS (-filter-at inst-of (ret-o expr-tr))
                               (-not-filter-at inst-of (ret-o expr-tr)))
                          -empty))))

(defmethod check :new
  [{cls :class :keys [ctor args env] :as expr} & [expected]]
  (prn "check: :new" "env" env)
  (binding [*current-env* env]
    (let [cls-stub (Class->symbol cls)
          clssym (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))
          ifn (ret (or (and (@DATATYPE-ENV clssym)
                            (DataType-ctor-type clssym))
                       (Constructor->Function ctor)))
          _ (prn "Expected constructor" (unparse-type (ret-t ifn)))
          cargs (doall (map check args))
          res-type (check-funapp ifn (map expr-type cargs) nil)]
      (assoc expr
             expr-type res-type))))

(defmethod check :throw
  [{:keys [exception] :as expr} & [expected]]
  (let [cexception (check exception)
        _ (assert (subtype? (ret-t (expr-type cexception))
                            (RClass-of (Class->symbol Throwable) nil))
                  (str "Can only throw Throwable, found "
                       (unparse-type (ret-t (expr-type cexception)))))]
    (assoc expr
           expr-type (ret (Un)))))

(declare combine-props)

(def ^:dynamic *recur-target* nil)
(set-validator! #'*recur-target* #(or (nil? %)
                                      (every? Type? %)))

(defmethod check :recur
  [{:keys [args] :as expr} & [expected]]
  (assert *recur-target* "No recur target")
  (let [cargs (doall (map check args))
        _ (assert (= (count cargs) (count *recur-target*))
                  "Wrong number of arguments to recur")
        _ (doall (map subtype
                      (map (comp ret-t expr-type) cargs)
                      *recur-target*))]
    (assoc expr
           expr-type (ret (Un)))))

(defn check-let [{:keys [binding-inits body is-loop] :as expr} expected & {:keys [expected-bnds]}]
  (assert (or (not is-loop) expected-bnds) "Loop requires more annotations")
  (let [env (reduce (fn [env [{{:keys [sym init]} :local-binding} expected-bnd]]
                      {:pre [(PropEnv? env)]
                       :post [(PropEnv? env)]}
                      (let [{:keys [t fl o]} (->
                                               (expr-type
                                                 (with-lexical-env env
                                                   (check init (when is-loop
                                                                 (ret expected-bnd)))))
                                               ;substitute previous references to sym with an empty object,
                                               ;as old binding is shadowed
                                               (update-in [:t] subst-type sym -empty true)
                                               (update-in [:fl] subst-filter-set sym -empty true)
                                               (update-in [:o] subst-object sym -empty true))
                            ; update old env and new result with previous references of sym (which is now shadowed)
                            ; replaced with an empty object
                            _ (pr "ENV")
                            _ (print-env)
                            env (-> env
                                  (update-in [:l] #(into {} (for [[oldsym ty] %]
                                                              [oldsym (subst-type ty sym -empty true)])))
                                  (update-in [:props] (fn [props]
                                                        (doall (map #(subst-filter % sym -empty true) props)))))
                            ;_ (do (pr "let: env after") (print-env env))
                            ]
                        (cond
                          (FilterSet? fl)
                          (let [{:keys [then else]} fl
                                p* [(-imp (-not-filter (Un -nil -false) sym) then)
                                    (-imp (-filter (Un -nil -false) sym) else)]]
                            (-> env
                              ;update binding type
                              (assoc-in [:l sym] t)
                              ;update props
                              (update-in [:props] #(apply concat 
                                                          (combine-props p* % (atom true))))))

                          (NoFilter? fl) (-> env
                                           ;no propositions to add, just update binding type
                                           (assoc-in [:l sym] t)))))
                    *lexical-env* (map vector binding-inits (or expected-bnds
                                                                (repeat nil))))
        cbody (with-lexical-env env
                (if is-loop
                  (binding [*recur-target* expected-bnds]
                    (check body expected))
                  (check body expected)))

        ;now we return a result to the enclosing scope, so we
        ;erase references to any bindings this scope introduces
        unshadowed-type 
        (reduce (fn [ty sym]
                  {:pre [(TCResult? ty)
                         (symbol? sym)]}
                  (-> ty
                    (update-in [:t] subst-type sym -empty true)
                    (update-in [:fl] subst-filter-set sym -empty true)
                    (update-in [:o] subst-object sym -empty true)))
                (expr-type cbody)
                (map (comp :sym :local-binding) binding-inits))]
    (assoc expr
           expr-type unshadowed-type)))

(defmethod check :let
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (if-let [expected-bnds (and (:is-loop expr) *loop-bnd-anns*)]
    (binding [*loop-bnd-anns* nil]
      (check-let expr expected :expected-bnds expected-bnds))
    (check-let expr expected)))

(defn resolve* [atoms prop]
  {:pre [(every? Filter? atoms)
         (Filter? prop)]
   :post [(Filter? %)]}
  (reduce (fn [prop a]
            (cond
              (AndFilter? a)
              (loop [ps (:fs a)
                     result []]
                (if (empty? ps)
                  (apply -and result)
                  (let [p (first ps)]
                    (cond
                      (opposite? a p) -bot
                      (implied-atomic? p a) (recur (next ps) result)
                      :else (recur (next ps) (cons p result))))))
              :else prop))
          prop
          atoms))

(defn flatten-props [ps]
  {:post [(every? Filter? %)]}
  (cond
    (empty? ps) []
    (AndFilter? (first ps)) (flatten-props (concat (-> ps first :fs) (next ps)))
    :else (cons (first ps) (flatten-props (next ps)))))

(def type-equal? =)

(defn combine-props [new-props old-props flag]
  {:pre [(every? Filter? (concat new-props old-props))
         (instance? clojure.lang.Atom flag)
         (boolean? @flag)]
   :post [(let [[derived-props derived-atoms] %]
            (and (every? (some-fn ImpFilter? OrFilter? AndFilter?) derived-props)
                 (every? (some-fn TypeFilter? NotTypeFilter?) derived-atoms)))]}
  (let [atomic-prop? (some-fn TypeFilter? NotTypeFilter?)
        {new-atoms true new-formulas false} (group-by atomic-prop? (flatten-props new-props))]
    (loop [derived-props []
           derived-atoms new-atoms
           worklist (concat old-props new-formulas)]
      (if (empty? worklist)
        [derived-props derived-atoms]
        (let [p (first worklist)
              p (resolve* derived-atoms p)]
          (cond
            (AndFilter? p) (recur derived-props derived-atoms (concat (:fs p) (next worklist)))
            (ImpFilter? p) 
            (let [{:keys [a c]} p
                  implied? (some (fn [p] (implied-atomic? a p)) (concat derived-props derived-atoms))]
              #_(prn "combining " (unparse-filter p) " with " (map unparse-filter (concat derived-props
                                                                                        derived-atoms))
                   " and implied:" implied?)
              (if implied?
                (recur derived-props derived-atoms (cons c (rest worklist)))
                (recur (cons p derived-props) derived-atoms (next worklist))))
            (OrFilter? p)
            (let [ps (:fs p)
                  new-or (loop [ps ps
                                result []]
                           (cond
                             (empty? ps) (apply -or result)
                             (some (fn [other-p] (opposite? (first ps) other-p))
                                   (concat derived-props derived-atoms))
                             (recur (next ps) result)
                             (some (fn [other-p] (implied-atomic? (first ps) other-p))
                                   derived-atoms)
                             -top
                             :else (recur (next ps) (cons (first ps) result))))]
              (if (OrFilter? new-or)
                (recur (cons new-or derived-props) derived-atoms (next worklist))
                (recur derived-props derived-atoms (cons new-or (next worklist)))))
            (and (TypeFilter? p)
                 (type-equal? (Un) (:type p)))
            (do (reset! flag false)
              [derived-props derived-atoms])
            (TypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (NotTypeFilter? p)
                 (type-equal? (->Top) (:type p)))
            (do (reset! flag false)
              [derived-props derived-atoms])
            (NotTypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (TopFilter? p) (recur derived-props derived-atoms (next worklist))
            (BotFilter? p) (do (reset! flag false)
                             [derived-props derived-atoms])
            :else (recur (cons p derived-props) derived-atoms (next worklist))))))))

;; also not yet correct
;; produces old without the contents of rem
(defn remove* [old rem]
  (let [initial (if (subtype? old rem)
                  (Un) ;the empty type
                  (cond
                    ;FIXME TR also tests for App? here. ie (or (Name? old) (App? old))
                    (Name? old) ;; must be different, since they're not subtypes 
                                ;; and n must refer to a distinct struct type
                    old
                    (Union? old) (let [l (:types old)]
                                   (apply Un (map (fn [e] (remove* e rem)) l)))
                    (Mu? old) (remove* (unfold old) rem)
                    (Poly? old) (let [vs (repeatedly (:nbound old) gensym)
                                      b (Poly-body* vs old)]
                                  (Poly* vs 
                                         (Poly-bbnds* vs old)
                                         (remove* b rem)))
                    :else old))]
    (if (subtype? old initial) old initial)))

(defn -hmap-or-bot [types]
  (if (some #(= (Bottom) %) (concat (keys types) (vals types)))
    (Bottom)
    (->HeterogeneousMap types)))

(defn update [t lo]
  (let [t (-resolve t)]
    (cond
      ;heterogeneous map ops
      (and (TypeFilter? lo)
           (KeyPE? (first (:path lo)))
           (HeterogeneousMap? t)) (let [{:keys [type path id]} lo
                                        [{fpth-kw :val} & rstpth] path
                                        fpth (->Value fpth-kw)
                                        type-at-pth (get (:types t) fpth)]
                                    (if type-at-pth 
                                      (-hmap-or-bot (assoc (:types t) fpth (update type-at-pth (-filter type id rstpth))))
                                      (Bottom)))

      (and (NotTypeFilter? lo)
           (KeyPE? (first (:path lo)))
           (HeterogeneousMap? t)) (let [{:keys [type path id]} lo
                                        [{fpth-kw :val} & rstpth] path
                                        fpth (->Value fpth-kw)
                                        type-at-pth (get (:types t) fpth)]
                                    (if type-at-pth 
                                      (-hmap-or-bot (assoc (:types t) fpth (update type-at-pth (-not-filter type id rstpth))))
                                      (Bottom)))

      (and (TypeFilter? lo)
           (CountPE? (first (:path lo))))
      (let [u (:type lo)]
        (if-let [cnt (when (and (Value? u) (integer? (:val u)))
                       (make-ExactCountRange (:val u)))]
          (restrict cnt t)
          (do (prn "WARNING:" (str "Cannot infer Count from type " (unparse-type u)))
            t)))

      ;can't do much without a NotCountRange type or difference type
      (and (NotTypeFilter? lo)
           (CountPE? (first (:path lo))))
      t

      (and (TypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)]
        (restrict u t))

      (and (NotTypeFilter? lo)
           (empty? (:path lo))) (let [u (:type lo)]
                                  (remove* t u))

      (Union? t) (let [ts (:types t)]
                   (apply Un (doall (map (fn [t] (update t lo)) ts))))
      (Intersection? t) (let [ts (:types t)]
                          (apply In (doall (map (fn [t] (update t lo)) ts))))
      :else (throw (Exception. (str "update along ill-typed path " (unparse-type t) " " (with-out-str (pr lo))))))))

; f can be a composite filter. bnd-env is a the :l of a PropEnv
; ie. a map of symbols to types
(defn update-composite [bnd-env f]
  {:pre [(Filter? f)]}
  ;(prn "update-composite")
  (cond
    (AndFilter? f) (apply merge-with In
                     (for [fl (:fs f)]
                       (update-composite bnd-env fl)))
    (OrFilter? f) (apply merge-with Un
                    (for [fl (:fs f)]
                      (update-composite bnd-env fl)))

    (BotFilter? f)
    (zipmap (:keys bnd-env) (Un))

    (or (TypeFilter? f)
        (NotTypeFilter? f))
    (let [x (:id f)]
      (update-in bnd-env [x] (fn [t]
                               ;check if var is ever a target of a set!
                               (if (is-var-mutated? x)
                                 ; if it is, we do nothing
                                 t
                                 ;otherwise, refine the type
                                 (let [t (or t (->Top))
                                       new-t (update t f)]
                                   new-t)))))
    :else bnd-env))

;; sets the flag box to #f if anything becomes (U)
(defn env+ [env fs flag]
  {:pre [(PropEnv? env)
         (every? Filter? fs)
         (boolean? @flag)]
   :post [(PropEnv? env)
          (boolean? @flag)]}
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              {:pre [(PropEnv? env)
                     (Filter? f)]}
              (let [env (update-in env [:l] update-composite f)]
                ; update flag if a variable is now bottom
                (when (seq (set/intersection (set (vals (:l env)))
                                             #{(Un)}))
                  (reset! flag false))
                env))
            (assoc env :props (concat atoms props))
            (concat atoms props))))

(def object-equal? =)

(defn check-if [tst thn els & [expected]]
  {:pre [(TCResult? tst)
         ((some-fn TCResult? nil?) expected)]
   :post [(TCResult? %)]}
  (letfn [(tc [expr reachable?]
            {:post [(TCResult? %)]}
            (when-not reachable?
              #_(prn "Unreachable code found.. " expr))
            (cond
              ;; if reachable? is #f, then we don't want to verify that this branch has the appropriate type
              ;; in particular, it might be (void)
              (and expected reachable?)
              (-> (check expr (-> expected
                                (update-in [:fl] #(map (constantly (->NoFilter)) %))
                                (update-in [:o] #(map (constantly (->NoObject)) %))))
                expr-type)
              ;; this code is reachable, but we have no expected type
              reachable? (-> (check expr) expr-type)
              ;; otherwise, this code is unreachable
              ;; and the resulting type should be the empty type
              :else (do #_(prn "Not checking unreachable code")
                      (ret (Un)))))]
    (let [{fs+ :then fs- :else :as f1} (:fl tst)
         ; _ (prn "check-if: fs+" (unparse-filter fs+))
         ; _ (prn "check-if: fs-" (unparse-filter fs-))
          flag+ (atom true)
          flag- (atom true)
          _ (set-validator! flag+ boolean?)
          _ (set-validator! flag- boolean?)

          _ (print-env)
          idsym (gensym)
          env-thn (env+ *lexical-env* [fs+] flag+)
;          _ (do (pr "check-if: env-thn")
;              (print-env env-thn))
          env-els (env+ *lexical-env* [fs-] flag-)
;          _ (do (pr "check-if: env-els")
;              (print-env env-els))
;          new-thn-props (set
;                          (filter atomic-filter?
;                                  (set/difference
;                                    (set (:props *lexical-env*))
;                                    (set (:props env-thn)))))
          ;_ (prn idsym"env+: new-thn-props" (map unparse-filter new-thn-props))
;          new-els-props (set
;                          (filter atomic-filter?
;                                  (set/difference
;                                    (set (:props *lexical-env*))
;                                    (set (:props env-els)))))
          ;_ (prn idsym"env+: new-els-props" (map unparse-filter new-els-props))
          {ts :t fs2 :fl os2 :o :as then-ret} (with-lexical-env env-thn
                                                (tc thn @flag+))
          {us :t fs3 :fl os3 :o :as else-ret} (with-lexical-env env-els
                                                (tc els @flag-))]

      ;some optimization code here, contraditions etc? omitted

;      (prn "check-if: then branch:" (unparse-TCResult then-ret))
;      (prn "check-if: else branch:" (unparse-TCResult else-ret))
      (cond
        ;both branches reachable
        (and (not (type-equal? (Un) ts))
             (not (type-equal? (Un) us)))
        (let [r (let [filter (cond
                               (or (NoFilter? fs2)
                                   (NoFilter? fs3)) (-FS -top -top)
                               (and (FilterSet? fs2)
                                    (FilterSet? fs3))
                               (let [{f2+ :then f2- :else} fs2
                                     {f3+ :then f3- :else} fs3
                                     ; +ve test, +ve then
                                     new-thn-props (:props env-thn)
                                     new-els-props (:props env-els)
                                     +t+t (apply -and fs+ f2+ new-thn-props)
                                     ; -ve test, +ve else
                                     -t+e (apply -and fs- f3+ new-els-props)
                                     ; +ve test, -ve then
                                     +t-t (apply -and fs+ f2- new-thn-props)
                                     ; -ve test, -ve else
                                     -t-e (apply -and fs- f3- new-els-props)

                                     final-thn-prop (-or +t+t -t+e)
                                     final-els-prop (-or +t-t -t-e)
                                     fs (-FS final-thn-prop final-els-prop)]
                                 fs)
                               :else (throw (Exception. (str "What are these?" fs2 fs3))))
                      type (Un ts us)
                      object (if (object-equal? os2 os3) os2 (->EmptyObject))]
                  (ret type filter object))]
          ;(prn "check if:" "both branches reachable, with combined result" (unparse-TCResult r))
          (if expected (check-below r expected) r))
        ;; special case if one of the branches is unreachable
        (type-equal? us (Un))
        (if expected (check-below (ret ts fs2 os2) expected) (ret ts fs2 os2))
        (type-equal? ts (Un))
        (if expected (check-below (ret us fs3 os3) expected) (ret us fs3 os3))
        :else (throw (Exception. "Something happened"))))))

(defmethod check :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (prn "check :if")
  (let [ctest (check test)]
    (assoc expr
           expr-type (check-if (expr-type ctest) then else))))

(defmethod check :def
  [{:keys [var init init-provided] :as expr} & [expected]]
  (assert (not expected) expected)
  (prn "Checking" var)
  (if (not (.isMacro ^Var var))
    (let [cexpr (cond 
                  (not init-provided) expr ;handle (declare ..)
                  :else (check init (ret (type-of (var->symbol var))
                                         (-FS -top -top)
                                         -empty)))]
      (assoc cexpr
             expr-type (ret (RClass-of (Class->symbol Var) nil))))
    (assoc expr
           expr-type (ret (RClass-of (Class->symbol Var) nil)))))

(declare check-new-instance-method)

(defn unwrap-datatype 
  "Takes a possibly polymorphic DataType and returns the 
  DataType after instantiating it"
  ([dt nms]
   {:pre [((some-fn DataType? Poly?) dt)
          (every? symbol? nms)]
    :post [(DataType? %)]}
   (if (Poly? dt)
     (Poly-body* nms dt)
     dt))
  ([dt] (let [nms (when (Poly? dt)
                    (repeatedly (:nbound dt) gensym))]
          (unwrap-datatype dt nms))))

(defmethod check :deftype*
  [{nme :name :keys [methods] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert nme) ;remove once analyze is released
  ;TODO check fields match
  (prn "Checking deftype definition:" nme)
  (let [cmmap (into {} (for [[k v] (:mmap expr)]
                         [(symbol (first k)) (@#'clojure.reflect/method->map v)]))
        _ (assert ((hash-c? (every-pred symbol? (complement namespace))
                            #(instance? clojure.reflect.Method %))
                     cmmap))
        dtp (@DATATYPE-ENV nme)
        dt (if (Poly? dtp)
             (do (assert (-> dtp meta :actual-frees))
               (unwrap-datatype dtp (-> dtp meta :actual-frees)))
             dtp)

        _ (assert (DataType? dt))
        _ (assert dt (str "Untyped datatype definition: " nme))
        ; update this deftype's ancestors to include each protocol/interface in this deftype
        old-ancestors (or (@DATATYPE-ANCESTOR-ENV nme) #{})
        ancestor-diff (set/difference
                        (set
                          (for [[_ method] cmmap]
                            (let [tsym (:declaring-class method)]
                              (if-let [cls (when-let [cls (resolve tsym)]
                                             (and (class? cls) 
                                                  cls))]
                                (or (first (filter #(= (:on-class %) tsym) (vals @PROTOCOL-ENV)))
                                    (RClass-of (Class->symbol cls) nil))
                                (resolve-protocol tsym)))))
                        old-ancestors)
        _ (prn "ancestor diff" ancestor-diff)
        _ (swap! DATATYPE-ANCESTOR-ENV update-in [nme] set/union ancestor-diff)
        _ (try
            (doseq [inst-method methods]
              (prn "Checking deftype* method: "(:name inst-method))
              (let [nme (:name inst-method)
                    _ (assert (symbol? nme)) ;can remove once new analyze is released
                    method-sig (cmmap nme)
                    _ (assert (instance? clojure.reflect.Method method-sig))
                    ;_ (prn "method-sig" method-sig)
                    expected-ifn 
                    (extend-method-expected dt
                                            (or (let [ptype (first
                                                              (filter #(= (:on-class %) (:declaring-class method-sig))
                                                                      (vals @PROTOCOL-ENV)))]
                                                  ;(prn "ptype" ptype)
                                                  (when ptype
                                                    (let [munged-methods (into {} (for [[k v] (:methods ptype)]
                                                                                    [(symbol (munge k)) v]))]
                                                      (munged-methods (:name method-sig)))))
                                                (instance-method->Function method-sig)))
                    ;_ (prn "expected-ifn: " (unparse-type expected-ifn))
                    ]
                (with-locals (:fields dt)
                  (prn "lexical env when checking method" nme *lexical-env*)
                  (prn (:fields dt))
                  (check-new-instance-method
                    inst-method 
                    expected-ifn))))
            (catch Throwable e
              ; reset old ancestors
              (swap! DATATYPE-ANCESTOR-ENV update-in [nme] set/difference ancestor-diff)
              (throw e)))]
    (assoc expr
           expr-type (ret (let [res (resolve nme)]
                            (assert (class? res))
                            (-val res))))))

(defn check-new-instance-method
  [{:keys [body required-params] :as expr} expected-fin]
  {:pre [(Fn-Intersection? expected-fin)]}
  (let [_ (assert (= 1 (count (:types expected-fin))))
        {:keys [dom rng] :as expected-fn} (-> expected-fin :types first)
        _ (assert (not (:rest expected-fn)))
        cbody (with-locals (zipmap (map :sym required-params) dom)
                (print-env)
                (check body (ret (:t rng)
                                 (:fl rng)
                                 (:o rng))))]
    (assoc expr
           expr-type (expr-type cbody))))

(defmethod check :import*
  [{:keys [class-str] :as expr} & [expected]]
  (assoc expr
         expr-type (ret -nil)))

(defmethod check :case*
  [{:keys [] :as expr} & [expected]]
  (prn "Checking case")
  ; tests have no duplicates
  (let [;_ (prn (:the-expr expr))
        cthe-expr (check (:the-expr expr))
        etype (expr-type cthe-expr)
        ctests (doall (map check (:tests expr)))
        cdefault (check (:default expr))
        cthens-and-envs (for [[tst-ret thn] (map vector (map expr-type ctests) (:thens expr))]
                          (let [{{fs+ :then} :fl :as rslt} (tc-equiv := etype tst-ret)
                                flag+ (atom true)
                                env-thn (env+ *lexical-env* [fs+] flag+)
                                then-ret (with-lexical-env env-thn
                                           (check thn))]
                            [(assoc thn
                                    expr-type (expr-type then-ret))
                             env-thn]))
        ;TODO consider tests that failed to refine env
        cdefault (check (:default expr))
        case-result (let [type (apply Un (map (comp :t expr-type) (cons cdefault (map first cthens-and-envs))))
                          ; TODO
                          filter (-FS -top -top)
                          ; TODO
                          object -empty]
                      (ret type filter object))]
    (assoc expr
           expr-type case-result)))

(defmacro cf 
  "Type check a form and return its type"
  ([form]
  `(-> (ast ~form) check expr-type unparse-TCResult))
  ([form expected]
  `(-> (ast ~form) #(check % (parse-type '~expected) expr-type unparse-TCResult))))

(defn check-ns 
  ([] (check-ns (ns-name *ns*)))
  ([nsym]
   (require nsym)
   (with-open [pbr (analyze/pb-reader-for-ns nsym)]
     (let [[_ns-decl_ & asts] (analyze/analyze-ns pbr nsym)]
       (doseq [ast asts]
         (check ast))))))

(defn trepl []
  (clojure.main/repl 
    :eval (fn [f] 
            (let [t (-> (analyze/analyze-form f) 
                      check expr-type unparse-TCResult)] 
              (prn t) 
              (eval f)))))

(comment 
  (check-ns 'typed.test.example)
  ; very slow because of update-composite
  (check-ns 'typed.test.rbt)
  (check-ns 'typed.test.macro)
  (check-ns 'typed.test.conduit)
  (check-ns 'typed.test.deftype)
  (check-ns 'typed.test.core-logic)
  )
