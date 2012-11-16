(ns typed.core
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef AReference ARef IDeref IReference APersistentSet PersistentHashSet Sorted
                         LazySeq APersistentMap))
  (:require [analyze.core :refer [ast] :as analyze]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [trammel.core :as contracts]
            [clojure.math.combinatorics :as comb]
            #_[clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))

;load constraint shorthands, other handy functions
(load "utils")

;Note: defrecord is now trammel's defconstrainedrecord

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions

(defn print-env
  "Print the current type environment, and debug-string"
  [debug-string] nil)

(defn print-filterset
  "Print the filter set attached to form, and debug-string"
  [debug-string frm] 
  frm)

(declare Method->Function unparse-type unparse-filter)

(defn method-type 
  "Given a method symbol, print the Typed Clojure types assigned to it"
  [mname]
  (let [ms (->> (reflect/type-reflect (Class/forName (namespace mname)))
             :members
             (filter #(and (instance? clojure.reflect.Method %)
                           (= (str (:name %)) (name mname))))
             set)
        _ (assert (seq ms) (str "Method " mname " not found"))]
    (prn "Method name:" mname)
    (doseq [m ms]
      (prn (unparse-type (Method->Function m))))))

(defn inst-poly 
  [inst-of types-syn]
  inst-of)

(defn inst-poly-ctor [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor."
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

(defn fn>-ann [fn-of param-types-syn]
  fn-of)

(defn pfn>-ann [fn-of polys param-types-syn]
  fn-of)

(defn loop>-ann [loop-of bnding-types]
  loop-of)

(defn- parse-fn>
  "(fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [is-poly forms]
  (let [name (when (symbol? (first forms))
               (first forms))
        forms (if name (rest forms) forms)
        poly (when is-poly
               (first forms))
        forms (if poly (rest forms) forms)
        methods (if ((some-fn vector? keyword?) (first forms))
                  (list forms)
                  forms)
        ;(fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
        ; (HMap {:dom (Seqable TypeSyntax)
        ;        :rng (U nil TypeSyntax)
        ;        :body Any})
        parsed-methods (doall 
                         (for [method methods]
                           (let [[ret has-ret?] (when (not (vector? (first method)))
                                                  (assert (= :- (first method))
                                                          "Return type for fn> must be prefixed by :-")
                                                  [(second method) true])
                                 method (if ret 
                                          (nnext method)
                                          method)
                                 body (rest method)
                                 arg-anns (first method)
                                 [required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                             (assert (sequential? required-params)
                                     "Must provide a sequence of typed parameters to fn>")
                             (assert (not rest-param) "fn> doesn't support rest parameters yet")
                             {:dom-syntax (doall (map (comp second next) required-params))
                              :dom-lhs (doall (map first required-params))
                              :rng-syntax ret
                              :has-rng? has-ret?
                              :body body})))]
    {:poly poly
     :fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body dom-lhs]} parsed-methods]
                    (apply list (vec dom-lhs) body))))
     :parsed-methods parsed-methods}))

(defmacro pfn> 
  "Define a polymorphic typed anonymous function.
  (pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [poly fn parsed-methods]} (parse-fn> true forms)]
    `(pfn>-ann ~fn '~poly '~parsed-methods)))

(defmacro fn> 
  "Define a typed anonymous function.
  (fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [fn parsed-methods]} (parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))

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

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(tc-ignore
   (let [sym# '~sym
         qsym# (if (namespace sym#)
                 sym#
                 (symbol (name (ns-name *ns*)) (name sym#)))
         ty# (parse-type '~ty)]
     (assert (not (namespace sym#)) (str "Cannot declare qualified name " sym#))
     (declare ~sym)
     (declare-names ~sym)
     (declare-alias-kind* qsym# ty#))))

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
    (when-let [tfn# (@DECLARED-KIND-ENV sym#)]
      (assert (subtype? ty# tfn#) (error-msg "Declared kind " (unparse-type tfn#)
                                             " does not match actual kind " (unparse-type ty#))))
    [sym# (unparse-type ty#)])))

(defn into-array>* [javat cljt coll]
  (into-array (resolve javat) coll))

(defmacro into-array> 
  "Make a Java array with Java class javat and Typed Clojure type
  cljt. Resulting array will be of type javat, but elements of coll must be under
  cljt. cljt should be a subtype of javat (the same or more specific)."
  [javat cljt coll]
  `(into-array>* '~javat '~cljt ~coll))

(defn ann-form* [form ty]
  form)

(defn ann-form* [form ty]
  form)

(defmacro ann-form [form ty]
  `(ann-form* ~form '~ty))

(defn unsafe-ann-form* [form ty]
  form)

(defmacro unsafe-ann-form [form ty]
  `(unsafe-ann-form* ~form '~ty))

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

(declare abstract-many instantiate-many)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(def nat? (every-pred integer? (complement neg?)))

(def Type ::Type)

(defn Type? [a]
  (isa? (class a) Type))

(declare TCResult? Result? Function? DottedPretype? TypeFn?)

(def AnyType ::AnyType)

(defn AnyType? [a]
  (isa? (class a) AnyType))

(derive Type AnyType)

(defn declare-type [a]
  (derive a Type))

(defn declare-AnyType [a]
  (derive a AnyType))

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

(declare ->HeterogeneousMap HeterogeneousMap? Bottom)

(def empty-union (->Union #{}))

(defn -hmap [types]
  (if (some #(= (Bottom) %) (concat (keys types) (vals types)))
    (Bottom)
    (->HeterogeneousMap types)))

#_(defn simplify-HMap-Un [hmaps]
  {:pre [(every? HeterogeneousMap? hmaps)]
   :post [(Type? %)]}
  (let [mss (vals
              (group-by #(-> % :types keys set) (set hmaps)))
        ;union the vals of maps with exactly the same keys
        flat (set
               (for [ms mss]
                 (-hmap
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
      :else (->Union (set (apply concat
                                 (for [t (set types)]
                                   (if (Union? t)
                                     (:types t)
                                     [t]))))))))

(defn Bottom []
  empty-union)

(def -nothing (Bottom))

(defn Bottom? [a]
  (= empty-union a))

(declare Function? Poly? PolyDots?)

;should probably be ordered
(defrecord Intersection [types]
  "An unordered intersection of types."
  [(seq types)
   (every? Type? types)])

(defrecord FnIntersection [types]
  "An ordered intersection of Functions."
  [(seq types)
   (sequential? types)
   (every? Function? types)])

(declare-type FnIntersection)

(declare In HeterogeneousMap? ->HeterogeneousMap overlap)

(defn In [& types]
  {:pre [(every? Type? types)]
   :post [(Type? %)]}
           ;flatten intersections
  (let [ts (set (apply concat
                       (for [t (set types)]
                         (if (Intersection? t)
                           (:types t)
                           [t]))))]
    (cond
      (or (empty? ts)
          (ts (Un))) (Bottom)

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

      (ts -any) (apply In (disj ts -any))
      :else (->Intersection ts))))

(declare-type Intersection)

(def variances #{:constant :covariant :contravariant :invariant :dotted})

(defn variance? [v]
  (contains? variances v))

(declare Scope? TypeFn?)

(defrecord Bounds [upper-bound lower-bound higher-kind]
  "A type bound or higher-kind bound on a variable"
  [(some-fn (and (every? (some-fn Type? Scope?) [upper-bound lower-bound])
                 (nil? higher-kind))
            (and (every? nil? [upper-bound lower-bound])
                 (TypeFn? higher-kind)))])

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

(defn scope-depth? 
  "True if scope is has depth number of scopes nested"
  [scope depth]
  {:pre [(Scope? scope)
         (nat? depth)]}
  (Type? (last (take (inc depth) (iterate #(and (Scope? %)
                                                (:body %))
                                          scope)))))

(defrecord Projection [afn ts]
  "Projects type variables as arguments to afn"
  [(fn? afn)
   (every? AnyType? ts)])

(declare-type Projection)

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

(defn RClass->Class [rcls]
  (Class/forName (str (.the-class rcls))))

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
  (if (seq variances)
    (Poly* names (repeat (count names) no-bounds) (->RClass variances poly? the-class replacements) names)
    (->RClass nil nil the-class replacements)))

(declare poly-RClass-from)

(declare substitute-many unparse-type Class->symbol symbol->Class)

(defn RClass-supers* 
  "Return a set of ancestors to the RClass"
  [{:keys [poly? replacements the-class] :as rcls}]
  {:pre [(RClass? rcls)]
   :post [(set? %)
          (every? Type? %)
          (<= (count (filter (some-fn FnIntersection? Poly? PolyDots?) %))
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

(defrecord TypeFn [nbound variances bbnds scope]
  "A type function containing n bound variables with variances.
  It is of a higher kind"
  [(nat? nbound)
   (every? variance? variances)
   (every? Bounds? bbnds)
   (apply = nbound (map count [variances bbnds]))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type TypeFn)

(defn tfn-bound [tfn]
  (->Bounds nil nil tfn))

(declare visit-bounds)

;smart constructor
(defn TypeFn* [names variances bbnds body]
  {:pre [(every? symbol names)
         (every? variance? variances)
         (every? Bounds? bbnds)
         (apply = (map count [names variances bbnds]))
         ((some-fn TypeFn? Type?) body)]}
  (if (empty? names)
    body
    (->TypeFn (count names) 
              variances
              (vec
                (for [bnd bbnds]
                  (visit-bounds bnd #(abstract-many names %))))
              (abstract-many names body))))

;smart destructor
(defn TypeFn-body* [names typefn]
  {:pre [(every? symbol? names)
         (TypeFn? typefn)]}
  (assert (= (.nbound typefn) (count names)) "Wrong number of names")
  (instantiate-many names (.scope typefn)))

(defn TypeFn-bbnds* [names typefn]
  {:pre [(every? symbol? names)
         (TypeFn? typefn)]
   :post [(every? Bounds? %)]}
  (assert (= (.nbound typefn) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (visit-bounds b #(instantiate-many names %)))
        (.bbnds typefn)))

(defrecord Poly [nbound bbnds scope actual-frees]
  "A polymorphic type containing n bound variables, with display names actual-frees"
  [(nat? nbound)
   (every? Bounds? bbnds)
   (every? symbol? actual-frees)
   (apply = nbound (map count [bbnds actual-frees]))
   (scope-depth? scope nbound)
   (Scope? scope)])

(declare-type Poly)

;smart constructor
(defn Poly* [names bbnds body free-names]
  {:pre [(every? symbol names)
         (every? Bounds? bbnds)
         (Type? body)
         (every? symbol? free-names)
         (apply = (map count [names bbnds free-names]))]}
  (if (empty? names)
    body
    (->Poly (count names) 
            (vec
              (for [bnd bbnds]
                (visit-bounds bnd #(abstract-many names %))))
            (abstract-many names body)
            free-names)))

(defn Poly-free-names* [poly]
  {:pre [(Poly? poly)]
   :post [((every-pred seq (every-c? symbol?)) %)]}
  (.actual-frees poly))

;smart destructor
(defn Poly-body* [names poly]
  {:pre [(every? symbol? names)
         (Poly? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (.scope poly)))

(defn Poly-bbnds* [names poly]
  {:pre [(every? symbol? names)
         (Poly? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (visit-bounds b #(instantiate-many names %)))
        (.bbnds poly)))

(defrecord PolyDots [nbound bbnds scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(nat? nbound)
   (every? Bounds? bbnds)
   (= nbound (count bbnds))
   (scope-depth? scope nbound)
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

(defrecord TApp [rator rands]
  "An application of a type function to arguments."
  [((some-fn Name? TypeFn? F? B?) rator)
   (every? (some-fn TypeFn? Type?) rands)])

(declare-type TApp) ;not always a type

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

(declare error-msg)

(defn instantiate-typefn [t types]
  (assert (TypeFn? t) (unparse-type t))
  (do (assert (= (.nbound t) (count types)) (error-msg "Wrong number of arguments passed to type function: "
                                                       (unparse-type t) (mapv unparse-type types)))
    (let [nms (repeatedly (.nbound t) gensym)
          body (TypeFn-body* nms t)]
      (subst-all (make-simple-substitution nms types) body))))

(defn instantiate-poly [t types]
  (cond
    (Poly? t) (do (assert (= (:nbound t) (count types)) (error-msg "Wrong number of arguments passed to polymorphic type: "
                                                             (unparse-type t) (mapv unparse-type types)))
                (let [nms (repeatedly (:nbound t) gensym)
                      body (Poly-body* nms t)]
                  (subst-all (make-simple-substitution nms types) body)))
    ;PolyDots NYI
    :else (throw (Exception. "instantiate-poly: requires Poly, and PolyDots NYI"))))

(declare ^:dynamic *current-env* resolve-app*)

(declare resolve-tapp*)

(defn resolve-TApp [app]
  {:pre [(TApp? app)]}
  (resolve-tapp* (.rator app) (.rands app)))

(defn resolve-tapp* [rator rands]
  (let [rator (-resolve rator)
        _ (assert (TypeFn? rator) (unparse-type rator))]
    (assert (= (count rands) (.nbound rator))
            (error-msg "Wrong number of arguments provided to type function"
                       (unparse-type rator)))
    (instantiate-typefn rator rands)))

(defn resolve-App [app]
  {:pre [(App? app)]}
  (resolve-app* (.rator app) (.rands app)))

(defn resolve-app* [rator rands]
  (let [rator (-resolve rator)]
    (cond
      (Poly? rator) (do (assert (= (count rands) (.nbound rator))
                                (error-msg "Wrong number of arguments provided to polymorphic type"
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
    (TApp? ty) (resolve-TApp ty)
    :else ty))

(defn requires-resolving? [ty]
  (or (Name? ty)
      (App? ty)
      (and (TApp? ty)
           (not (F? (.rator ty))))
      (Mu? ty)))

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
  [((hash-c? Value? (some-fn Type? Result?))
     types)])

(defn make-HMap [mandatory optional]
  (assert (= #{}
             (set/intersection (-> mandatory keys set)
                               (-> optional keys set))))
  (apply Un
         (for [ss (map #(into {} %) (comb/subsets optional))]
           (-hmap (merge mandatory ss)))))

(declare-type HeterogeneousMap)

(defrecord HeterogeneousVector [types]
  "A constant vector, clojure.lang.IPersistentVector"
  [(vector? types)
   (every? (some-fn Type? Result?) types)])

(defn -hvec [types]
  (if (some Bottom? types)
    (Bottom)
    (->HeterogeneousVector types)))

(declare-type HeterogeneousVector)

(defrecord HeterogeneousList [types]
  "A constant list, clojure.lang.IPersistentList"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousList)

(defrecord HeterogeneousSeq [types]
  "A constant seq, clojure.lang.ISeq"
  [(sequential? types)
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

(declare-AnyType DottedPretype)

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

(declare-AnyType Function)

(defrecord TopFunction []
  "Supertype to all functions"
  [])

(defrecord CountRange [lower upper]
  "A sequence of count between lower and upper.
  If upper is nil, between lower and infinity."
  [(nat? lower)
   (or (nil? upper)
       (and (nat? upper)
            (<= lower upper)))])

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

(declare ->EmptyObject ->Result -FS -top)

(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil))
  ([t f] (make-Result t f nil))
  ([t f o] (->Result t (or f (-FS -top -top)) (or o (->EmptyObject)))))

(declare ret-t ret-f ret-o)

(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to the most general filter
  and EmptyObject"
  ([dom rng] (make-Function dom rng nil nil))
  ([dom rng rest] (make-Function dom rng rest nil))
  ([dom rng rest drest & {:keys [filter object kws]}]
   (->Function dom (->Result rng (or filter (-FS -top -top)) (or object (->EmptyObject))) rest drest kws)))

(defn make-FnIntersection [& fns]
  {:pre [(every? Function? fns)]}
  (->FnIntersection fns))

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

(declare Filter? RObject? ret)

(defrecord Result [t fl o]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (Filter? fl)
   (RObject? o)])

(declare-AnyType Result)

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
   :post [(Filter? %)]}
  (:fl r))

(defn Result-object* [r]
  {:pre [(Result? r)]
   :post [(RObject? %)]}
  (:o r))

(def no-bounds (->Bounds (->Top) (Un) nil))


(declare unparse-path-elem)

(declare TypeFilter? NotTypeFilter? type-of TCResult? ret-t Nil? False? True? unparse-type)

(def ^:dynamic *mutated-bindings* #{})

(defn is-var-mutated? [id]
  (contains? *mutated-bindings* id))

(load "type_ops")
(load "filter_rep")
(load "filter_ops")

(defrecord TCResult [t fl o]
  "This record represents the result of typechecking an expression"
  [(AnyType? t)
   (FilterSet? fl)
   (RObject? o)])

(declare-AnyType TCResult)

(load "path_rep")
(load "object_rep")

; must be after type/object/filter definitions
(load "fold")

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
   ;; DO NOT REMOVE
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

(defn gen-datatype* [provided-name fields variances args ancests]
  `(let [provided-name-str# (str '~provided-name)
         _# (prn "provided-name-str" provided-name-str#)
         munged-ns-str# (if (some #(= \. %) provided-name-str#)
                          (apply str (butlast (apply concat (butlast (partition-by #(= \. %) provided-name-str#)))))
                          (str (munge (-> *ns* ns-name))))
         _# (prn "munged-ns-str" munged-ns-str#)
         demunged-ns-str# (str (clojure.repl/demunge munged-ns-str#))
         _# (prn "demunged-ns-str" demunged-ns-str#)
         local-name# (if (some #(= \. %) provided-name-str#)
                       (symbol (apply str (last (partition-by #(= \. %) (str provided-name-str#)))))
                       provided-name-str#)
         _# (prn "local-name" local-name#)
         s# (symbol (str munged-ns-str# \. local-name#))
         fs# (apply array-map (apply concat (with-frees (mapv make-F '~args)
                                              (mapv parse-field '~fields))))
         as# (set (with-frees (mapv make-F '~args)
                    (mapv parse-type '~ancests)))
         _# (add-datatype-ancestors s# as#)
         pos-ctor-name# (symbol demunged-ns-str# (str "->" local-name#))
         args# '~args
         vs# '~variances
         dt# (if args#
               (Poly* args# (repeat (count args#) no-bounds)
                      (->DataType s# vs# (map make-F args#) fs#)
                      args#)
               (->DataType s# nil nil fs#))
         pos-ctor# (if args#
                     (Poly* args# (repeat (count args#) no-bounds)
                            (make-FnIntersection
                              (make-Function (vec (vals fs#)) (->DataType s# vs# (map make-F args#) fs#)))
                            args#)
                     (make-FnIntersection
                       (make-Function (vec (vals fs#)) dt#)))]
     (do 
       (when vs#
         (let [f# (mapv make-F (repeatedly (count vs#) gensym))]
           (alter-class* s# (RClass* (map :name f#) vs# f# s# {}))))
       (add-datatype s# dt#)
       (add-var-type pos-ctor-name# pos-ctor#)
       [[s# (unparse-type dt#)]
        [pos-ctor-name# (unparse-type pos-ctor#)]])))

(defmacro ann-datatype [dname fields & {ancests :unchecked-ancestors rplc :replace}]
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide name symbol: " dname))
  `(tc-ignore
     ~(gen-datatype* dname fields nil nil ancests)))

(defmacro ann-pdatatype [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace}]
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide local symbol: " dname))
  `(tc-ignore
     ~(gen-datatype* dname fields (map second vbnd) (map first vbnd) ancests)))

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
         t# (if fs#
              (Poly* (map :name fs#) (repeat (count fs#) no-bounds) 
                     (->Protocol s# '~variances fs# on-class# ms#)
                     (map :name fs#))
              (->Protocol s# nil nil on-class# ms#))]
     (do
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

(defmacro override-constructor [ctorsym typesyn]
  `(tc-ignore
  (let [t# (parse-type '~typesyn)
        s# '~ctorsym]
    (do (add-constructor-override s# t#)
      [s# (unparse-type t#)]))))

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
    byte Byte/TYPE
    short Short/TYPE
    int Integer/TYPE
    long Long/TYPE
    float Float/TYPE
    double Double/TYPE
    boolean Boolean/TYPE
    char Character/TYPE
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


(load "dvar_env")
(load "datatype_ancestor_env")
(load "datatype_env")
(load "protocol_env")
(load "method_override_env")
(load "ctor_override_env")
(load "method_return_nilables")
(load "method_param_nilables")
(load "declared_kind_env")
(load "name_env")
(load "rclass_env")

(load "parse")
(load "unparse")
(load "frees")
(load "promote_demote")
(load "cs_gen")
(load "subst_dots")
(load "infer")
(load "tvar_rep")
(load "subst")
(load "trans")
(load "inst")
(load "subtype")
(load "alter")
(load "ann")
(load "check")

(defmacro cf 
  "Type check a form and return its type"
  ([form]
  `(tc-ignore
     (-> (ast ~form) check expr-type unparse-TCResult)))
  ([form expected]
  `(tc-ignore
     (-> (ast (ann-form ~form ~expected)) (#(check % (ret (parse-type '~expected)))) expr-type unparse-TCResult))))

(defn check-ns 
  ([] (check-ns (ns-name *ns*)))
  ([nsym]
   (require nsym)
   (with-open [pbr (analyze/pb-reader-for-ns nsym)]
     (let [[_ns-decl_ & asts] (analyze/analyze-ns pbr (analyze/uri-for-ns nsym) nsym)]
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
  (check-ns 'typed.test.ckanren)
  )
