#_(set! *warn-on-reflection* false)

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

(def third (comp second next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint shorthands

(def boolean? (some-fn true? false?))

(defn =-c? [& as]
  #(apply = (concat as %&)))

(defn every-c? [c]
  #(every? c %))

(defn hvector-c? [& ps]
  (apply every-pred vector?
         (map (fn [p i] #(p (nth % i false))) ps (range))))

(defn array-map-c? [ks-c? vs-c?]
  (every-pred #(instance? PersistentArrayMap %)
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn hmap-c? [& key-vals]
  (every-pred map?
              #(every? identity 
                       (for [[k vc] (partition 2 key-vals)]
                         (and (contains? % k)
                              (vc (get % k)))))))

(defn hash-c? [ks-c? vs-c?]
  (every-pred map?
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn set-c? [c?]
  (every-pred set?
              #(every? c? %)))

(defn sequential-c? [c?]
  (every-pred sequential?
              (every-c? c?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions

(defn print-env
  "Print the current type environment, and debug-string"
  [debug-string] nil)

(defn print-filterset
  "Print the filter set attached to form, and debug-string"
  [debug-string frm] 
  frm)

(declare Method->Function unparse-type)

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

(declare add-declared-kind)

(defn declare-alias-kind* [sym ty]
  (add-declared-kind sym ty))

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

(add-default-fold-case FnIntersection
                       (fn [ty _]
                         (-> ty
                           (update-in [:types] #(mapv type-rec %)))))

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

(add-default-fold-case Bounds
                       (fn [ty _]
                         (visit-bounds ty type-rec)))

(add-default-fold-case Projection
                       (fn [ty _]
                         (-> ty
                           (update-in [:ts] #(mapv type-rec %)))))

(add-default-fold-case DottedPretype
                       (fn [ty _]
                         (-> ty
                           (update-in [:pre-type] type-rec))))

(add-default-fold-case Function
                       (fn [ty _]
                         (-> ty
                           (update-in [:dom] #(mapv type-rec %))
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

(add-default-fold-case TApp
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

(add-default-fold-case TypeFn
                       (fn [ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (TypeFn-body* names ty)
                               bbnds (TypeFn-bbnds* names ty)]
                           (TypeFn* names 
                                    (.variances ty)
                                    (doall 
                                      (for [bnd bbnds]
                                        (-> bnd
                                          (update-in [:upper-bound] type-rec)
                                          (update-in [:lower-bound] type-rec))))
                                    (type-rec body)))))


(add-default-fold-case Poly
                       (fn [ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (Poly-body* names ty)
                               bbnds (Poly-bbnds* names ty)]
                           (Poly* names 
                                  (doall 
                                    (for [bnd bbnds]
                                      (-> bnd
                                        (update-in [:upper-bound] type-rec)
                                        (update-in [:lower-bound] type-rec))))
                                  (type-rec body)
                                  (Poly-free-names* ty)))))

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

;; FIXME much better algorithms around I'm sure
(defn countrange-overlap? 
  [{lowerl :lower upperl :upper :as l}
   {lowerr :lower upperr :upper :as r}]
  {:pre [(CountRange? l)
         (CountRange? r)]}
  (cond 
    (and upperl upperr)
        (or 
          ;; -----
          ;;   -------
          ;; and
          ;;   ---
          ;;   -------
          (<= lowerl lowerr upperl upperr)

          ;;    --
          ;;   -------
          (<= lowerr lowerl upperl upperr)

          ;;     ------
          ;; -------
          ;; and
          ;;     ---
          ;; -------
          (<= lowerr lowerl upperr upperl)

          ;; otherwise no overlap
          false)

    upperl ;; and (not upperr)
      (or 
        ;; ----
        ;;  ----->>
        ;; and
        ;;  ---
        ;;  ----->>
        (<= lowerl lowerr upperl)
        ;;   ---
        ;;  ----->>
        (<= lowerr lowerl)
        ;; otherwise no overlap
        false)
    upperr
      (or
        ;; ------>>
        ;;  ----
        ;; and
        ;;  ----->>
        ;;  ---
        (<= lowerl lowerr)
        
        ;;   --->>
        ;; ----
        (<= lowerr lowerl upperr)

        ;; else no overlap
        false)
    :else ;; (and (not upperl) (not upperr))
    ;; ---->>
    ;;   -->>
    ;; and
    ;;   -->>
    ;; ---->>
    true))


;true if types t1 and t2 overlap (NYI)
(defn overlap [t1 t2]
  (cond 
    (= t1 t2) true
    ;if both are Classes, and at least one isn't an interface, then they must be subtypes to have overlap
    (and (RClass? t1)
         (RClass? t2)
         (let [{t1-flags :flags} (reflect/type-reflect (RClass->Class t1))
               {t2-flags :flags} (reflect/type-reflect (RClass->Class t2))]
           (some (complement :interface) [t1-flags t2-flags])))
    (or (subtype? t1 t2)
        (subtype? t2 t1))
    (or (Value? t1)
        (Value? t2)) (or (subtype? t1 t2)
                         (subtype? t2 t1))
    (and (CountRange? t1)
         (CountRange? t2)) (countrange-overlap? t1 t2)
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

    (not (overlap t1 t2)) (Un) ;there's no overlap, so the restriction is empty

    (Union? t1) (apply Un (map (fn [e] (restrict e t2)) (:types t1)))
    (Union? t2) (apply Un (map (fn [e] (restrict t1 e)) (:types t2)))

    (Poly? t2)
    (let [names (repeatedly (:nbound t2) gensym)
          t (Poly-body* names t2)
          bbnds (Poly-bbnds* names t2)
          subst (try 
                  (infer (zipmap names bbnds) {} (list t1) (list t) t1)
                  (catch IllegalArgumentException e
                    (throw e))
                  (catch Exception e))]
      (and subst (restrict t1 (subst-all subst t1))))

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

;remove opposites in and filter
(defn remove-opposite [and-f atom-f]
  {:pre [(Filter? and-f)
         (Filter? atom-f)]
   :post [(Filter? %)]}
  (if (AndFilter? and-f)
    (apply -and (remove #(opposite? % atom-f) (:fs and-f)))
    and-f))

(defn -or [& args]
  (loop [new-props (set args)
         atoms #{}
         last-props #{} ;stop iteration when (= (set/union new-props atoms) last-props)
         ]
    (assert ((set-c? atomic-filter?) atoms))
    (assert (every? (set-c? Filter?) [new-props last-props]))
    (cond
      ;reached fixed point
      (= (set/union new-props atoms) last-props)
      (case (count last-props)
        0 -bot
        1 (first last-props)
        (->OrFilter last-props))

      :else
      (let [;flatten OrFilters
            original-props (set/union new-props atoms)
            original-atoms atoms
            fs (-> (apply concat
                          (for [a (set/union new-props atoms)]
                            (if (OrFilter? a)
                              (:fs a)
                              [a])))
                 set (disj -bot))
            {:keys [atoms] old-props :props} (group-by #(cond
                                                          ((some-fn TypeFilter? NotTypeFilter?) %) :atoms
                                                          :else :props)
                                                       fs)
            ;simplify AndFilters by removing atomic props directly inside the AndFilter
            ;if they are opposite of any atomic props we already have
            next-props (doall
                         (for [p old-props]
                           (reduce (fn [p a] (remove-opposite p a))
                                   p atoms)))
            {:keys [atoms] new-props :props} (group-by #(cond
                                                          ((some-fn TypeFilter? NotTypeFilter?) %) :atoms
                                                          :else :props)
                                                       (set/union (set next-props) (set atoms)))]
        (assert (<= (count original-atoms) (count atoms)))
        (recur (set new-props) (set atoms) (set original-props))))))

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
    ;; P -> tt = tt for any P
    (TopFilter? c) -top
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

(defmethod unparse-filter* TopFilter [f] 'tt)
(defmethod unparse-filter* BotFilter [f] 'ff)

(declare unparse-type)

(defmethod unparse-filter* TypeFilter
  [{:keys [type path id]}]
  (concat (list 'is (unparse-type type) id)
          (when (seq path)
            [(map unparse-path-elem path)])))

(defmethod unparse-filter* NotTypeFilter
  [{:keys [type path id]}]
  (concat (list '! (unparse-type type) id)
          (when path
            [(map unparse-path-elem path)])))

(defmethod unparse-filter* AndFilter [{:keys [fs]}] (apply list '& (map unparse-filter fs)))
(defmethod unparse-filter* OrFilter [{:keys [fs]}] (apply list '| (map unparse-filter fs)))

(defmethod unparse-filter* ImpFilter
  [{:keys [a c]}]
  (list 'when (unparse-filter a) (unparse-filter c)))

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

(defrecord TCResult [t fl o]
  "This record represents the result of typechecking an expression"
  [(AnyType? t)
   (FilterSet? fl)
   (RObject? o)])

(declare-AnyType TCResult)

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

(def -kpe ->KeyPE)

(declare-path-elem FirstPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)

(defmulti unparse-path-elem class)
(defmethod unparse-path-elem KeyPE [t] (list 'Key (:val t)))
(defmethod unparse-path-elem CountPE [t] 'Count)
(defmethod unparse-path-elem ClassPE [t] 'Class)

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
  pathelem being applied first (think of -> threading operator)."
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
(defmethod unparse-object Path [{:keys [path id]}] (conj {:id id} (when (seq path) [:path (mapv unparse-path-elem path)])))

(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty _]
                         (-> ty
                           (update-in [:path] #(when %
                                                 (mapv pathelem-rec %))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted Variable Environment

;symbol -> F
(def ^:dynamic *dotted-scope* {})
(set-validator! #'*dotted-scope* (hash-c? symbol? F?))

(defn bound-index? [n]
  (contains? *dotted-scope* n))

(defmacro with-dotted [dvars & body]
  `(with-dotted-mappings (into {} (for [v# ~dvars]
                                    [(:name v#) v#]))
     ~@body))

(defmacro with-dotted-mappings [dvar-map & body]
  `(binding [*dotted-scope* (merge *dotted-scope* ~dvar-map)]
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
(set-validator! METHOD-OVERRIDE-ENV (hash-c? (every-pred namespace symbol?)
                                             (some-fn Poly? FnIntersection?)))

(defn add-method-override [sym t]
  (swap! METHOD-OVERRIDE-ENV assoc sym t)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor Override Env

(defonce CONSTRUCTOR-OVERRIDE-ENV 
  (atom {}
        :validator (hash-c? symbol? Type?)))

(defn add-constructor-override [sym t]
  (swap! CONSTRUCTOR-OVERRIDE-ENV assoc sym t)
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
    (boolean (or (= :all as)
                 (when as
                   (as arity))))))

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
;; Declared kind Env

(defonce DECLARED-KIND-ENV (atom {}))
(set-validator! DECLARED-KIND-ENV (hash-c? (every-pred symbol? namespace) TypeFn?))

(defn add-declared-kind [sym tfn]
  (swap! DECLARED-KIND-ENV assoc sym tfn))

(defn get-declared-kind [sym]
  (if-let [tfn (@DECLARED-KIND-ENV sym)]
    tfn
    (throw (Exception. (error-msg "No declared kind for Name " sym)))))

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
  (swap! TYPE-NAME-ENV assoc sym (if (Type? ty)
                                   (vary-meta ty assoc :from-name sym)
                                   ty))
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

(declare error-msg)

(defn- resolve-name* [sym]
  (let [t (@TYPE-NAME-ENV sym)]
    (cond
      (= protocol-name-type t) (resolve-protocol sym)
      (= datatype-name-type t) (resolve-datatype sym)
      (= declared-name-type t) (throw (IllegalArgumentException. (str "Reference to declared but undefined name " sym)))
      (Type? t) (vary-meta t assoc :source-Name sym)
      :else (throw (IllegalArgumentException. (error-msg "Cannot resolve name " sym))))))

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

(load "parse")
(load "unparse")
(load "frees")
(load "promote_demote")

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

(defrecord cset-entry [fixed dmap projections]
  ""
  [((hash-c? symbol? c?) fixed)
   (dmap? dmap)
   ((set-c? (hvector-c? (some-fn Type? Projection?)
                        (some-fn Type? Projection?)))
     projections)])

(defn make-cset-entry
  ([fixed] (make-cset-entry fixed nil nil))
  ([fixed dmap] (make-cset-entry fixed dmap nil))
  ([fixed dmap projections] (->cset-entry fixed 
                                          (or dmap (->dmap {}))
                                          (or projections #{}))))

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
                         (->dmap {})
                         #{})]))

(defn empty-cset-projection [X Y proj]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {})
                         #{proj})]))

(defn meet [s t] (In s t))
(defn join [s t] (Un s t))

(declare subtype type-error)

(defn c-meet [{S  :S X  :X T  :T bnds  :bnds :as c1}
              {S* :S X* :X T* :T bnds* :bnds :as c2}
              & [var]]
  #_(prn "c-meet" c1 c2)
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
  (let [maps (doall (for [[{map1 :fixed dmap1 :dmap prj1 :projections} 
                           {map2 :fixed dmap2 :dmap prj2 :projections}] (map vector maps1 maps2)]
                      (->cset-entry (merge-with c-meet map1 map2)
                                    (dmap-meet dmap1 dmap2)
                                    (set/union prj1 prj2))))]
    (when (empty? maps)
      (throw (Exception. (str "No meet found for csets"))))
    (->cset maps)))

(defn cset-meet* [args]
  {:pre [(every? cset? args)]
   :post [(cset? %)]}
  (reduce (fn [a c] (cset-meet a c))
          (->cset [(->cset-entry {} (->dmap {}) #{})])
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
                            dmap
                            #{})))))

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

(load "cs_gen")
(load "subst_dots")
(load "infer")
(load "tvar_rep")

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

(load "trans")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys]
  {:pre [((some-fn Poly? PolyDots?) ptype)
         (seq argtys)
         (every? Type? argtys)]
   :post [(Type? %)]}
  (cond
    (Poly? ptype)
    (let [_ (assert (= (.nbound ptype) (count argtys)) (error-msg "Wrong number of arguments to instantiate polymorphic type"))
          names (repeatedly (.nbound ptype) gensym)
          body (Poly-body* names ptype)
          bbnds (Poly-bbnds* names ptype)]
      (doseq [[nme ty bnds] (map vector names argtys bbnds)]
        (if (.higher-kind bnds)
          (do 
            (if (F? ty)
              (assert (and (TypeFn? (.higher-kind bnds))
                           (let [given-bnds (free-with-name-bnds (.name ty))
                                 _ (assert given-bnds *free-scope*)]
                             (and (.higher-kind given-bnds)
                                  (subtype? (.higher-kind given-bnds) (.higher-kind bnds)))))
                      (error-msg "Must instantitate higher-order type variable with another higher-order type variable, given: "
                                 (unparse-type ty)))
              (do 
                (assert (TypeFn? ty) (error-msg "Must instantiate higher-order type variable with type function, given:"
                                                (unparse-type ty)))
                (assert (subtype? ty (.higher-kind bnds))
                        (error-msg "Higher-order type variable " (unparse-type ty)
                                   " does not match bound " (unparse-type (.higher-kind bnds)))))))
          (let [lower-bound (substitute-many (.lower-bound bnds) argtys names)
                upper-bound (substitute-many (.upper-bound bnds) argtys names)]
            (assert (subtype? lower-bound upper-bound)
                    (error-msg "Lower-bound " (unparse-type lower-bound)
                               " is not below upper-bound " (unparse-type upper-bound)))
            (assert (and (subtype? ty upper-bound)
                         (subtype? lower-bound ty))
                    (error-msg "Manually instantiated type " (unparse-type ty)
                               " is not between bounds " (unparse-type lower-bound)
                               " and " (unparse-type upper-bound))))))
      (substitute-many body argtys names))

    (PolyDots? ptype)
    (let [nrequired-types (dec (.nbound ptype))
          _ (assert (<= nrequired-types (count argtys)) "Insufficient arguments to instantiate dotted polymorphic type")
          names (repeatedly (.nbound ptype) gensym)
          body (PolyDots-body* names ptype)
          bbnds (PolyDots-bbnds* names ptype)]
      (doseq [[nme ty bnds] (map vector names argtys bbnds)]
        (assert (not (.higher-kind bnds)) "NYI")
        (let [lower-bound (substitute-many (.lower-bound bnds) argtys names)
              upper-bound (substitute-many (.upper-bound bnds) argtys names)]
          (assert (subtype? lower-bound upper-bound)
                  (error-msg "Lower-bound " (unparse-type lower-bound)
                             " is not below upper-bound " (unparse-type upper-bound)))
          (assert (and (subtype? ty upper-bound)
                       (subtype? lower-bound ty))
                  (error-msg "Manually instantiated type " (unparse-type ty)
                             " is not between bounds " (unparse-type lower-bound)
                             " and " (unparse-type upper-bound)))))
      (-> body
        ; expand dotted pre-types in body
        (trans-dots (last names) ;the bound
                    (drop (dec (:nbound ptype)) argtys)) ;the types to expand pre-type with
        ; substitute normal variables
        (substitute-many (take nrequired-types argtys) (butlast names))))))

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
