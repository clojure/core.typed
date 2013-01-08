(in-ns 'typed.core)

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

(declare symbol->Class)

(defn RClass->Class [rcls]
  (symbol->Class (.the-class rcls)))

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
                (mapv (fn [bnd] 
                        (visit-bounds bnd #(abstract-many names %)))
                      bbnds)
                (abstract-many names body))))

;smart destructor
(defn PolyDots-body* [names poly]
  {:pre [(every? symbol? names)
         (PolyDots? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (.scope poly)))

(defn PolyDots-bbnds* [names poly]
  {:pre [(every? symbol? names)
         (PolyDots? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (visit-bounds b #(instantiate-many names %)))
        (.bbnds poly)))

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
  (assert (TypeFn? t) (str "instantiate-typefn requires a TypeFn: " (unparse-type t)))
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

(defn Value->Class [tval]
  (class (.val tval)))

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
  [(every? (hash-c? Value? Type?) [mandatory optional])])

(defrecord Function [dom rng rest drest kws]
  "A function arity, must be part of an intersection"
  [(or (nil? dom)
       (sequential? dom))
   (every? Type? dom)
   (Result? rng)
   (<= (count (remove nil? [rest drest kws])) 1)
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

(declare FilterSet?)

(defrecord TCResult [t fl o]
  "This record represents the result of typechecking an expression"
  [(Type? t)
   (FilterSet? fl)
   (RObject? o)])

(declare-AnyType TCResult)
