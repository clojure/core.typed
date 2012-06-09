(ns typed.new
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry))
  (:require [analyze.core :refer [ast] :as analyze]
            [clojure.set :as set]
            [trammel.core :as contracts]
            [clojure.tools.trace :refer [trace trace-ns]]))

(defn inst-poly [inst-of types-syn]
  inst-of)

(defmacro inst [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defn fn>-ann [fn-of param-types-syn]
  fn-of)

(defmacro fn> [arg-anns & body]
  (let [[required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)
        ;(fn> [[a :- Number] & [n :- Number *]] a)
        _ (assert (not rest-param) "fn> doesn't support rest parameters yet")
        params (map first required-params)
        param-types (map (comp second next) arg-anns)]
    `(fn>-ann (fn ~(vec params) ~@body) '~param-types)))

(defmacro defrecord [name slots inv-description invariants & etc]
  `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc))

(declare abstract-many instantiate-many)

;(defn- comp-mm [mm disps]
;  (set/difference disps (set (keys (methods mm)))))
;
;(comp-mm replace-image (disj kinds :scope))
;(comp-mm replace-image (disj kinds :scope))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(def nat? (every-pred (complement neg?) integer?))

(def Type ::Type)

(defn Type? [a]
  (isa? (class a) Type))

(defn declare-type [a]
  (derive a Type))

(defrecord Top []
  "The top type"
  [])

(declare-type Top)

;TODO flatten unions
(defrecord Union [types]
  "An unordered union of types"
  [(every? Type? types)])

(declare-type Union)

(defn Un [types]
  (if (= 1 (count types))
    (first types)
    (->Union (-> types set vec))))

(def empty-union (->Union []))

(defn Bottom []
  empty-union)

(defrecord Intersection [types]
  "An ordered intersection of types"
  [(sequential? types)
   (seq types)
   (every? Type? types)])

(declare-type Intersection)

(def variances #{:constant :covariant :contravariant :invariant})

(defn variance? [v]
  (contains? variances v))

(defrecord B [idx upper-bound lower-bound variance]
  "A bound variable. Should not appear outside this file"
  [(nat? idx)
   (or (nil? upper-bound)
       (Type? upper-bound))
   (or (nil? lower-bound)
       (Type? lower-bound))
   (or (nil? variance)
       (variance? variance))])

(declare-type B)

(defrecord F [name upper-bound lower-bound variance]
  "A named free variable"
  [(symbol? name)
   (or (nil? upper-bound)
       (Type? upper-bound))
   (or (nil? lower-bound)
       (Type? lower-bound))
   (or (nil? variance)
       (variance? variance))])

(declare-type F)

(defrecord Scope [body]
  "A scope that contains bound variables. Not used directly"
  [((some-fn Type? #(instance? Scope %)) body)])

(defrecord RClass [variances the-class replacements]
  "A restricted class, where ancestors are
  (replace replacements (ancestors the-class))"
  [(or (nil? variances)
       (and (sequential? variances)
            (every? #(or (nil? %)
                         (variance? %))
                    variances)))
   (class? the-class)
   (map? replacements)
   (every? class? (keys replacements))
   (every? (some-fn Type? Scope?) (vals replacements))])

;smart constructor
(defn RClass* [tparams the-class replacements]
  (if (seq tparams)
    (->RClass (doall (map :variance tparams))
              the-class
              (into {} (for [[k v] replacements]
                         [k (abstract-many (map :name tparams) v)])))
    (->RClass nil the-class replacements)))

;smart destructor
(defn RClass-replacements* [names rclass]
  (into {} (for [[k v] (:replacements rclass)]
             [k (instantiate-many (map #(->F % nil nil nil) names) v)])))

(declare-type RClass)

(defrecord RInstance [poly? constructor]
  "An instance of a class"
  [(or (nil? poly?)
       (and (sequential? poly?)
            (every? Type? poly?)))
   (RClass? constructor)])

(declare-type RInstance)

(defrecord Field [name type]
  "A datatype/record field"
  [(symbol? name)
   (Type? type)])

(defrecord Record [the-class fields]
  "A record"
  [(class? the-class)
   (every? Field? fields)])

(declare-type Record)

(defrecord DataType [the-class fields]
  "A Clojure datatype"
  [(class? the-class)
   (every? Field? fields)])

(declare-type DataType)

(defrecord Poly [nbound scope]
  "A polymorphic type containing n bound variables"
  [(nat? nbound)
   (Scope? scope)])

(declare-type Poly)

;smart constructor
(defn Poly* [names body]
  {:pre [(every? symbol names)
         (Type? body)]}
  (if (empty? names)
    body
    (->Poly (count names) (abstract-many names body))))

;smart destructor
(defn Poly-body* [names poly]
  {:pre [(every? symbol? names)
         (Poly? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (instantiate-many (map #(->F % nil nil nil) names) (:scope poly)))

(defrecord PolyDots [n scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(nat? n)
   (Scope? scope)])

(declare-type PolyDots)

;smart constructor
(defn PolyDots* [names body]
  {:pre [(every? symbol names)
         (Type? body)]}
  (if (empty? names)
    body
    (->PolyDots (count names) (abstract-many names body))))

(defrecord Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)])

(declare-type Mu)

(defrecord Value [val]
  "A Clojure value"
  [(not (nil? val))])

(declare-type Value)

(defrecord HeterogeneousMap [types]
  "A constant map, clojure.lang.PersistentHashMap"
  [(every? #(and (= 2 (count %))
                 (let [[k v] %]
                   (and (Type? k)
                        (Type? v))))
           types)])

(declare-type HeterogeneousMap)

(defrecord HeterogeneousVector [types]
  "A constant vector, clojure.lang.PersistentVector"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousVector)

(defrecord Function [dom rng rest drest]
  "A function arity"
  [(sequential? dom)
   (every? Type? dom)
   (Type? rng)
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (Type? drest))])

(declare-type Function)

(defn Fn-Intersection [fns]
  {:pre [(every? Function? fns)]}
  (->Intersection fns))

(declare abstract)

;smart constructor
(defn Mu* [name body]
  (->Mu (abstract name body)))

(defrecord Nil []
  "Type for nil"
  [])

(declare-type Nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(def VAR-ANNOTATIONS (atom {}))
(def ^:dynamic *local-annotations* {})

(defmacro ann [varsym typesyn]
  `(let [t# (parse-type '~typesyn)
         s# (symbol '~varsym)]
     (do (swap! VAR-ANNOTATIONS #(assoc % s# t#))
       [s# (unparse-type t#)])))

(defn lookup-local [sym]
  (*local-annotations* sym))

(defn lookup-var [var]
  (let [nsym (symbol (str (ns-name (.ns ^Var var)))
                     (str (.sym ^Var var)))]
    (assert (contains? @VAR-ANNOTATIONS nsym) (str "Untyped var reference: " var))
    (@VAR-ANNOTATIONS nsym)))

(defmacro with-locals [locals & body]
  `(binding [*local-annotations* (merge *local-annotations* ~locals)]
     ~@body))

(defn type-of [sym-or-var]
  (cond
    (symbol? sym-or-var) (if-let [t (lookup-local sym-or-var)]
                           t
                           (throw (Exception. (str "Cannot resolve type: " sym-or-var))))
    (var? sym-or-var) (lookup-var sym-or-var)
    :else (throw (Exception. (str "Cannot resolve type: " sym-or-var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

;Class -> RClass
(def RESTRICTED-CLASS (atom {}))

(declare parse-binder with-frees)

(defn- build-replacement-syntax [m]
  (into {} (for [[k v] m]
             [k `(parse-type '~v)])))

(defmacro alter-class [the-class frees-syn & opts]
  (let [{replacements-syn :replace} (apply hash-map opts)
        replacements (build-replacement-syntax replacements-syn)]
     `(let [frees# (when-let [fs# (seq '~frees-syn)]
                     (parse-binder fs#))]
        (swap! RESTRICTED-CLASS 
               #(assoc % ~the-class (RClass* frees# ~the-class (with-frees frees#
                                                                   ~replacements))))
        ~the-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type syntax

;(Map Symbol F)
(def ^:dynamic *free-scope* {})

(defmacro with-frees [frees & body]
  `(let [m# (zipmap (map :name ~frees) ~frees)]
     (binding [*free-scope* (merge *free-scope* m#)]
       ~@body)))

(defmulti parse-type class)
(defmulti parse-type-list first)

(defn parse-free [f]
  (if (symbol? f)
    (->F f nil nil nil)
    (let [[n & opts] f
          {upp :<
           low :>
           variance :variance} (apply hash-map opts)]
      (->F n 
           (when upp 
             (parse-type upp)) 
           (when low
             (parse-type low))
           variance))))

(defn check-forbidden-rec [rec tbody]
  (when (or (= rec tbody) 
            (and (Intersection? tbody)
                 (contains? (set (:types tbody)) rec))
            (and (Union? tbody)
                 (contains? (set (:types tbody)) rec)))
    (throw (Exception. "Recursive type not allowed here"))))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [_ (assert (= 1 (count bnder)) "Only one variable in allowed: Rec")
        f (->F free-symbol nil nil nil)
        body (with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

(defmethod parse-type-list 'Rec
  [syn]
  (parse-rec-type syn))

(defn parse-binder [bnds]
  (let [scope (atom [])]
    (doall
      (for [bnd bnds]
        (let [f (with-frees @scope
                  (parse-free bnd))
              _ (swap! scope #(conj % f))]
          f)))))

(defn parse-all-type [[all bnds type]]
  (let [frees (parse-binder bnds)]
    (Poly* (map :name frees)
           (with-frees frees
             (parse-type type)))))

(defmethod parse-type-list 'All
  [syn]
  (parse-all-type syn))

(defn parse-union-type [[u & types]]
  (Un (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (->Intersection (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defn parse-fn-intersection-type [[Fn & types]]
  (Fn-Intersection (doall (map parse-type types))))

(defmethod parse-type-list 'Fn
  [syn]
  (parse-fn-intersection-type syn))

(defn parse-rinstance-type [[cls-sym & params-syn]]
  (let [cls (resolve cls-sym)
        _ (assert cls (str cls-sym " does not resolve to a class"))
        rclass (@RESTRICTED-CLASS cls)
        _ (assert rclass (str cls " not declared as polymorphic"))
        tparams (doall (map parse-type params-syn))]
    (->RInstance tparams rclass)))

(declare constant-type)

(defmethod parse-type-list 'Value
  [[Value syn]]
  (constant-type syn))

(defmethod parse-type-list :default
  [syn]
  (parse-rinstance-type syn))

(defmethod parse-type Cons
  [l]
  (parse-type-list l))

(defmethod parse-type IPersistentList
  [l]
  (parse-type-list l))

(defmulti parse-type-symbol identity)
(defmethod parse-type-symbol 'Any [_] (->Top))
(defmethod parse-type-symbol 'Nothing [_] (Bottom))

(defmethod parse-type-symbol :default
  [sym]
  (cond
    (sym *free-scope*) (sym *free-scope*)

    :else
    (let [res (resolve sym)]
      (cond 
        ;make instance, must provide type parameters if any
        (class? res) (let [rclass (@RESTRICTED-CLASS res)
                           _ (assert (empty? (:variances rclass)) (str "RClass " res " must be instantiated"))]
                       (->RInstance nil (or rclass (->RClass nil res {}))))

        :else (throw (Exception. (str "Cannot resolve type: " sym)))))))

(defmethod parse-type Symbol
  [l]
  (parse-type-symbol l))

(defmethod parse-type nil
  [_]
  (->Nil))

(defn parse-function [f]
  (let [[all-dom _ [rng]] (partition-by #(= '-> %) f)

        _ (assert (not (and (some #(= '& %) all-dom)
                            (some #(= '... %) all-dom)))
                  "Cannot provide both rest type and dotted rest type")

        fixed-dom (take-while #(not (or (= '& %)
                                        (= '... %)))
                              all-dom)

        [_ rest-type] (drop-while #(not= '& %) all-dom)
        [_ drest-type] (drop-while #(not= '... %) all-dom)]
    (->Function (doall (map parse-type fixed-dom))
                (parse-type rng) 
                (when rest-type
                  (parse-type rest-type))
                (when drest-type
                  (parse-type drest-type)))))

(defmethod parse-type IPersistentVector
  [f]
  (parse-function f))

(defmulti unparse-type class)

(defmethod unparse-type Nil [n] 'nil)
(defmethod unparse-type Top [t] 'Any)

(defmethod unparse-type F
  [{:keys [name]}]
  name)

(defmethod unparse-type Union
  [{types :types}]
  (list* 'U (doall (map unparse-type types))))

(defmethod unparse-type Intersection
  [{types :types}]
  (list* 'I (doall (map unparse-type types))))

(defmethod unparse-type Function
  [{:keys [dom rng rest drest]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 ['& (unparse-type rest)])
               (when drest
                 ['... (unparse-type drest)])
               ['-> (unparse-type rng)])))

(defmethod unparse-type RClass
  [{the-class :the-class}]
  (symbol (.getName the-class)))

(defmethod unparse-type RInstance
  [{poly? :poly? constructor :constructor}]
  (if (empty? poly?)
    (unparse-type constructor)
    (list* (unparse-type constructor)
           (doall (map unparse-type poly?)))))

(defmethod unparse-type Poly
  [p]
  (let [fs (vec 
             (for [x (range (:nbound p))]
               (gensym)))
        body (Poly-body* fs p)]
    (list 'All fs (unparse-type body))))

(defmethod unparse-type Value
  [v]
  (list 'Value (:val v)))

(defmethod unparse-type HeterogeneousMap
  [v]
  (list* 'HetMap (doall (map (fn [[k v]]
                               (vector (unparse-type k)
                                       (unparse-type v)))
                             (:types v)))))

(defmethod unparse-type HeterogeneousVector
  [v]
  (list* 'HetVector (doall (map unparse-type (:types v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(defn add-scopes [n t]
  "Wrap type in n Scopes"
  {:pre [(nat? n)
         (Type? t)]}
  (doall
    (last 
      (take (inc n) (iterate ->Scope t)))))

(defn remove-scopes [n sc]
  "Unwrap n Scopes"
  {:pre [(nat? n)
         (Type? sc)]}
  (doall
    (last
      (take (inc n) (iterate (fn [{body :body :as t}]
                               (assert (Scope? t) "Tried to remove too many Scopes")
                               body)
                             sc)))))

(defmulti name-to
  "Convert a name in the type to de Bruijn index of res,
  where n is the position in the current binder, and outer is the
  number of indices previously bound"
  (fn [ty name res]
    {:pre [(Type? ty)
           (symbol? name)
           (nat? res)]}
    (class ty)))

(defmethod name-to B [ty name res] ty)

(defmethod name-to F
  [{name* :name upper :upper-bound lower :lower-bound variance :variance :as ty} name res] 
  (if (= name name*)
    (->B res upper lower variance)
    ty))

(defmethod name-to Function
  [f name res]
  (let [ufn #(name-to % name res)]
    (-> f
      (update-in [:dom] #(doall (map ufn %)))
      (update-in [:rng] ufn)
      (update-in [:rest] #(when %
                            (ufn %)))
      (update-in [:drest] #(when %
                            (ufn %))))))

(defmethod name-to Top
  [t name res]
  t)

(defmethod name-to Nil
  [n name res]
  n)

(defmethod name-to Intersection
  [{:keys [types]} name res]
  (->Intersection (doall (map #(name-to % name res) types))))

(defmethod name-to Union
  [{:keys [types]} name res]
  (->Union (doall (map #(name-to % name res) types))))

;(defmethod name-to RClass
;  [{:keys [variances the-class replacements]} name res]
;  (->RClass variances
;            the-class 
;            (into {} (for [[k v] replacements]
;                       (let [v (remove-scopes (count variances) v)]
;                         [k (name-to v name (+ res (count variances)))])))))

(defmethod name-to RInstance
  [{:keys [poly? constructor]} name res]
  (->RInstance (doall (map #(name-to % name res) poly?))
               constructor)) ;should this call name-to?

(defmethod name-to Poly
  [{n :nbound scope :scope} name res]
  (let [body (remove-scopes n scope)]
    (->Poly n (add-scopes n (name-to body name (+ n res))))))

(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (iterate dec (dec (count c))) c))

(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         (Type? ty)]}
  (let [n (count names)]
    (->> 
      ;convert each given name to a bound de Bruijn index
      (reduce (fn [ty [cnt name]]
                (name-to ty name cnt))
              ty
              (rev-indexed names))
      ;then wrap n scopes
      (add-scopes n))))

(defmulti replace-image
  "Replace all bound variables with index target, with
  the free variable image, keeping bound variable's upper/lower bounds"
  (fn [type image target]
    {:pre [(Type? type)
           (F? image)
           (nat? target)]}
    (class type)))

(defmethod replace-image F [ty image target] ty)

(defmethod replace-image B
  [{idx :idx upper :upper-bound lower :lower-bound variance :variance :as ty} image target]
  (if (= idx target)
    (assoc image
           :upper-bound upper
           :lower-bound lower
           :variance variance)
    ty))

(defmethod replace-image Union
  [{types :types} image target]
  (->Union (doall (map #(replace-image % image target) types))))

(defmethod replace-image Nil [n image target] n)
(defmethod replace-image Top [t image target] t)

(defmethod replace-image RInstance
  [r image target]
  (let [ufn #(replace-image % image target)]
    (-> r
      (update-in [:poly?] #(when %
                             (doall (map ufn %)))))))

(defmethod replace-image Function
  [f image target]
  (let [ufn #(replace-image % image target)]
    (-> f
      (update-in [:dom] #(doall (map ufn %)))
      (update-in [:rng] ufn)
      (update-in [:rest] #(when %
                            (ufn %)))
      (update-in [:drest] #(when %
                            (ufn %))))))

(defmethod replace-image Intersection
  [{types :types} image target]
  (->Intersection (doall (map #(replace-image % image target) types))))

(defmethod replace-image Poly
  [{scope :scope n :nbound :as ty} image target]
  (let [body (remove-scopes n scope)]
    (assoc ty :scope (add-scopes n (replace-image body image (+ target n))))))

(defn instantiate-many 
  "instantiate-many : List[Type] Scope^n -> Type
  where n is the length of types
  all of the types MUST be Fs"
  [images sc]
  {:pre [(every? F? images)
         (Scope? sc)]}
  (let [n (count images)]
    (reduce (fn [ty [cnt image]]
              (replace-image ty image cnt))
            (remove-scopes n sc)
            (rev-indexed images))))

(defn abstract [name ty]
  "Make free name bound"
  {:pre [(symbol? name)
         (Type? ty)]}
  (abstract-many [name] ty))

(defn instantiate [f sc]
  "Instantiate bound name to free"
  {:pre [(F? f)
         (Scope? sc)]}
  (instantiate-many [f] sc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(declare subtype)

(defmulti substitute (fn [target image name] (class target)))

(defn substitute-many [target images names]
  (reduce (fn [t [im nme]] (substitute t im nme))
          target
          (map vector images names)))

(defmethod substitute F
  [{name* :name :keys [upper-bound lower-bound] :as f} image name]
  (if (= name* name)
    (do 
      (when upper-bound
        (subtype image upper-bound))
      (when lower-bound
        (subtype lower-bound image))
      image)
    f))

(defmethod substitute Nil [t image name] t)
(defmethod substitute Top [t image name] t)

(defmethod substitute RInstance
  [rinst image name]
  (let [sub #(substitute % image name)]
    (-> rinst
      (update-in [:poly?] #(when %
                             (doall (map sub %)))))))

(defmethod substitute Union
  [u image name]
  (let [sub #(substitute % image name)]
    (-> u
      (update-in [:types] #(doall (map sub %))))))

(defmethod substitute Intersection
  [i image name]
  (let [sub #(substitute % image name)]
    (-> i
      (update-in [:types] #(doall (map sub %))))))

(defmethod substitute Function
  [f image name]
  (let [sub #(substitute % image name)]
    (-> f
      (update-in [:dom] #(doall (map sub %)))
      (update-in [:rng] #(sub %))
      (update-in [:rest] #(when %
                            (sub %)))
      (update-in [:drest] #(when %
                             (sub %))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtypes

(defn type-error [s t]
  (throw (Exception. (str (unparse-type s) 
                          " is not a subtype of: " 
                          (unparse-type t)))))

(def ^:dynamic *A*)
(def ^:dynamic *A0*)

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

(defn subtypeA* [A s t]
  (cond
    (or (contains? A [s t])
        (= s t))
    A

    (Union? s)
    (binding [*A* A
              *A0* (conj A [s t])]
      (if (every? #(subtypeA* *A0* % t) (:types s))
        *A0*
        (type-error s t)))

    (Union? t)
    (binding [*A* A
              *A0* (conj A [s t])]
      (if (some #(subtypeA*? *A0* s %) (:types t))
        *A0*
        (type-error s t)))

    (Intersection? s)
    (binding [*A* A
              *A0* (conj A [s t])]
      (if (some #(subtypeA*? *A0* % t) (:types s))
        *A0*
        (type-error s t)))

    (Intersection? t)
    (binding [*A* A
              *A0* (conj A [s t])]
      (if (every? #(subtypeA* *A0* s %) (:types s))
        *A0*
        (type-error s t)))

    :else
    (binding [*A* A
              *A0* (conj A [s t])]
      (subtype* s t))))

(defn subtype [s t]
  (subtypeA* #{} s t))

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
      *A0*
      (type-error s t))))

; (Cons Integer) <: (Seqable Integer)
; (ancestors (Seqable Integer)

(defmethod subtype* [Value RInstance]
  [{val :val} t]
  (let [cls (class val)]
    (subtype* (->RInstance nil (or (@RESTRICTED-CLASS cls)
                                   (->RClass nil cls {})))
              t)))

(defn- RInstance-supers* 
  "Return a set of Types that are the super-Types
  of this RInstance"
  [{:keys [poly? constructor] :as rinst}]
  {:pre [(RInstance? rinst)]
   :post [(every? Type? %)]}
  (let [names (map gensym (range (count poly?)))
        rplce (RClass-replacements* names constructor)
        rplce-subbed (into {} (for [[k v] rplce]
                                [k (substitute-many v poly? names)]))
        ancest (supers (:the-class constructor))
        not-replaced (set/difference ancest (keys rplce-subbed))
        super-types (set/union (set (for [t not-replaced]
                                      (->RInstance nil (or (when-let [r (@RESTRICTED-CLASS t)]
                                                             (assert (empty? (:variances r))
                                                                     (str "RClass " (unparse-type r) " must be instantiated"
                                                                          " in " t))
                                                             r)
                                                           (->RClass nil t {})))))
                               (vals rplce-subbed))]
    super-types))

(defn- subtype-rinstance-common-base 
  [{polyl? :poly? constl :constructor :as s}
   {polyr? :poly? constr :constructor :as t}]
  {:pre [(= constl constr)]}
  (let [{variances :variances} constl]
    (every? identity
            (doall (map #(case %1
                           :covariant (subtype? %2 %3)
                           :contravariant (subtype? %3 %2)
                           (= %2 %3))
                        variances
                        polyl?
                        polyr?)))))

(defmethod subtype* [RInstance RInstance]
  [{polyl? :poly? constl :constructor :as s}
   {polyr? :poly? constr :constructor :as t}]
  (cond 
    (or ;same base class
        (and (= constl constr)
             (subtype-rinstance-common-base s t))

        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(and (= constr (:constructor %))
                    (subtype-rinstance-common-base % t))
              (RInstance-supers* s)))
    *A0*

    ;try each ancestor

    :else (type-error s t)))

(defmethod subtype* [HeterogeneousMap RInstance]
  [s t]
  (let [sk (Un (map first (:types s)))
        sv (Un (map second (:types s)))]
    (if-let [A1 (subtypeA* *A0* 
                           (->RInstance [sk sv] (@RESTRICTED-CLASS IPersistentMap))
                           t)]
      A1
      (type-error s t))))

(defmethod subtype* [HeterogeneousVector RInstance]
  [s t]
  (let [ss (Un (:types s))]
    (if-let [A1 (subtypeA* *A0* 
                           (->RInstance [ss] (@RESTRICTED-CLASS IPersistentVector))
                           t)]
      A1
      (type-error s t))))

(defmethod subtype* [Type Top]
  [s t]
  *A0*)

(defmethod subtype* :default
  [s t]
  (type-error s t))

(defmacro sub [s t]
  `(subtype (parse-type '~s)
            (parse-type '~t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

(alter-class Seqable [[a :variance :covariant]])

(alter-class IPersistentCollection [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)})

(alter-class ISeq [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              IPersistentCollection (IPersistentCollection a)})

(alter-class ILookup [[a :variance :invariant]
                      [b :variance :covariant]])

(alter-class IPersistentSet [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class Associative [[a :variance :invariant]
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
              ISeq (ISeq a)})

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
              ISeq (ISeq a)})

(comment
  (alter-class IPersistentCollection [a])
  (IPersistentCollection Integer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

(ann clojure.core/*ns* Namespace)
(ann clojure.core/ns-name [Namespace -> Symbol])
(ann clojure.core/in-ns [Symbol -> nil])
(ann clojure.core/import [(IPersistentCollection Symbol) -> nil])

(ann clojure.core/symbol
     (Fn [(U Symbol String) -> Symbol]
         [String String -> Symbol]))

(ann clojure.core/seq
     (All [x]
          (Fn [(Seqable x) -> (U nil (ASeq x))]
              [nil -> nil]
              [String -> (U nil (ASeq Character))]
              [(U java.util.Map Iterable) -> (U nil (ASeq Any))])))

(ann clojure.core/first
     (All [x]
          (Fn [(Seqable x) -> (U nil (ASeq x))]
              [nil -> nil]
              [String -> (U nil Character)]
              [(U java.util.Map Iterable) -> (U nil Any)])))

(ann clojure.core/conj
     (All [x]
          (Fn [(U nil (IPersistentCollection x)) x -> (IPersistentCollection x)])))

(ann clojure.core/get
     (All [x]
          (Fn [(ILookup Any x) Any -> (U nil x)]
              [String Any -> (U nil Character)]
              [nil Any -> nil]
              [(IPersistentSet x) Any -> (U nil x)]
              [java.util.Map -> (U nil Any)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(def expr-type ::expr-type)

(defmulti check (fn [expr & [expected]] (:op expr)))

(defn check-top-level [nsym form]
  (let [ast (analyze/analyze-form-in-ns nsym form)]
    (check ast)))

(defmacro tc [form]
  `(check-top-level (symbol (ns-name *ns*))
                    '~form))

(defmulti constant-type class)

(defmethod constant-type Symbol [v] (->Value v))
(defmethod constant-type Long [v] (->Value v))
(defmethod constant-type Double [v] (->Value v))
(defmethod constant-type java.math.BigDecimal [v] (->Value v))
(defmethod constant-type clojure.lang.BigInt [v] (->Value v))
(defmethod constant-type String [v] (->Value v))
(defmethod constant-type Character [v] (->Value v))
(defmethod constant-type clojure.lang.Keyword [v] (->Value v))

(defmethod constant-type IPersistentVector
  [cvec]
  (->HeterogeneousVector (doall (map constant-type cvec))))

(defmethod constant-type IPersistentMap
  [cmap]
  (->HeterogeneousMap (doall (map #(vector (constant-type (first %))
                                           (constant-type (second %)))
                                  cmap))))

(defn check-value
  [{:keys [val] :as expr} & [expected]]
  (let [actual-type (constant-type val)
        _ (when expected
            (subtype actual-type expected))]
    (assoc expr
           expr-type actual-type)))

(defmethod check :constant [& args] (apply check-value args))
(defmethod check :number [& args] (apply check-value args))
(defmethod check :string [& args] (apply check-value args))
(defmethod check :keyword [& args] (apply check-value args))

;Expr Expr^n Type Type^n (U nil Type) -> Type
(defn check-app [fexpr args fexpr-type arg-types expected]
  (assert (not expected) "TODO incorporate expected type")
  (cond
    (Intersection? fexpr-type)
    (let [{ftypes :types} fexpr-type
          _ (assert (every? Function? ftypes) "Must be intersection type containing Functions")
          ;find first 
          success-type (some (fn [{:keys [dom rest drest rng]}]
                               (assert (not (or drest rest)) "TODO rest arg checking")
                               (and (= (count dom)
                                       (count arg-types))
                                    (every? true? (map subtype? arg-types dom))
                                    rng))
                             ftypes)
          _ (when-not success-type
              ;just report first function
              (throw (Exception.
                       (str "Cannot supply arguments " (with-out-str (pr (map unparse-type arg-types)))
                            " to Function " (with-out-str (prn (unparse-type (first ftypes))))))))]
      success-type)


    (Function? fexpr-type)
    (let [{rest-type :rest :keys [dom rng drest]} fexpr-type]
      (cond
        (and (not= (count dom)
                   (count arg-types))
             (not rest-type))
        (throw (Exception. (str "Wrong number of arguments")))

        (and (= (count dom)
                (count arg-types))
             (not rest-type))
        (do (doall (map subtype
                        arg-types
                        dom))
          rng)))

    (Poly? fexpr-type)
    (throw (Exception. "Cannot infer arguments to polymorphic functions"))

    :else (throw (Exception. "Give up"))))

(defmethod check :var
  [{:keys [var] :as expr} & [expected]]
  (assoc expr
         expr-type (lookup-var var)))

(defn- manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [{n :nbound, :as ptype} argtys]
  {:pre [(Poly? ptype)
         (every? Type? argtys)]}
  (assert (= (count argtys) n) "Instatiating polymorphic type with wrong number of arguments")
  (let [names (map gensym (range n))
        body (Poly-body* names ptype)]
    (substitute-many body argtys names)))

(defmethod check :invoke
  [{:keys [fexpr args] :as expr} & [expected]]
  (cond
    ;manual instantiation
    (identical? #'inst-poly (:var fexpr))
    (let [[pexpr targs-exprs] args
          ptype (-> (check pexpr) expr-type)
          targs (doall (map parse-type (:val targs-exprs)))]
      (assoc expr
             expr-type (manual-inst ptype targs)))
    
    ;fn literal
    (identical? #'fn>-ann (:var fexpr))
    (assert false "TODO fn> parameters")

    ;must be monomorphic for now
    :else
    (let [cfexpr (check fexpr)
          cargs (doall (map check args))
          actual (check-app fexpr args (expr-type cfexpr) (map expr-type cargs) expected)]
      (assoc expr
             :fexpr cfexpr
             :args cargs
             expr-type actual))))

(defmethod check :fn-expr
  [{:keys [methods] :as expr} & [expected]]
  (let [cmethods (map check methods)]
    (assoc expr
           :methods cmethods
           expr-type (->Intersection (map expr-type cmethods)))))

(defmethod check :fn-method
  [{:keys [required-params rest-param body] :as expr} & [expected]]
  (let [poly-syn? (or (and (seq required-params)
                           (-> required-params first :sym meta ::poly))
                      (and rest-param
                           (-> rest-param :sym meta ::poly)))
        reqd-syms (map :sym required-params)
        reqd-anns (map #(-> % meta ::ann parse-type) reqd-syms)
        rest-ann (when rest-param
                   (-> rest-param :sym meta ::ann parse-type))
        _ (assert (not rest-param) "Rest parameter not supported")
        cbody (with-locals (zipmap reqd-syms reqd-anns)
                (check body))]
    (assoc expr
           :body cbody
           expr-type (->Function reqd-anns (expr-type cbody) nil nil))))

(defmethod check :do
  [{:keys [exprs] :as expr} & [expected]]
  (let [cexprs (doall (map check exprs))]
    (assoc expr
           :exprs cexprs
           expr-type (-> cexprs last expr-type))))

(defmethod check :local-binding-expr
  [{:keys [local-binding] :as expr} & [expected]]
  (assoc expr
         expr-type (type-of (-> local-binding :sym))))
