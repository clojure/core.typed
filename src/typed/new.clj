(ns typed.new
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup))
  (:require [analyze.core :as analyze]
            [clojure.set :as set]
            [trammel.core :as contracts]
            [clojure.tools.trace :refer [trace trace-ns]]))

(defmacro defrecord [name slots inv-description invariants & etc]
  `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc))

(declare abstract-many)

(defn- comp-mm [mm disps]
  (set/difference disps (set (keys (.getMethodTable mm)))))

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

(defn Bottom []
  (->Union []))

(defrecord Intersection [types]
  "An ordered intersection of types"
  [(sequential? types)
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

(defrecord Scope [body]
  "A scope that contains bound variables. Not used directly"
  [(Type? body)])

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

(defrecord Function [dom rng rest drest]
  "A function arity"
  [(sequential? dom)
   (every? Type? dom)
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (Type? drest))])

(declare-type Function)

(declare abstract)

;smart constructor
(defn Mu* [name body]
  (->Mu (abstract name body)))

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
  (->Union (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (->Intersection (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defmethod parse-type-list :default
  [[cls-sym & params-syn]]
  (let [cls (resolve cls-sym)
        rclass (@RESTRICTED-CLASS cls)
        _ (assert rclass (str cls " not declared as polymorphic"))
        tparams (doall (map parse-type params-syn))]
    (->RInstance tparams rclass)))

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
                           _ (assert (not (:variances rclass)) (str "RClass " res " must be instantiated"))]
                       (->RInstance nil (or rclass (->RClass nil res {}))))

        :else (throw (Exception. (str "Cannot resolve type: " res)))))))

(defmethod parse-type Symbol
  [l]
  (parse-type-symbol l))

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

(defmethod name-to Union
  [{:keys [types]} name res]
  (->Union (doall (map #(name-to % name res) types))))

(defmethod name-to RClass
  [{:keys [variances the-class replacements]} name res]
  (->RClass variances
            the-class 
            (into {} (for [[k v] replacements]
                       (let [v (remove-scopes (count variances) v)]
                         [k (name-to v name (+ res (count variances)))])))))

(defmethod name-to RInstance
  [{:keys [poly? constructor]} name res]
  (->RInstance (doall (map #(name-to % name res) poly?))
               (name-to constructor name res)))

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
;; Subtypes

(defn type-error [s t]
  (throw (Exception. (str s " is not a subtype of: " t))))

(def ^:dynamic *A*)
(def ^:dynamic *A0*)

(defmulti subtype* (fn [s t] [(class s) (class t)]))

(defn subtypeA* [A s t]
  (cond
    (or (contains? A [s t])
        (= s t))
    A

    :else
    (binding [*A* A
              *A0* (conj A [s t])]
      (subtype* s t))))

(defn subtype [s t]
  (subtypeA* #{} s t))

(defmethod subtype* [RInstance RInstance]
  [{polyl? :poly? constl :constructor :as s}
   {polyr? :poly? constr :constructor :as t}]
  (cond 
    (and (= constl constr)
         (or (not (or polyl? polyr?)) ;no type args, 
             (let [{variances :variances} constl]
               (every? identity
                       (map #(case %1
                               :covariant (subtype* %2 %3)
                               :contravariant (subtype* %3 %2)
                               (= %2 %3))
                            variances
                            polyl?
                            polyr?)))))
    *A0*

    :else (type-error s t)))

(defmethod subtype* [Type Top]
  [s t]
  *A0*)

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

(alter-class ASeq [[a :variance :invariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              ISeq (ISeq a)})

(alter-class Cons [[a :variance :invariant]]
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
;; Checker

(defmulti check :op)

(defn check-top-level [form nsym]
  (let [ast (analyze/ast-in-ns nsym form)]
    (check ast)))
