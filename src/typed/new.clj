(ns typed.new
  (:refer-clojure :exclude [defrecord])
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

(def kinds #{:B :F :RClass :Poly :Mu :Union :intersection
             :Top})

(def Type ::Type)

(defn Type? [a]
  (isa? a Type))

(defn declare-type [a]
  (derive a Type))

(defrecord Top []
  "The top type"
  [])

(declare-type Top)

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

(defrecord B [idx]
  "A bound variable. Should not appear outside this file"
  [(nat? idx)])

(declare-type B)

(defrecord F [name]
  "A named free variable"
  [(symbol? name)])

(declare-type F)

(defrecord RClass [the-class replacements]
  "A restricted class, where ancestors are
  (replace replacements (ancestors the-class))"
  [(class? the-class)
   (every? class? (keys replacements))
   (every? Type? (vals replacements))])

(declare-type RClass)

(defrecord Scope [body]
  "A scope that contains bound variables. Not used directly"
  [(Type? body)])

(defrecord Poly [n scope]
  "A polymorphic type containing n bound variables"
  [(nat? n)
   (Scope? scope)])

(declare-type Poly)

;smart constructor
(defn Poly* [names body]
  {:pre [(every? symbol names)
         (Type? body)]}
  (if (empty? names)
    body
    (->Poly (count names) (abstract-many names body))))

(defrecord Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)])

(declare-type Mu)

(declare abstract)

;smart constructor
(defn Mu* [name body]
  (->Mu (abstract name body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(defn add-scopes [n t]
  "Wrap type in n Scopes"
  {:pre [(nat? n)
         (Type? t)]}
  (last 
    (take (inc n) (iterate Scope t))))

(defn remove-scopes [n sc]
  "Unwrap n Scopes"
  {:pre [(nat? n)
         (Type? sc)]}
  (last
    (take (inc n) (iterate (fn [{body :body :as t}]
                             (assert (Scope? t) "Tried to remove too many Scopes")
                             body)
                           sc))))

(defmulti name-to
  "Convert a name in the type to de Bruijn index of res,
  where n is the position in the current binder, and outer is the
  number of indices previously bound"
  (fn [ty name res]
    {:pre [(Type? ty)
           (symbol? name)
           (nat? res)]}
    (:kind ty)))

(defmethod name-to B [ty name res] ty)

(defmethod name-to F
  [{name* :name :as ty} name res] 
  (if (= name name*)
    (->B res)
    ty))

(defmethod name-to RClass
  [{:keys [the-class replacements]} name res]
  (->RClass the-class (into {} (for [[k v] replacements]
                                 [k (name-to v name res)]))))

(defmethod name-to Poly
  [{n :nbound scope :scope} name res]
  (let [body (remove-scopes n scope)]
    (->Poly n (add-scopes n (name-to body name (+ n res))))))

(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (range (dec (count c)) -1 -1) c))

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
  (fn [type image target]
    {:pre [(Type? type)
           (F? image)
           (nat? target)]}
    (:kind type)))

(defmethod replace-image F [ty image target] ty)

(defmethod replace-image B
  [{idx :idx :as ty} image target]
  (if (= idx target)
    image
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

(def ^:dynamic *A*)
(def ^:dynamic *A0*)

(defmulti subtype* (fn [s t] [(:kind s) (:kind t)]))

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

(defmethod subtype* [Type Top]
  [s t]
  )

(defmulti check :op)

(defn check-top-level [form nsym]
  (let [ast (analyze/ast-in-ns nsym form)]
    (check ast)))
