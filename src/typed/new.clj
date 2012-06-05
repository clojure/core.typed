(ns typed.new
  (:require [analyze.core :as analyze]
            [clojure.tools.trace :refer [trace trace-ns]]))

(declare abstract-many)

(def nat? (every-pred (complement neg?) integer?))

(def kinds #{:b :f :rclass :poly :scope})
(defn kind-inst? [k]
  (boolean (kinds (:kind k))))

(defn B [i]
  {:pre [(nat? i)]}
  {:kind :b
   :idx i})

(defn F [name]
  {:pre [(symbol? name)]}
  {:kind :f
   :name name})

(defn F? [a]
  (= (:kind a) :f))

(defn RClass [the-class replacements]
  {:pre [(class? the-class)
         (every? class? (keys replacements))
         (every? kind-inst? (vals replacements))]}

  {:kind :rclass
   :the-class the-class
   :replacements replacements})

(declare Scope?)

(defn make-Poly [n scope]
  {:pre [(nat? n)
         (Scope? scope)]}
  {:kind :poly
   :nbound n
   :scope scope})

;smart constructor
(defn Poly [names body]
  {:pre [(every? symbol names)
         (kind-inst? body)]}
  (if (empty? names)
    body
    (make-Poly (count names) (abstract-many names body))))

(defn Scope [body]
  {:pre [(kind-inst? body)]}

  {:kind :scope
   :body body})

(defn Scope? [a]
  (= (:kind a) :scope))

(defn add-scopes [n t]
  "Wrap type in n Scopes"
  {:pre [(nat? n)
         (kind-inst? t)]}
  (last 
    (take (inc n) (iterate Scope t))))

(defn remove-scopes [n sc]
  "Unwrap n Scopes"
  {:pre [(nat? n)
         (kind-inst? sc)]}
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
    {:pre [(kind-inst? ty)
           (symbol? name)
           (nat? res)]}
    (:kind ty)))

(defmethod name-to :b [ty name res] ty)

(defmethod name-to :f 
  [{name* :name :as ty} name res] 
  (if (= name name*)
    (B res)
    ty))

(defmethod name-to :rclass 
  [{:keys [the-class replacements]} name res]
  (RClass the-class (into {} (for [[k v] replacements]
                               [k (name-to v name res)]))))

(defmethod name-to :poly 
  [{n :nbound scope :scope} name res]
  (let [body (remove-scopes n scope)]
    (make-Poly n (add-scopes n (name-to body name (+ n res))))))

(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (range (dec (count c)) -1 -1) c))

(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         (kind-inst? ty)]}
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
    {:pre [(kind-inst? type)
           (F? image)
           (nat? target)]}
    (:kind type)))

(defmethod replace-image :f [ty image target] ty)

(defmethod replace-image :b
  [{idx :idx :as ty} image target]
  (if (= idx target)
    image
    ty))

(defmethod replace-image :poly
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
         (kind-inst? ty)]}
  (abstract-many [name] ty))

(defn instantiate [f sc]
  "Instantiate bound name to free"
  {:pre [(F? f)
         (Scope? sc)]}
  (instantiate-many [f] sc))

(defmulti subtype (fn [A s t] [(:kind s) (:kind t)]))

(defmulti check :op)

(defn check-top-level [form nsym]
  (let [ast (analyze/ast-in-ns nsym form)]
    (check ast)))
