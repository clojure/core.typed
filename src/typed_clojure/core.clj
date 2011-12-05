(ns typed-clojure.core
  (:require [cljs.compiler :as cljs]
            [clojure.walk :as walk]
            [trammel.core :as c]))

;; Primitives

(def Type ::Type)

(defprotocol IType
  (type-fields [this])
  (type-predicate [this]))

(defn type? [t]
  (isa? (class t) Type))

(defmacro def-type-constructor [name & body]
  `(do
     (c/defconstrainedtype ~name
                           ~@body)
     (derive ~name Type))) ;; TODO Should this be the global hierarchy?

;; Union Type

(def-type-constructor 
  Union 
  [elems]
  [(every? type? elems)]

  Object
  (toString [_] (str "(Un " (apply str (interpose " " elems)) ")")))

(defn Un [& types]
  (->Union types))

;; Numeric Types

(def-type-constructor 
  Base 
  [name pred]
  [(symbol? name)]
  
  Object
  (toString [_] (str name)))

(def Zero (->Base 'Zero #(= 0 %1)))
(def One  (->Base 'One #(= 1 %1)))
(def PositiveInteger (->Base 'PositiveInteger (comp pos? integer?)))
(def NegativeInteger (->Base 'NegativeInteger (comp neg? integer?)))

(def IntegerT (Un NegativeInteger Zero PositiveInteger))
(def FloatT (->Base 'FloatT float?))

(def TopT (->Base 'TopT (constantly true)))

(def NumberT (Un IntegerT FloatT))

;; Functions

(c/defconstrainedtype 
  Arity 
  [dom rng]
  [(every? type? dom)
   (type? rng)]
  
  Object
  (toString [_] (str (apply str (interpose " " dom) " -> " rng))))

(defn arity? [a]
  (instance? Arity a))

(def-type-constructor 
  Function 
  [arrs]
  [(every? arity? arrs)]
  
  Object
  (toString [_] (apply str (interpose " , " (map str arrs)))))

;; Type ann database

(def type-anns (atom {}))

(defn add-type-ann! [id type]
  (swap! type-anns assoc id type))

(defn lookup-type [id]
  (if-let [t (@type-anns id)]
    t
    (throw (Exception. (str "No type annotation for " id)))))

(def ^:dynamic *local-type-env* {})

(defn lookup-local-type [id]
  (*local-type-env* id))

(defn extend-local-type-env [id type]
  (assoc *local-type-env* id type))

;; Primitives

(defn type-error [& msg]
  (throw (Exception. (apply str msg))))

(declare type-check)

(defn type-of 
  "Get the type of a binding from metadata"
  [id]
  (if-let [t (-> name meta ::type)]
    t
    (throw (Exception. (str "untyped variable" id)))))

(defn type-check* [exp-obj]
  (case (:op exp-obj)
    :constant (let [f (:form exp-obj)]
                (cond (integer? f) IntegerT
                      (float? f) FloatT
                      (number? f) NumberT
                      :else TopT))

    :var (if-let [t (lookup-local-type (:form exp-obj))]
           t
           (lookup-type (:form exp-obj)))

    :def (let [name (-> (:form exp-obj) second resolve)
               children (:children exp-obj)
               _ (assert (or (nil? children) (== 1 (count children)))) ;; TODO Pretty sure a def can only have 0 or 1 child (?)
               ;; FIXME should probably be :init key ?
               body (first children)]
           (type-check body (type-check name)))

    :if (let [then-type (type-check (:then exp-obj))
              else-type (type-check (:else exp-obj))]
          (when-not (== then-type else-type) ;; TODO subtyping
            (type-error "Expected: " then-type " Found: " else-type))
          else-type)

    :fn (let [methods (:methods exp-obj)]

          )

;    :invoke (let [arg-types (map type-check (-> exp-obj :methods first :params))
;                  local-env (reduce merge

    ))



;; Languages as Libraries, Sam TH
(defn type-check 
  "Type check a form returned from analyze"
  ([exp-obj]
   (type-check* exp-obj))
  ([exp-obj expected-type]
   (let [t (type-check* exp-obj)]
     (when-not (== t expected-type) ;; TODO subtyping
       (type-error "Expected " expected-type " found " t))
     t)))

(defmacro def. [name type & body]
  (let [form `(def ^{:analyze {::type ~type}} ~name  ;;TODO need to quote ~name ?
                ~@body)]
    (type-check (cljs/analyze {} form))
    form))

(defn add-fn-arg-types 
  "Convert fn body from (fn [arg] ..) to (fn [[arg :- type]] ..)"
  [body ^Function type]
  (letfn [(prepare-param [name ptype]
            [name :- ptype])
          (prepare-arg-vector [v atypes]
            (vec (map prepare-param v atypes)))
          (prepare-method [[args & rest] ^Arity mtype]
            (cons (prepare-arg-vector args (.dom mtype)) rest))]
    (if (vector? (first body))
      (prepare-method body type)                ;; Single arity
      (map prepare-method body (.arrs type))))) ;; Multi arity

(defmacro defn. [name & body]
  (let [type (lookup-type (resolve name))
        _ (assert type)]
    `(def. ~name
       (fn. ~@(add-fn-arg-types body type)))))

(defn normalize-fn-arg-types
  "Convert fn body from (fn [[arg :- type]] ...) to (fn [^{:analyze {::type type} name] ..)"
  [body]
  (letfn [(prepare-param [[name _ type]]
            (with-meta name {:analyze {::type type}}))
          (prepare-arg-vector [v]
            (vec (map prepare-param v)))
          (prepare-method [[args & rest]]
            (cons (prepare-arg-vector args) rest))]
    (if (vector? (first body))
      (prepare-method body)        ;; Single arity
      (map prepare-method body)))) ;; Multi arity

(defmacro fn. [& body]
  `(fn ~@(normalize-fn-arg-types body)))

(fn. [[a :- IntegerT]]
  a)

(defmacro T [id type]
  (add-type-ann! (resolve id) (eval type))
  nil)

;; Examples

(T asdf (->Function (list (->Arity (list IntegerT) IntegerT))))
(defn. asdf
  ([a] 1))

(T a IntegerT)
(def. a
  1)

