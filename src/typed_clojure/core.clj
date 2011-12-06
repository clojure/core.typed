(ns typed-clojure.core
  (:require [cljs.compiler :as cljs]
            [clojure.walk :as walk]
            [trammel.core :as c]))

(defn symbol-ns [s]
  (symbol (name (ns-name *ns*)) (name s)))

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
  (toString [_] (str "(Un " (apply str (interpose " " elems)) ")"))
  (equals [_ that] (= (.elems ^Union that) elems)))

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
  (toString [_] (apply str (concat (apply str (interpose " " dom)) (str " -> " rng))))
  (equals [_ that] (and (= (.dom that) dom)
                        (= (.rng that) rng))))

(defn arity? [a]
  (instance? Arity a))

(def-type-constructor 
  Function 
  [arrs]
  [(every? arity? arrs)]
  
  Object
  (toString [_] (apply str (interpose " , " (map str arrs))))
  (equals [_ that] (= (.arrs that) arrs)))

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
  (if-let [t (-> id meta ::type)]
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

    :def (let [id (-> (:form exp-obj) second symbol-ns)
               body (:init exp-obj)
               id-type (lookup-type id)
               _ (assert id-type)]
           (type-check body id-type))

    :if (let [then-type (type-check (:then exp-obj))
              else-type (type-check (:else exp-obj))]
          (when-not (= then-type else-type) ;; TODO subtyping
            (type-error "Expected: " then-type " Found: " else-type))
          else-type)

    :fn (let [methods (:methods exp-obj)
              arities (doall
                        (map (fn [{params :params variadic :variadic ret :ret}]
                               (assert (not variadic) "Variadic functions not yet supported")
                               (let [arg-types (doall (map (comp ::type meta) params)) ;; TODO bleh, store actual type in metadata
                                     local-type-env (merge (map extend-local-type-env params arg-types))]
                                 (binding [*local-type-env* local-type-env]
                                   ;; TODO type check each form in the body, while accumulating local type env ...
                                   (let [rng (type-check ret)
                                         a (->Arity arg-types rng)]
                                     a))))
                             methods))]
          (->Function arities))

    :invoke (let [invoke-type (type-check (-> exp-obj :f))
                  formals-type (.dom ^Function invoke-type)
                  actuals (-> exp-obj :args)
                  _ (when-not (= (count formals-type) (count actuals))
                      (type-error "Wrong number of arguments"))
                  _ (map type-check actuals formals-type)
                  rng (.rng ^Function formals-type)]
              rng)
    (throw (Exception. (str exp-obj)))
    ))



;; Languages as Libraries, Sam TH
(defn type-check 
  "Type check a form returned from analyze"
  ([exp-obj]
   (type-check* exp-obj))
  ([exp-obj expected-type]
   (let [t (type-check* exp-obj)]
     (when-not (= t expected-type) ;; TODO subtyping
       (type-error "Expected " expected-type " found " t))
     t)))


;; Frontend

(defn emit-type [t]
  (cond
    (type? t) t
    (symbol? t) (-> t resolve deref)
    (= 'Un (first t)) (apply Un (map emit-type (rest t)))   ;; Union
    (= 'Fn (first t)) (->Function (map emit-type (rest t))) ;; Function
    (= '-> (-> t butlast last)) (->Arity (map emit-type (-> t butlast butlast)) (emit-type (last t)))   ;; Arity
    :else (assert (symbol? t) (str "Invalid type syntax " t))))

(defmacro def-T [name & body]
  (let [form `(def ~name ~@body)]
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

(defmacro defn-T [name & body]
  (let [type (lookup-type (symbol-ns name))
        _ (assert type)]
    `(def-T ~name
       (fn-T ~@(add-fn-arg-types body type)))))

(defn normalize-fn-arg-types
  "Convert fn body from (fn [[arg :- type]] ...) to (fn [^{:analyze {::type type} name] ..)"
  [body]
  (letfn [(prepare-param [[name _ type]]
            (with-meta name {:analyze {::type (emit-type type)}}))
          (prepare-arg-vector [v]
            (vec (map prepare-param v)))
          (prepare-method [[args & rest]]
            (cons (prepare-arg-vector args) rest))]
    (if (vector? (first body))
      (prepare-method body)        ;; Single arity
      (map prepare-method body)))) ;; Multi arity

(defmacro fn-T [& body]
  `(fn* ~@(normalize-fn-arg-types body))) ;; TODO should fn be a special form?

;; TODO
(defmacro let-T [bindings & body]
  `(let ~@(normalize-fn-arg-types body)
     ))

(fn-T [[a :- IntegerT]]
  a)

(defmacro T [id _ type]
  (add-type-ann! (symbol-ns id) (emit-type type))
  nil)

;; Examples

;(let-T [[^{:T IntegerT} a 
;        ^{:T Integer} b] [1 2]
;       ^{:T FloatT} c 2.3]
;  c)

(T test-typed-def-fn :- (Fn (IntegerT -> IntegerT)))
(def-T test-typed-def-fn
  (fn-T 
    ([[a :- IntegerT]] 1)))

(T asdf :- (Fn (IntegerT -> IntegerT)))
(defn-T asdf
  ([a] 1))

(T a :- IntegerT)
(def-T a 1)

(T test-typed-def-fn :- (Fn (IntegerT -> IntegerT)))
(def-T test-typed-def-fn
  (fn-T 
    ([[a :- IntegerT]] 1)))

(T clojure.core/+ :- (Fn (IntegerT IntegerT -> IntegerT)
                         (FloatT FloatT -> FloatT)))

(T test-def-plus :- IntegerT)
(def-T test-def-plus
  (+ 1 2))
