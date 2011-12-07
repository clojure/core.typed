(ns typed-clojure.core
  (:require [cljs.compiler :as cljs]
            [clojure.walk :as walk]
            [trammel.core :as c]))

;; Primitives

(def Type ::Type)

(defprotocol IType
  (type-fields [this])
  (type-predicate [this])
  (subtype [this super]))

(defn type? [t]
  (isa? (class t) Type))



(defmacro def-type-constructor [name fields contracts & body]
  `(do
     (c/defconstrainedtype ~name
                           ~fields
                           ~contracts

                           java.lang.Comparable
                           (compareTo [this# that#]
                                      (cond
                                        (= this# that#) 0
                                        (subtype this# that#) -1
                                        :else 1))
                           ~@body)
     (derive ~name Type))) ;; TODO Should this be the global hierarchy?

;; Union Type

(declare union?)

(def-type-constructor 
  Union 
  [elems]
  [(every? type? elems)]

  Object
  (toString [_] (str "(Un " (apply str (interpose " " elems)) ")"))
  (equals [_ that] (and (union? that)
                        (= (.elems ^Union that) elems)))
  
  IType
  (subtype [this super]
            (if (union? super)
              (every? true? (map #(subtype %1 super) elems))
              false)))

(defn union? [a]
  (instance? Union a))

(defn Un [& types]
  (->Union types)) ;; TODO sort types uniformly

;; Numeric Types

(def-type-constructor 
  Base 
  [name pred]
  [(symbol? name)]
  
  Object
  (toString [_] (str name))
  (equals [this that] (identical? this that))
  
  IType
  (subtype [this super]
            (cond 
              (= this super) true
              (union? super) (boolean (some true? (map #(subtype this %1) (.elems ^Union super))))
              :else false)))

(def Zero (->Base 'Zero #(= 0 %1)))
(def PositiveInteger (->Base 'PositiveInteger (comp pos? integer?)))
(def NegativeInteger (->Base 'NegativeInteger (comp neg? integer?)))

(def IntegerT (Un NegativeInteger Zero PositiveInteger))
(def FloatT (->Base 'FloatT float?))

;; TODO subtyping for TopT
(def TopT (->Base 'TopT (constantly true)))

(def NumberT (Un IntegerT FloatT))

;; Functions

(declare arity?)

(c/defconstrainedtype 
  Arity 
  [dom rng]
  [(every? type? dom)
   (type? rng)]
  
  Object
  (toString [_] (apply str (concat (apply str (interpose " " dom)) (str " -> " rng))))
  (equals [_ that] (and (arity? that)
                        (= (.dom that) dom)
                        (= (.rng that) rng)))

  ;; TODO copied from def-type, should refactor out
  java.lang.Comparable
  (compareTo [this that]
             (cond
               (= this that) 0
               (subtype this that) -1
               :else 1))

  IType
  (subtype [this supertype]
           (if (arity? supertype)
             ;; One function type is a subtype of another if they have the same number of arguments, 
             ;; the subtype’s arguments are more permissive (is a supertype), and the subtype’s 
             ;; result type is less permissive (is a subtype). 
             ;; For example, (Any -> String) is a subtype of (Number -> (U String #f)).
             (or (= this supertype)
                 (let [sub-dom dom
                       sub-rng rng
                       sup-dom (.dom ^Arity supertype)
                       sup-rng (.rng ^Arity supertype)]
                   (and (= (count sub-dom)
                           (count sup-dom))
                        (= sub-rng
                           sup-rng)
                        (every? true? (map subtype sup-dom sub-dom))
                        (subtype sub-rng sup-rng))))
             false)))

(defn arity? [a]
  (instance? Arity a))

(declare function?)

(def-type-constructor 
  Function 
  [arrs]
  [(every? arity? arrs)]
  
  Object
  (toString [_] (str "(Fn " (apply str (map #(str "(" %1 ")") arrs)) ")"))
  (equals [_ that] (and (function? that)
                        (= (.arrs that) arrs)))
  
  IType
  (subtype [this supertype]
           (if (function? supertype)
             ;; One function type is a subtype of another if they have the same number of arguments, 
             ;; the subtype’s arguments are more permissive (is a supertype), and the subtype’s 
             ;; result type is less permissive (is a subtype). 
             ;; For example, (Any -> String) is a subtype of (Number -> (U String #f)).
             (let [sub-arrs (sort arrs)
                   sup-arrs (sort (.arrs ^Function supertype))]
               (and (= (count sub-arrs)
                       (count sup-arrs))
                    (every? true? (map subtype sub-arrs sup-arrs))))
             false)))

(defn function? [a]
  (instance? Function a))

;; Type ann database

(defn add-type-ann! [id type]
  (assert (var? id))
  (assert (type? type))
  (alter-meta! id assoc ::type type))

(defn lookup-type [id]
  (assert (var? id))
  (if-let [t (-> id meta ::type)]
    t
    (throw (Exception. (str "No type annotation for " id)))))

(def ^:dynamic *local-type-env* {})

(defn lookup-local-type [id]
  (assert (symbol? id))
  (*local-type-env* id))

(defn extend-local-type-env [id type]
  (assert (and (not (namespace id))
               (symbol? id)))
  (assert (type? type))
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
                (cond (zero? f) Zero
                      (integer? f) IntegerT
                      (float? f) FloatT
                      (number? f) NumberT
                      :else TopT))

    :var (if-let [t (lookup-local-type (:form exp-obj))]
           t
           (lookup-type (resolve (:form exp-obj))))

    :def (let [id-var (-> (:form exp-obj) second resolve)
               body (:init exp-obj)
               id-type (lookup-type id-var)
               _ (assert id-type)]
           (type-check body id-type))

    :if (let [then-type (type-check (:then exp-obj))
              else-type (type-check (:else exp-obj))]
          (Un then-type else-type))

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

    :invoke (let [invoked-fn-type (type-check (-> exp-obj :f))
                  actual-args (-> exp-obj :args)
                  same-arity-type (some (fn [^Arity a] 
                                          (and (= (count (.dom a)) (count actual-args))
                                               a))
                                        (.arrs ^Function invoked-fn-type))
                  _ (when-not same-arity-type
                      (type-error "Wrong number of arguments"))
                  _ (map type-check actual-args (.dom ^Arity same-arity-type))
                  rng (.rng ^Arity same-arity-type)]
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
     (when-not (subtype t expected-type)
       (type-error "Not a subtype: " t ", " expected-type))
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
  (let [type (lookup-type (resolve name))
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
    (let [unqualified
          (if (vector? (first body))
            (prepare-method body)        ;; Single arity
            (map prepare-method body))] ;; Multi arity
      ;;TODO qualify symbols
      unqualified)))

(defmacro fn-T [& body]
  `(fn* ~@(normalize-fn-arg-types body))) ;; TODO should fn be a special form?

;; TODO
(defmacro let-T [bindings & body]
  `(let ~@(normalize-fn-arg-types body)
     ))

(fn-T [[a :- IntegerT]]
  a)

(defmacro T [id _ type]
  (assert (symbol? id))
  (let [id-var (if-let [n (namespace id)]
                 (intern (symbol n) (symbol (name id)))
                 (intern *ns* id))]
    (add-type-ann! id-var (emit-type type))
    id-var))

;; Examples

;(let-T [[^{:T IntegerT} a 
;        ^{:T Integer} b] [1 2]
;       ^{:T FloatT} c 2.3]
;  c)

(T test-typed-def-fn :- (Fn (IntegerT -> IntegerT)))
(def-T test-typed-def-fn
  (fn-T 
    ([[a :- IntegerT]] 1)))

(T asdf :- (Fn (IntegerT -> IntegerT)
               (IntegerT IntegerT -> IntegerT)))
(defn-T asdf
  ([a] 1)
  ([a b] 3))

(T a :- IntegerT)
(def-T a 1)

(T test-typed-def-fn :- (Fn (IntegerT -> IntegerT)))
(def-T test-typed-def-fn
  (fn-T 
    ([[a :- IntegerT]] 1)))

(T clojure.core/inc :- (Fn (NumberT -> NumberT)))
(T clojure.core/dec :- (Fn (NumberT -> NumberT)))

(T test-inc-dec :- NumberT)
(def-T test-inc-dec
  (inc 1))
