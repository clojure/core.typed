(ns typed-clojure.infer
  (:import (clojure.lang Var Symbol IPersistentList IPersistentVector Keyword))
  (:use [trammel.core :only [defconstrainedrecord]])
  (:require [analyze.core :as a]
            [analyze.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug macros

(defmacro ast [form]
  `(a/analyze-one {:ns {:name ~'(ns-name *ns*)} :context :eval} '~form))

(defmacro print-ast [form]
  `(-> (a/analyze-one {:ns {:name '~'user} :context :eval} '~form)
    (util/print-expr :children :Expr-obj :LocalBinding-obj :ObjMethod-obj :env)))

(defmacro check-form [form]
  `(check (ast ~form)))

(defmacro synthesize-form [form]
  `(synthesize (ast ~form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(declare map->PrimitiveClass map->TClass)

(defn resolve-or-primitive [sym]
  (case sym
    char Character/TYPE
    boolean Boolean/TYPE
    byte Byte/TYPE
    short Short/TYPE
    int Integer/TYPE
    long Long/TYPE
    float Float/TYPE
    double Double/TYPE
    void nil
    (if-let [res (resolve sym)]
      res
      (throw (Exception. (str sym " does not resolve to a type"))))))

;(+T resolve-class-symbol [Symbol -> Object])
(defn- resolve-class-symbol 
  [sym]
  (let [t (resolve-or-primitive sym)]
    (assert (or (nil? sym) (= 'void sym) (class? t)) (str sym " expected to resolve to a class, instead " t))
    (if (.isPrimitive ^Class t)
      (map->PrimitiveClass
        {:the-class t})
      (map->TClass
        {:the-class t}))))

(declare map->Fun map->FixedArity ->NilType)

;(+T method->fun [clojure.reflect.Method -> ITypedClojureType])
(defn- method->Fun [method]
  (map->Fun
    {:arities [(map->FixedArity 
                 {:dom (->> 
                         (map resolve-class-symbol (:parameter-types method))
                         (map #(union [(->NilType) %]))) ; Java methods can return null
                  :rng (union [->NilType
                               (resolve-class-symbol (:return-type method))])})]}))

(defn var-or-class->sym [var-or-class]
  (assert (or (var? var-or-class)
              (class? var-or-class)))
  (cond
    (var? var-or-class) (symbol (str (.name (.ns var-or-class))) (str (.sym var-or-class)))
    :else (symbol (.getName var-or-class))))

(defmacro map-all-true? [& body]
  `(every? true? (map ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type contexts

(def ^:dynamic *type-db* (atom {}))
(def ^:dynamic *local-type-db* {})

(defn reset-type-db []
  (swap! *type-db* (constantly {})))

(defn type-of [sym-or-var]
  (let [qual-sym (if (var? sym-or-var)
                   (symbol (str (.name (.ns sym-or-var))) (str (.sym sym-or-var)))
                   sym-or-var)
        _ (assert (namespace qual-sym))]
    (if-let [the-local-type (*local-type-db* qual-sym)]
      the-local-type
      (if-let [the-type (@*type-db* qual-sym)]
        the-type
        (throw (Exception. (str "No type for " qual-sym)))))))

(defmacro with-local-types [type-map & body]
  `(binding [*local-type-db* (merge *local-type-db* ~type-map)]
     ~@body))

(defmacro with-type-anns [type-map-syn & body]
  `(binding [*type-db* (atom (apply hash-map (mapcat #(list (or (when-let [var-or-class# (resolve (first %))]
                                                                  (var-or-class->sym var-or-class#))
                                                                (when (namespace (first %))
                                                                  (first %))
                                                                (symbol (str (ns-name *ns*)) (name (first %))))
                                                            (parse-syntax (second %)))
                                                     '~type-map-syn)))]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed Clojure Kinds

(declare arity?)

(defrecord Any [])
(defrecord Nothing [])
(defrecord NilType [])
(defrecord Fun [arities])
(defrecord TClass [the-class])
(defrecord PrimitiveClass [the-class])
(defrecord TProtocol [the-protocol])
(defrecord Union [types])

(defn- simplify-union [the-union]
  (if (some #(instance? Union %) (:types the-union))
    (recur (->Union (set (mapcat #(or (and (instance? Union %)
                                           (:types %))
                                      [%])
                                 (:types the-union)))))
    the-union))

(defn union [types]
  (simplify-union (->Union (set types))))

(def the-tc-types #{Any Nothing Fun TClass PrimitiveClass TProtocol Union NilType})

(defprotocol IArity
  (matches-args [this args] "Return the arity if it matches the number of args,
                            otherwise nil")
  (match-to-fun-arity [this fun-type] "Return an arity than appears to match a fun-type
                                      arity, by counting arguments, not subtyping"))

(defrecord FixedArity [dom rng]
  IArity
  (matches-args [this args]
    (when (= (count dom)
             (count args))
      this))

  (match-to-fun-arity [this fun-type]
    (some #(and (instance? FixedArity %)
                (= (count (:dom %))
                   (count (:dom this)))
                %)
          (:arities fun-type))))

(defrecord VariableArity [fixed-dom rest-type rng]
  IArity
  (matches-args [this args]
    (when (<= (count fixed-dom)
              (count args))
      this)))

(def the-arity-types #{FixedArity VariableArity})

(defn arity? [a]
  (boolean (the-arity-types (class a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

(defprotocol IParseType
  (parse-syntax* [this]))

(defn parse-syntax
  "Type syntax parser, entry point"
  [syn]
  (parse-syntax* (if (vector? syn)
                   (list syn) ; handle implicit single arity syntax
                   syn)))

(extend-protocol IParseType
  Symbol
  (parse-syntax* [this]
    (cond
      (nil? this) (->NilType) ;; nil
      :else (let [res (resolve-or-primitive this)]
              (cond
                (nil? res) (->NilType) ;; void primtive

                (var? res) (map->TProtocol
                             {:the-protocol @res})

                (class? res) (if (.isPrimitive res) 
                               (map->PrimitiveClass
                                 {:the-class res})
                               (map->TClass
                                 {:the-class res})))))))


(defmulti parse-list-syntax first
  :default :default-fun-syntax)

(defmethod parse-list-syntax 'U
  [[_ & syn]]
  (union (doall (map parse-syntax syn))))

(defmethod parse-list-syntax :default-fun-syntax
  [[& arities]]
  (map->Fun 
    {:arities (map parse-syntax* arities)}))

(extend-protocol IParseType
  IPersistentList
  (parse-syntax* [this]
    (parse-list-syntax this)))

(defn- split-arity-syntax 
  "Splits arity syntax into [dom rng]"
  [arity-syntax]
  (assert (some #(= '-> %) arity-syntax) (str "Arity " arity-syntax " missing return type"))
  (let [[dom [_ rng]] (split-with #(not= '-> %) arity-syntax)]
    [dom rng]))

(extend-protocol IParseType
  IPersistentVector
  (parse-syntax* [this]
    (let [_ (assert (not (some #(= '& %) this)) "Variable arity not implemented")
          [dom rng] (split-arity-syntax this)
          dom-types (map parse-syntax dom)
          rng-type (parse-syntax rng)]
      (map->FixedArity
        {:dom dom-types
         :rng rng-type})))
  nil
  (parse-syntax* [_]
    (map->NilType {})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse type syntax

(defprotocol IUnparseType
  (unparse-type* [this]))

(defn unparse-type
  [type-obj]
  (unparse-type* type-obj))

(extend-protocol IUnparseType
  TClass
  (unparse-type* [this]
    (symbol (.getName (:the-class this))))

  PrimitiveClass
  (unparse-type* [this]
    (symbol (.getName (:the-class this))))

  Union
  (unparse-type* [this]
    (apply list 'U (doall (map unparse-type (:types this)))))

  Fun
  (unparse-type* [this]
    (apply list (doall (map unparse-type (:arities this)))))

  FixedArity
  (unparse-type* [this]
    (vec (concat (doall (map unparse-type (:dom this))) ['-> (unparse-type (:rng this))])))

  TProtocol
  (unparse-type* [this]
    (var-or-class->sym (-> this :the-protocol :var)))

  NilType
  (unparse-type* [this]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(defprotocol ISubtype
  (subtype?* [this t]))

(declare subtype?)

(extend-protocol ISubtype
  Any
  (subtype?* [_ t]
    (instance? Any t))

  Nothing
  (subtype?* [_ t]
    true)

  TClass
  (subtype?* [s t]
    (assert (instance? TClass t) t)
    (isa? (:the-class s)
          (:the-class t)))

  PrimitiveClass
  (subtype?* [s t]
    (assert (instance? TClass t))
    (isa? (:the-class s)
          (:the-class t)))

  FixedArity
  (subtype?* [s t]
    (and (instance? FixedArity t)
         (map-all-true? subtype? (:dom t) (:dom s))
         (subtype? (:rng s) (:rng t))))

  Fun
  (subtype?* [s t]
    (println s t)
    (every? true?
            (for [sub-arity (:arities s)]
              (when-let [t-arity (match-to-fun-arity sub-arity t)]
                (subtype? sub-arity t-arity)))))
  
  Union
  (subtype?* [s t]
    (cond 
      (instance? Union t) 
      (map-all-true? #(subtype? % t) (:types s)))

      :else (boolean (some #(subtype? % t) (:types s))))
  
  NilType
  (subtype?* [s t]
    (instance? NilType t)))

(defn subtype? [s t]
  (assert (satisfies? ISubtype t) t)
  (subtype?* s t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Bidirectional checking (Local Type Inference (2000) Pierce & Turner, Chapter 4)

(defmulti check :op)
(defmulti synthesize :op)

(defmethod check :def
  [{:keys [var init init-provided] :as expr}]
  (assert init-provided)
  (let [var-type (type-of var)
        checked-init-expr (-> init
                            (assoc ::+T var-type)
                            check)]
    (assoc expr
           :init checked-init-expr
           ::+T (map->TClass
                  {:the-class Var}))))

(defn- infer-invoke [{:keys [fexpr args] :as expr}]
  (let [synthesized-fexpr (synthesize fexpr)
        fexpr-type (::+T synthesized-fexpr)
        arity-type (some #(matches-args % args) fexpr-type)

        _ (assert arity-type)
        _ (assert (instance? FixedArity arity-type))

        checked-args (doall (map #(-> %1
                                    (assoc ::+T %2)
                                    check) 
                                 args (.dom arity-type)))
        return-type (.rng arity-type)]
    (assoc expr
           :fexpr synthesized-fexpr
           :args checked-args
           ::+T return-type)))

(defmethod synthesize :invoke
  [expr]
  (infer-invoke expr))

(defmethod check :invoke
  [expr]
  (let [expected-type (::+T expr)
        _ (assert expected-type "Checking context for function invocation requires full
                                type annotations")
        inferred-expr (infer-invoke expr)
        actual-type (::+T inferred-expr)
        _ (assert (subtype? actual-type expected-type))]
    inferred-expr))

(defmethod synthesize :literal
  [{:keys [val] :as expr}]
  (assoc expr
         ::+T (map->TClass
                {:the-class (class val)})))
  
(defmethod check :literal
  [{:keys [val] :as expr}]
  (assert (subtype? (map->TClass {:the-class (class val)})
                    (::+T expr))
          (str "Expected " (with-out-str (pr (-> expr ::+T))) ", found " (class val)))
  expr)

(defmethod check :fn-expr
  [{:keys [methods] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert (instance? Fun expected-type) (str "Expected Fun type, instead found " expected-type))

        checked-methods (doall 
                          (for [method methods]
                            (let [_ (assert (not (:rest-param method)))
                                  arity (some #(matches-args % (:required-params method)) (:arities expected-type))
                                  _ (assert arity 
                                            (str "No arity with " (count (:required-params method)) 
                                                 " parameters in type "
                                                 expected-type))]
                              (check (assoc method
                                            ::+T arity)))))

        actual-type (map->Fun
                      {:arities (doall (map ::+T checked-methods))})

        _ (assert (subtype? actual-type expected-type))]
    (assoc expr
           ::+T actual-type
           :methods checked-methods)))

(defmethod check :fn-method
  [{:keys [required-params rest-param body] :as expr}]
  (assert (not rest-param))
  (let [expected-type (::+T expr)
        
        typed-required-params (doall 
                                (map #(assoc %1 ::+T %2)
                                     required-params
                                     expected-type))

        typed-lbndings (apply hash-map 
                              (doall (mapcat #(vector (:sym %) 
                                                      (::+T %))
                                             typed-required-params)))

        checked-body (with-local-types typed-lbndings
                       (check (assoc body
                                     ::+T (:rng expected-type))))]
    (assoc expr
           :required-params typed-required-params
           :body checked-body)))

(defmethod check :do
  [{:keys [exprs] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert expected-type)

        typed-exprs (doall (map check (butlast exprs)))
        _ (assert (seq exprs))
        last-typed-expr (check (assoc (last exprs)
                                      ::+T expected-type))]
    (assoc expr
           :exprs (concat typed-exprs [last-typed-expr]))))

(defmethod check :static-method
  [{:keys [method args] :as expr}]
  (let [fun-type (method->Fun method)
        arity-type (first (:arities fun-type))
        _ (assert (instance? FixedArity arity-type))
        checked-args (doall 
                       (map #(check (assoc %1
                                           ::+T %2))
                            args
                            (:dom arity-type)))]
    (assoc expr
           :args checked-args
           ::+T (:rng arity-type))))

(comment

  (with-type-anns
    {a Keyword}
    (check-form (def a 1)))

  (with-type-anns
    {a [Integer -> Integer]}
    (check-form (defn a [b]
                  (+ 1 1))))

  ;; Literals
  (synthesize-form 1)
  (synthesize-form "a")
  (synthesize-form :a)
  (synthesize-form [1])

)
