(ns typed-clojure.infer
  (:import (clojure.lang Var Symbol IPersistentList IPersistentVector Keyword Cons))
  (:use [trammel.core :only [defconstrainedrecord]])
  (:require [analyze.core :as a]
            [analyze.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug macros

(def debug-mode (atom true))

(defmacro debug [& body]
  `(when @debug-mode
     (println ~@body)))

(defmacro ast [form]
  `(a/analyze-one {:ns {:name (ns-name *ns*)} :context :eval} '~form))

(defn- ppexpr [form]
  (util/print-expr form :children :env :Expr-obj :ObjMethod-obj))

(defmacro print-ast [form]
  `(-> (a/analyze-one {:ns {:name '~'user} :context :eval} '~form)
    (util/print-expr :children :Expr-obj :LocalBinding-obj :ObjMethod-obj :env)))

(defmacro check-form [form]
  `(-> (check (ast ~form))
     (util/print-expr :children :Expr-obj :env)))

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

(declare map->Fun map->FixedArity union Nil)

;(+T method->fun [clojure.reflect.Method -> ITypedClojureType])
(defn- method->Fun [method]
  (map->Fun
    {:arities [(map->FixedArity 
                 {:dom (->> 
                         (map resolve-class-symbol (:parameter-types method))
                         (map #(union [Nil %]))) ; Java methods can return null
                  :rng (union [Nil
                               (resolve-class-symbol (:return-type method))])})]}))

(defn var-or-class->sym [var-or-class]
  (assert (or (var? var-or-class)
              (class? var-or-class)))
  (cond
    (var? var-or-class) (symbol (str (.name (.ns var-or-class))) (str (.sym var-or-class)))
    :else (symbol (.getName var-or-class))))

(defmacro map-all-true? [& body]
  `(every? true? (map ~@body)))

(declare subtype? unparse-type)

(defn unp
  "Unparse a type and return string representation"
  [t]
  (with-out-str (-> t unparse-type pr)))

(defn assert-subtype [actual-type expected-type & msgs]
  (assert (subtype? actual-type expected-type)
          (apply str "Expected " (unp expected-type) ", found " (unp actual-type)
                 msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type contexts

(def ^:dynamic *type-db* (atom {}))
(def ^:dynamic *local-type-db* {})

(defn reset-type-db []
  (swap! *type-db* (constantly {})))

(declare isubtype?)

(defn type-of [sym-or-var]
  {:post [(isubtype? %)]}
  (let [sym (if (var? sym-or-var)
              (symbol (str (.name (.ns sym-or-var))) (str (.sym sym-or-var)))
              sym-or-var)]
    (if-let [the-local-type (and (not (namespace sym))
                                 (*local-type-db* sym))]
      the-local-type
      (if-let [the-type (and (namespace sym)
                             (@*type-db* sym))]
        the-type
        (throw (Exception. (str "No type for " sym)))))))

(defmacro with-local-types [type-map & body]
  `(binding [*local-type-db* (merge *local-type-db* ~type-map)]
     ~@body))

(defmacro with-type-anns [type-map-syn & body]
  `(binding [*type-db* (atom (apply hash-map (doall (mapcat #(list (or (when-let [var-or-class# (resolve (first %))]
                                                                         (var-or-class->sym var-or-class#))
                                                                       (when (namespace (first %))
                                                                         (first %))
                                                                       (symbol (str (ns-name *ns*)) (name (first %))))
                                                                   (parse-syntax (second %)))
                                                            '~type-map-syn))))]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed Clojure Kinds

(declare arity? tc-type?)

(defrecord AnyType [])
(def Any (->AnyType))

(defrecord NothingType [])
(def Nothing (->NothingType))

(defrecord NilType [])
(def Nil (->NilType))

(defconstrainedrecord Fun [arities]
  {:pre [(every? arity? arities)]})

(defconstrainedrecord TClass [the-class]
  {:pre [(class? the-class)]})

(defconstrainedrecord PrimitiveClass [the-class]
  {:pre [(or (nil? the-class) ; void primitive
             (and (class? the-class)
                  (.isPrimitive the-class)))]})

(defconstrainedrecord TProtocol [the-protocol]
  {:pre [(and (map? the-protocol)
              (:on the-protocol)
              (:var the-protocol))]})

(defconstrainedrecord Union [types]
  {:pre [(every? tc-type? types)]})

(defrecord TrueType [])
(def True (->TrueType))

(defrecord FalseType [])
(def False (->FalseType))

(defconstrainedrecord KeywordType [the-keyword]
  [(keyword? the-keyword)])

(defconstrainedrecord SymbolType [the-symbol]
  [(symbol? the-symbol)])

(defconstrainedrecord StringType [the-string]
  [(string? the-string)])

(defconstrainedrecord DoubleType [the-double]
  [(instance? Double the-double)])

(defconstrainedrecord LongType [the-long]
  [(instance? Long the-long)])

(defn- simplify-union [the-union]
  (if (some #(instance? Union %) (:types the-union))
    (recur (->Union (set (doall (mapcat #(or (and (instance? Union %)
                                                  (:types %))
                                             [%])
                                        (:types the-union))))))
    the-union))

(defn union [types]
  (simplify-union (->Union (set types))))

(def the-tc-types #{AnyType NothingType Fun TClass PrimitiveClass TProtocol Union NilType
                    StringType SymbolType KeywordType LongType DoubleType TrueType FalseType})

(def falsy-values #{False Nil})

(defn tc-type? [t]
  (boolean (the-tc-types (class t))))

(defprotocol IArity
  (matches-args [this args] "Return the arity if it matches the number of args,
                            otherwise nil")
  (match-to-fun-arity [this fun-type] "Return an arity than appears to match a fun-type
                                      arity, by counting arguments, not subtyping"))

(defconstrainedrecord FixedArity [dom rng]
  {:pre [(every? isubtype? dom)
         (isubtype? rng)]}
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

(defconstrainedrecord UniformVariableArity [fixed-dom rest-type rng]
  {:pre [(every? isubtype? fixed-dom)
         (isubtype? rest-type)
         (isubtype? rng)]}
  IArity
  (matches-args [this args]
    (when (<= (count fixed-dom)
              (count args))
      this))

  (match-to-fun-arity [this fun-type]
    (some #(and (instance? UniformVariableArity %)
                (= (count (:fixed-dom %))
                   (count (:fixed-dom this)))
                %)
          (:arities fun-type))))

(def the-arity-types #{FixedArity UniformVariableArity})

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

(def parse parse-syntax)

(extend-protocol IParseType
  Symbol
  (parse-syntax* [this]
    (cond
      (nil? this) Nil ;; nil
      :else (let [res (resolve-or-primitive this)]
              (cond
                (nil? res) (map->PrimitiveClass
                             {:the-class nil}) ;; void primtive

                (class? res) (if (.isPrimitive res) 
                               (map->PrimitiveClass
                                 {:the-class res})
                               (map->TClass
                                 {:the-class res}))

                (identical? Any @res) Any
                (identical? Nothing @res) Nothing

                (var? res) (map->TProtocol
                             {:the-protocol @res})))))
  
  Boolean
  (parse-syntax* [this]
    (if (true? this) 
      True 
      False))

  String
  (parse-syntax* [this]
    (map->StringType
      {:the-string this}))

  Keyword
  (parse-syntax* [this]
    (map->KeywordType
      {:the-keyword this}))

  Double
  (parse-syntax* [this]
    (map->DoubleType
      {:the-double this}))
  
  Long
  (parse-syntax* [this]
    (map->LongType
      {:the-long this})))

(defmulti parse-list-syntax first
  :default :default-fun-syntax)

(defmethod parse-list-syntax 'U
  [[_ & syn]]
  (union (doall (map parse-syntax syn))))

(defmethod parse-list-syntax 'quote
  [[_ & [sym :as args]]]
  (assert (= 1 (count args)))
  (assert (symbol? sym))
  (map->SymbolType
    {:the-symbol sym}))

(defmethod parse-list-syntax :default-fun-syntax
  [[& arities]]
  (map->Fun 
    {:arities (doall (map parse-syntax* arities))}))

(extend-protocol IParseType
  IPersistentList
  (parse-syntax* [this]
    (parse-list-syntax this))

  Cons
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
    (let [[dom rng] (split-arity-syntax this)

          [fixed-dom [_ uniform-rest-type :as rest-args]]
          (split-with #(not= '& %) dom)

          _ (assert (or (not (seq rest-args))
                        (= 2 (count rest-args)))
                    "Incorrect uniform variable arity syntax")

          fixed-dom-types (doall (map parse-syntax fixed-dom))
          rng-type (parse-syntax rng)]
      (if (seq rest-args)
        (map->UniformVariableArity
          {:fixed-dom fixed-dom-types
           :rest-type (parse-syntax uniform-rest-type)
           :rng rng-type})
        (map->FixedArity
          {:dom fixed-dom-types
           :rng rng-type}))))

  nil
  (parse-syntax* [_]
    Nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse type syntax

(defprotocol IUnparseType
  (unparse-type* [this]))

(defn unparse-type
  [type-obj]
  (unparse-type* type-obj))

(def unparse unparse-type)

(extend-protocol IUnparseType
  TClass
  (unparse-type* [this]
    (symbol (.getName ^Class (:the-class this))))

  PrimitiveClass
  (unparse-type* [this]
    (cond
      (nil? (:the-class this)) 'void
      :else (symbol (.getName ^Class (:the-class this)))))

  Union
  (unparse-type* [this]
    (list* 'U (doall (map unparse-type (:types this)))))

  Fun
  (unparse-type* [this]
    (list* (doall (map unparse-type (:arities this)))))

  FixedArity
  (unparse-type* [this]
    (vec (concat (doall (map unparse-type (:dom this))) ['-> (unparse-type (:rng this))])))

  UniformVariableArity
  (unparse-type* [this]
    (vec (concat (doall (map unparse-type (:fixed-dom this))) ['& (unparse-type (:rest-type this))
                                                               '-> (unparse-type (:rng this))])))

  TProtocol
  (unparse-type* [this]
    (var-or-class->sym (-> this :the-protocol :var)))

  AnyType
  (unparse-type* [this]
    (assert (identical? Any this))
    (var-or-class->sym #'Any))

  NothingType
  (unparse-type* [this]
    (assert (identical? Nothing this))
    (var-or-class->sym #'Nothing))

  KeywordType
  (unparse-type* [{:keys [the-keyword]}]
    the-keyword)

  StringType
  (unparse-type* [{:keys [the-string]}]
    the-string)

  DoubleType
  (unparse-type* [{:keys [the-double]}]
    the-double)

  LongType
  (unparse-type* [{:keys [the-long]}]
    the-long)

  TrueType
  (unparse-type* [this]
    (assert (identical? True this))
    true)

  FalseType
  (unparse-type* [this]
    (assert (identical? False this))
    false)

  SymbolType
  (unparse-type* [{:keys [the-symbol]}]
    `'~the-symbol)

  NilType
  (unparse-type* [this]
    (assert (identical? Nil this))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(defprotocol ISubtype
  (subtype?* [this t]))

(defn isubtype? [a]
  (satisfies? ISubtype a))

(declare subtype?)

(extend-protocol ISubtype
  AnyType
  (subtype?* [_ t]
    (identical? Any t))

  NothingType
  (subtype?* [s t]
    (assert (identical? s Nothing))
    true)

  TClass
  (subtype?* [s t]
    (boolean
      (when (instance? TClass t)
        (isa? (:the-class s)
              (:the-class t)))))

  PrimitiveClass
  (subtype?* [s t]
    (boolean
      (when (instance? PrimitiveClass t)
        (isa? (:the-class s)
              (:the-class t)))))

  FixedArity
  (subtype?* [s t]
    (and (instance? FixedArity t)
         (map-all-true? subtype? (:dom t) (:dom s))
         (subtype? (:rng s) (:rng t))))

  Fun
  (subtype?* [s t]
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
    (assert (identical? Nil s))
    (identical? Nil t)))

(defn subtype? [s t]
  (assert (satisfies? ISubtype t) t)
  (cond
    (identical? Any t) true

    (instance? Union t)
    (map-all-true? #(subtype? s %) (:types t))
    
    :else (subtype?* s t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Bidirectional checking (Local Type Inference (2000) Pierce & Turner, Chapter 4)

(defmulti check :op)
(defmulti synthesize :op)

;; var

(defmethod check :var
  [{:keys [var tag] :as expr}]
  (let [expected-type (::+T expr)
        actual-type (type-of var)]
    (assert-subtype actual-type expected-type (str " for var " var))
    (assoc expr
           ::+T actual-type)))

(defmethod synthesize :var
  [{:keys [var tag] :as expr}]
  (let [actual-type (type-of var)]
    (assoc expr
           ::+T actual-type)))

;; def

(defn infer-def [{:keys [var init init-provided] :as expr}]
  (let [var-type (type-of var)
        checked-init-expr (if init-provided
                            (-> init
                              (assoc ::+T var-type)
                              check)
                            (do
                              (debug "No init provided for" var ", ignore body")
                              init))]
    (assoc expr
           :init checked-init-expr
           ::+T (map->TClass
                  {:the-class Var}))))

(defmethod check :def
  [expr]
  (let [expected-type (::+T expr)
        inferred-def (infer-def expr)
        actual-type (::+T inferred-def)
        _ (assert-subtype actual-type expected-type)]
    inferred-def))

(defmethod synthesize :def
  [expr]
  (infer-def expr))

(defn- infer-invoke [{:keys [fexpr args] :as expr}]
  (let [synthesized-fexpr (synthesize fexpr)
        fexpr-type (::+T synthesized-fexpr)
        arity-type (some #(matches-args % args) (:arities fexpr-type))

        _ (assert arity-type)

        checked-args (doall (map #(-> %1
                                    (assoc ::+T %2)
                                    check)
                                 args
                                 (cond
                                   (instance? FixedArity arity-type)
                                   (:dom arity-type)

                                   (instance? UniformVariableArity arity-type)
                                   (concat (:fixed-dom arity-type)
                                           (repeat (:rest-type arity-type)))

                                   :else (assert false (str "Unsupported Arity" arity-type)))))

        return-type (:rng arity-type)]
    (assoc expr
           :fexpr synthesized-fexpr
           :args checked-args
           ::+T return-type)))

;; invoke

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
        _ (assert-subtype actual-type expected-type)]
    inferred-expr))

;; if

(defmethod check :if
  [{:keys [test then else] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert expected-type "if in checking mode requires full annotation")

        synthesized-test (synthesize test)

        check-else? (boolean (falsy-values (::+T synthesized-test)))

        checked-then (check (assoc then 
                                   ::+T expected-type))

        inferred-else (if check-else?
                        (check (assoc else
                                      ::+T expected-type))
                        (synthesize else))
        
        actual-type (union (map ::+T (concat [checked-then] (when check-else?
                                                              inferred-else))))
        _ (assert-subtype actual-type expected-type)]
    (assoc expr 
           :test synthesized-test
           :then checked-then
           :else inferred-else
           ::+T actual-type)))

;; local bindings

(defmethod check :local-binding-expr
  [{:keys [local-binding] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert expected-type (str "Local binding " (:sym local-binding)
                                     " requires type annotation in checking context."))
        checked-lb (check (assoc local-binding
                                 ::+T expected-type))
        actual-type (::+T checked-lb)
        _ (assert-subtype actual-type expected-type)]
    (assoc expr
           :local-binding checked-lb
           ::+T actual-type)))

(defmethod check :local-binding
  [{:keys [sym init] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert expected-type sym)
        actual-type (type-of sym)
        _ (assert-subtype actual-type expected-type)]
    (assoc expr
           ::+T actual-type)))

;; literals

(defmacro literal-dispatches [disp-keyword]
  `(do
     (defmethod synthesize ~disp-keyword
       [expr#]
       (let [val# (:val expr#)
             actual-type# (parse-syntax val#)]
         (assoc expr#
                ::+T actual-type#)))

     (defmethod check ~disp-keyword
       [expr#]
       (let [val# (:val expr#)
             expected-type# (::+T expr#)
             actual-type# (parse-syntax val#)]
         (assert-subtype actual-type# expected-type#)
         (assoc expr#
                ::+T actual-type#)))))

(literal-dispatches :keyword)
(literal-dispatches :string)
(literal-dispatches :symbol)
(literal-dispatches :number)
(literal-dispatches :constant)
(literal-dispatches :nil)

;; let

(defmethod synthesize :binding-init
  [{:keys [sym init] :as expr}]
  (let [synthesized-init (synthesize init)]
    (assoc expr
           :init synthesized-init
           ::+T (::+T synthesized-init))))

(defmethod check :let
  [{:keys [binding-inits body is-loop] :as expr}]
  (assert (not is-loop) "Loop not implemented")
  (let [expected-type (::+T expr)
        _ (assert expected-type)
        
        [typed-binding-inits local-types]
        (loop [binding-inits binding-inits
               typed-binding-inits []
               local-types {}]
          (if (empty? binding-inits)
            [typed-binding-inits local-types]
            (let [[bnd-init] binding-inits
                  typed-bnd-init (with-local-types local-types
                                   (synthesize bnd-init))
                  local-type-entry [(-> typed-bnd-init :local-binding :sym)
                                    (-> typed-bnd-init ::+T)]
                  _ (assert (every? identity local-type-entry))]
              (recur (rest binding-inits)
                     (conj typed-binding-inits typed-bnd-init)
                     (conj local-types local-type-entry)))))
        
        checked-body (with-local-types local-types
                       (check (assoc body
                                     ::+T expected-type)))]
    (assoc expr
           :binding-inits typed-binding-inits
           :body checked-body
           ::+T (-> body ::+T))))

;; fn

(defmethod check :fn-expr
  [{:keys [methods] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert (instance? Fun expected-type) (str "Expected Fun type, instead found " (unparse-type expected-type)))

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

        _ (assert-subtype actual-type expected-type)]
    (assoc expr
           ::+T actual-type
           :methods checked-methods)))

(defmethod check :fn-method
  [{:keys [required-params rest-param body] :as expr}]
  (assert (not rest-param))
  (let [expected-arity-type (::+T expr)
        _ (assert (instance? FixedArity expected-arity-type))
        
        typed-required-params (doall 
                                (map #(assoc %1 ::+T %2)
                                     required-params
                                     (:dom expected-arity-type)))

        typed-lbndings (apply hash-map 
                              (doall (mapcat #(vector (:sym %) 
                                                      (::+T %))
                                             typed-required-params)))

        checked-body (with-local-types typed-lbndings
                       (check (assoc body
                                     ::+T (:rng expected-arity-type))))]
    (assoc expr
           :required-params typed-required-params
           :body checked-body)))

;; do

(defmethod synthesize :do
  [{:keys [exprs] :as expr}]
  (let [synthesized-exprs (vec (doall (map synthesize exprs)))
        actual-type (-> synthesized-exprs last ::+T)
        _ (assert actual-type)]
    (assoc expr
           :exprs synthesized-exprs
           ::+T actual-type)))

(defmethod check :do
  [{:keys [exprs] :as expr}]
  (let [expected-type (::+T expr)
        _ (assert expected-type "do requires type annotation in checking mode")

        butlast-synthesized-exprs (vec (doall (map synthesize (butlast exprs))))
        _ (assert (seq exprs))
        last-checked-expr (check (assoc (last exprs)
                                      ::+T expected-type))
        typed-exprs (conj butlast-synthesized-exprs last-checked-expr)

        actual-type (::+T last-checked-expr)
        _ (assert actual-type)]
    (assoc expr
           :exprs typed-exprs
           ::+T actual-type)))

;; static method

(defn- overriden-annotation [{name-sym :name, class-sym :declaring-class,
                              :keys [declaring-class parameter-types] :as method}]
  (let [_ (assert (and class-sym name-sym)
                  (str "Unresolvable static method " class-sym name-sym))
        method-sym (symbol (name class-sym) (name name-sym))]
    (try
      (type-of method-sym)
      (catch Exception e))))

(defn infer-static-method [{:keys [method args] :as expr}]
  (let [override (overriden-annotation method)
        _ (if override
            (println "Overriding static method " (symbol (name (:declaring-class method))
                                                         (name (:name method))))
            (println "Not overriding static method " (symbol (name (:declaring-class method))
                                                             (name (:name method)))))
        fun-type (if override
                   override
                   (method->Fun method))
        arity-type (some #(matches-args % args) (:arities fun-type))
        _ (assert (instance? FixedArity arity-type) (str "No matching arity found for "
                                                         (unp fun-type) " with args "
                                                         args))
        checked-args (doall 
                       (map #(-> %1
                               (assoc ::+T %2)
                               check)
                            args
                            (:dom arity-type)))

        actual-type (:rng arity-type)]
    (assoc expr
           :args checked-args
           ::+T actual-type)))

(defmethod synthesize :static-method
  [expr]
  (infer-static-method expr))

(defmethod check :static-method
  [expr]
  (let [expected-type (::+T expr)
        _ (assert expected-type "Static method in checking mode requires annotation")

        inferred-expr (infer-static-method expr)

        actual-type (::+T inferred-expr)
        _ (assert-subtype actual-type expected-type)]
    expr))

(comment

  (with-type-anns
    {a Keyword}
    (check-form (def a 1)))

  (with-type-anns
    {str [Object -> String]
     a [Integer -> String]}
    (check-form (defn a [b]
                  (str 1))))

  (with-type-anns
    {str [& Object -> String]
     a [Integer -> String]}
    (check-form (defn a [b]
                  (str "a" 1))))
  (with-type-anns
    {str [& Object -> String]
     clojure.lang.Util/equiv [Number Number -> Boolean]
     a [Integer -> String]}
    (check-form (defn a [b]
                  (if (= b 1)
                    "a"))))

  (with-type-anns
    {ret-fn [-> [-> nil]]}
    (check-form 
      (defn ret-fn []
        (fn []))))

  (with-type-anns
    {test-let [Integer -> Boolean]}
    (check-form 
      (defn test-let [a]
        (let [b true]
          b))))

  (with-type-anns
    {test-let [Integer -> Boolean]}
    (check-form 
      (defn test-let [a]
        (loop [b true]
          b))))

  (with-type-anns
    {float? [Any -> Boolean]
     integer? [Any -> Boolean]
     takes-float [Float -> Boolean]
     takes-integer [Integer -> Boolean]
     occur [(U Float Integer) -> Boolean]}
    (synthesize-form 
      (do
        (declare takes-integer takes-float)
        (defn occur [a]
          (cond
            (float? a) (takes-float a)
            (integer? a) (takes-integer a)
            :else false)))))

  ;; TODO split :literal op into finer grained :op's in analyze

  ;; Literals
  (synthesize-form 1)
  (synthesize-form "a")
  (synthesize-form :a)
  (synthesize-form [1])

)
