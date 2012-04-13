(ns typed-clojure.attempt2
  (:import (clojure.lang Var Symbol IPersistentList IPersistentVector Keyword Cons
                         Ratio))
  (:use [trammel.core :only [defconstrainedrecord defconstrainedvar
                             constrained-atom]]
        [analyze.core :only [ast]])
  (:require [analyze.core :as a]
            [analyze.util :as util]
            [clojure.set :as set]))

(def type-key ::+T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug macros

(def debug-mode (atom true))
(def print-warnings (atom true))

(defmacro warn [& body]
  `(when @print-warnings
     (println ~@body)))

(defmacro debug [& body]
  `(when @debug-mode
     (println ~@body)))

(defmacro check-form [form]
  `(check (a/ast ~form)))

(defmacro synthesize-form [form]
  `(synthesize (a/ast ~form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(declare map->PrimitiveClass map->ClassType)

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
      (map->ClassType
        {:the-class t}))))

(declare map->Fun map->arity union Nil)

;(+T method->fun [clojure.reflect.Method -> ITypedClojureType])
(defn- method->Fun [method]
  (map->Fun
    {:arities [(map->arity 
                 {:dom (->> 
                         (map resolve-class-symbol (:parameter-types method))
                         (map #(union [Nil %]))) ; Java methods can return null
                  :rng (union [Nil
                               (resolve-class-symbol (:return-type method))])})]}))

(defn var-or-class->sym [var-or-class]
  {:pre [(or (var? var-or-class)
             (class? var-or-class))]}
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

(declare Type?)

(defn type-db-var-contract [m]
  (and (every? namespace (keys @m))
       (every? Type? (vals @m))))

(defn type-db-atom-contract [m]
  (and (every? namespace (keys m))
       (every? Type? (vals m))))

(defconstrainedvar 
  ^:dynamic *type-db* 
  (constrained-atom {}
                    "Map from qualified symbols to types"
                    [type-db-atom-contract])
  "Map from qualified symbols to types"
  [type-db-var-contract])

(defn local-type-db-contract [m]
  (and (every? (complement namespace) (keys m))
       (every? Type? (vals m))))

(defconstrainedvar 
  ^:dynamic *local-type-db* {}
  "Map from unqualified names to types"
  [local-type-db-contract])

(defn type-var-scope-contract [m]
  (and (every? (complement namespace) (keys m))
       (every? Type? (vals m))))

;(+T *type-var-scope* (IPersistentMap Symbol UnboundedTypeVariable))
(defconstrainedvar
  ^:dynamic *type-var-scope* {}
  "Map from unqualified names to types"
  [type-var-scope-contract])

(defn reset-type-db []
  (swap! *type-db* (constantly {})))

(defn type-of [sym-or-var]
  {:pre [(or (symbol? sym-or-var)
             (var? sym-or-var))]
   :post [(Type? %)]}
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

(defmacro with-type-vars [var-map & body]
  `(binding [*type-var-scope* (merge *type-var-scope* ~var-map)]
     ~@body))

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
;; Types

(def Type ::type-type)

(defn Type? [t]
  (isa? (class t) Type))

(defmacro def-type [nme & body]
  `(let [a# (defconstrainedrecord ~nme ~@body)]
     (derive a# Type)
     a#))

(def-type Value [val]
  "A singleton type for values, except nil"
  {:pre [(not (nil? val))]})

(declare subtype?)

(def-type Union [types]
  "A disjoint union of types"
  {:pre [(every? Type? types)
         (every? 
           (fn [t]
             (every? #(not (subtype? % t))
                     (disj (set types) t)))
           types)]})

(def-type NilType []
  "The nil value"
  [])

(def Nil (->NilType))

(def-type ClassType [the-class]
  "A class"
  {:pre [(class? the-class)]})

(def Any (Union. #{Nil (->ClassType Object)})) ; avoid constrained constructor because of
                                               ; call to subtype?, which is undefined
(def Any? (partial = Any))

(def Nothing (Union. #{}))
(def Nothing? (partial = Nothing))

(def True (->Value true))
(def True? (partial = True))

(def False (->Value false))
(def False? (partial = False))

(def falsy-values #{False Nil})

;; singleton types

(def-type ConstantVector [types]
  "A constant vector type"
  [(every? Type? types)])

(def-type ConstantList [types]
  "A constant list type"
  [(every? Type? types)])

;; Base types

(declare arity?)

(def-type Fun [arities]
  "Function with one or more arities"
  {:pre [(seq arities)
         (every? arity? arities)]})

(def-type PrimitiveClass [the-class]
  "A primitive class"
  {:pre [(or (nil? the-class) ; void primitive
             (and (class? the-class)
                  (.isPrimitive the-class)))]})

(def-type TProtocol [the-protocol]
  "A protocol"
  {:pre [(and (map? the-protocol)
              (:on the-protocol)
              (:var the-protocol))]})

(defn- simplify-union [the-union]
  (cond 
    (some #(instance? Union %) (:types the-union))
    (recur (->Union (set (doall (mapcat #(or (and (instance? Union %)
                                                  (:types %))
                                             [%])
                                        (:types the-union))))))

    (= 1
       (count (:types the-union)))
    (first (:types the-union))
    
    :else the-union))

(defn union [types]
  (simplify-union (->Union (set types))))

(def-type Intersection [types]
  "An intersection of types"
  {:pre [(every? Type? types)]})

;; type variables

(def-type UnboundedTypeVariable [nme]
  "A record for unbounded type variables, with an unqualified symbol as a name"
  {:pre [(symbol? nme)
         (not (namespace nme))]})

(def type-variables #{UnboundedTypeVariable})

(defn type-variable? [t]
  (boolean (type-variables (class t))))

;; arities

;(defprotocol IArity
;  (matches-args [this args] "Return the arity if it matches the number of args,
;                            otherwise nil")
;  (match-to-fun-arity [this fun-type] "Return an arity than appears to match a fun-type
;                                      arity, by counting arguments, not subtyping"))

(def Arity ::arity-type)

(defn Arity? [a]
  (isa? (class a) Arity))

(declare FilterSet?)

;; arity is NOT a type
(def-type arity [dom rng rest-type flter type-params]
  "An arity with fixed or variable domain. Supports optional filter, and optional type parameters"
  {:pre [(every? Type? dom)
         (Type? rng)
         (or (nil? rest-type)
             (Type? rest-type))
         (or (nil? flter)
             (FilterSet? flter))
         (or (nil? type-params)
             (every? type-variable? type-params))]})

(declare subtypes?)

(defn subtypes?*-varargs [argtys dom rst]
  (loop [dom dom
         argtys argtys]
    (cond
      (and (empty? argtys)
           (empty? dom))
      true

      (empty? argtys)
      false

      (and (empty? dom)
           rst)
      (if (subtype? (first argtys) rst)
        (recur dom (next argtys))
        false)

      (empty? dom)
      false

      (subtype? (first argtys)
                (first dom))
      (recur (next argtys)
             (next dom))

      :else false)))


(def top-arity ::top-arity)

(declare subtype?)

(defn subtype?*-arity [s t]
  (assert (not (:rest-type s)))
  (assert (not (:rest-type t)))
  (and (map-all-true? subtype? 
                      (:dom s)
                      (:dom t))
       (subtype? (:rng s)
                 (:rng t))))

(defn match-to-fun-arity [s fun-type]
  (first 
    (filter #(= (count (:dom s))
                (count (:dom %)))
            (:arities fun-type))))


(defn matches-args [arr args]
  (when (or (and (:rest-type arr)
                 (<= (count (:dom arr))
                     (count args)))
            (= (count (:dom arr))
               (count args)))
    arr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(def ^:private Filter ::filter-type)

(defmacro def-filter [nme & body]
  `(let [a# (defconstrainedrecord ~nme ~@body)]
     (derive a# Filter)
     a#))

(defn Filter? [a]
  (isa? (class a) Filter))

(def-filter TrivialFilter []
  "A proposition that is always true"
  [])

(def-filter ImpossibleFilter []
  "A proposition that is never true"
  [])

(def-filter TypeFilter [var type]
  "A proposition that says var is of type type"
  {:pre [(symbol? var)
         (Type? type)]})

(def-filter NotTypeFilter [var type]
  "A proposition that says var is not of type type"
  {:pre [(symbol? var)
         (Type? type)]})

(def-filter FilterSet [then else]
  "Contains two propositions, then for the truthy result,
  else for the falsy result"
  {:pre [(Filter? then)
         (Filter? else)]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

(defprotocol IParseType
  (parse-syntax* [this]))

(declare Fun-literal All-literal)

(defn parse-syntax
  "Type syntax parser, entry point"
  {:post [Type?]}
  [syn]
  (parse-syntax* (cond 
                   (and (list? syn)
                        (= (first syn)
                           All-literal)) (list Fun-literal syn) ; wrap (All [x] [x -> x]) sugar with (Fun ..)
                   (vector? syn) (list Fun-literal syn) ; wrap arity sugar [] with (Fun ..)
                   :else syn)))

(def parse parse-syntax)

(extend-protocol IParseType
  Symbol
  (parse-syntax* [this]
    (cond
      (*type-var-scope* this) (*type-var-scope* this) ; type variables
      (nil? this) Nil ;; nil
      :else (let [res (resolve-or-primitive this)]
              (cond
                (nil? res) (map->PrimitiveClass
                             {:the-class nil}) ;; void primtive

                (class? res) (if (.isPrimitive res) 
                               (map->PrimitiveClass
                                 {:the-class res})
                               (map->ClassType
                                 {:the-class res}))

                (Any? @res) Any
                (Nothing? @res) Nothing

                (var? res) (map->TProtocol
                             {:the-protocol @res})))))
  
  Boolean
  (parse-syntax* [this]
    (if this
      True
      False))

  String
  (parse-syntax* [this]
    (->Value this))

  Character
  (parse-syntax* [this]
    (->Value this))

  Keyword
  (parse-syntax* [this]
    (->Value this))

  Double
  (parse-syntax* [this]
    (->Value this))

  java.math.BigDecimal
  (parse-syntax* [this]
    (->Value this))

  clojure.lang.Ratio
  (parse-syntax* [this]
    (->Value this))
  
  Long
  (parse-syntax* [this]
    (->Value this))
  
  nil
  (parse-syntax* [this]
    Nil))

(defmulti parse-list-syntax first)

(def All-literal 'All)
(def U-literal 'U)
(def I-literal 'I)
(def Fun-literal 'Fun)
(def predicate-literal 'predicate)

(defmethod parse-list-syntax All-literal
  [[_ [& type-var-names] & [syn & more]]]
  (let [_ (assert (not more) "Only one arity allowed in All scope")
        type-vars (map #(map->UnboundedTypeVariable {:nme %})
                       type-var-names)

        type-var-scope (->> (mapcat vector type-var-names type-vars)
                         (apply hash-map))]

    (with-type-vars type-var-scope
      (assoc (parse-syntax* syn)
             :type-params type-vars))))
        

(defmethod parse-list-syntax U-literal
  [[_ & syn]]
  (union (doall (map parse-syntax syn))))

(defmethod parse-list-syntax I-literal
  [[_ & syn]]
  (map->Intersection 
    {:types (doall (map parse-syntax syn))}))

(defmethod parse-list-syntax predicate-literal
  [[_ & [typ-syntax :as args]]]
  (assert (= 1 (count args)))
  (let [pred-type (parse-syntax typ-syntax)]
    (->Fun [(map->arity
              {:dom [Any]
               :rng (->ClassType Boolean)
               :pred-type pred-type
               :named-params '(a)
               :flter (map->FilterSet
                        {:then (map->TypeFilter
                                 {:var 'a
                                  :type pred-type})
                         :else (map->NotTypeFilter
                                 {:var 'a
                                  :type pred-type})})})])))

(defmethod parse-list-syntax 'quote
  [[_ & [sym :as args]]]
  (assert (= 1 (count args)))
  (assert (symbol? sym))
  (->Value sym))

(defmethod parse-list-syntax Fun-literal
  [[_ & arities]]
  (map->Fun 
    {:arities (doall (map parse-syntax* arities))})) ; parse-syntax* to avoid implicit arity sugar wrapping

(extend-protocol IParseType
  IPersistentList
  (parse-syntax* [this]
    (parse-list-syntax this))

  Cons
  (parse-syntax* [this]
    (parse-list-syntax this)))

(defn- split-arity-syntax 
  "Splits arity syntax into [dom rng opts-map]"
  [arity-syntax]
  (assert (some #(= '-> %) arity-syntax) (str "Arity " arity-syntax " missing return type"))
  (let [[dom [_ rng & opts]] (split-with #(not= '-> %) arity-syntax)]
    [dom rng (apply hash-map opts)]))

(defn- parse-filter [syn]
  (assert (vector? syn))
  (let [[nme-sym keyw type-syn] syn
        type (parse-syntax type-syn)]
    (case keyw
      :-> (map->TypeFilter
            {:var nme-sym
             :type type})
      :!-> (map->NotTypeFilter
             {:var nme-sym
              :type type}))))

(extend-protocol IParseType
  IPersistentVector
  (parse-syntax* [this]
    (let [[dom rng opts-map] (split-arity-syntax this)

          [fixed-dom [_ & rest-args]]
          (split-with #(not= '& %) dom)

          uniform-rest-syntax (when (seq rest-args)
                                (if (= '* (second rest-args))
                                  (first rest-args)
                                  (assert false (str "Invalid rest args syntax " this))))

          extras (into {}
                       (for [[nme syn] opts-map]
                         (cond
                           (= :filter nme) [:flter (map->FilterSet
                                                     {:then (parse-filter (:then syn))
                                                      :else (parse-filter (:else syn))})]

                           :else (throw (Exception. (str "Unsupported option " nme))))))

          fixed-dom-types (doall (map parse-syntax fixed-dom))
          rng-type (parse-syntax rng)
          uniform-rest-type (when rest-args
                              (parse-syntax uniform-rest-syntax))]
      (map->arity
        (merge
          {:dom fixed-dom-types
           :rng rng-type}
          (when uniform-rest-type
            {:rest-type uniform-rest-type})))))

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

(defmulti unparse-filter class)

(defmethod unparse-filter FilterSet
  [{:keys [then else]}]
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(defmethod unparse-filter TypeFilter
  [{:keys [var type]}]
  [var :-> (unparse-type type)])

(defmethod unparse-filter NotTypeFilter
  [{:keys [var type]}]
  [var :!-> (unparse-type type)])

(extend-protocol IUnparseType
  ClassType
  (unparse-type* [this]
    (symbol (.getName ^Class (:the-class this))))

  PrimitiveClass
  (unparse-type* [this]
    (cond
      (nil? (:the-class this)) 'void
      :else (symbol (.getName ^Class (:the-class this)))))

  Union
  (unparse-type* [this]
    (list* U-literal (doall (map unparse-type (:types this)))))

  Intersection
  (unparse-type* [this]
    (list* I-literal (doall (map unparse-type (:types this)))))

  Fun
  (unparse-type* [this]
    (list* Fun-literal (doall (map unparse-type (:arities this)))))

  arity
  (unparse-type* [this]
    (let [dom (doall (map unparse-type (:dom this)))
          ;; handle named parameters
          dom (if-let [names (seq (:named-params this))]
                (doall
                  (map #(vector %1 :- %2)
                       names
                       dom))
                dom)
          rng (unparse-type (:rng this))
          flter (when-let [flter (:flter this)]
                  (unparse-filter flter))

          sig (-> (concat dom 
                          (when (:rest-type this)
                            ['& (unparse-type (:rest-type this)) '*])
                          ['-> rng]
                          (when flter
                            [:filter flter]))
                vec)]
      (if (seq (:type-params this))
        (list All-literal (vec (doall (map unparse-type (:type-params this))))
              sig)
        sig)))

  TProtocol
  (unparse-type* [this]
    (var-or-class->sym (-> this :the-protocol :var)))

  UnboundedTypeVariable
  (unparse-type* [{:keys [nme]}]
    nme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

;; TODO Type variables

(declare supertype-of-all subtype-of-all supertype-of-one)

(defmulti subtype?* (fn [s t]
                      [(class s)
                       (class t)]))

;unions

(defmethod subtype?* [Union Union]
  [s t]
  (or (= s t)
      (supertype-of-all t (:types s))))

(defmethod subtype?* [Type Union]
  [s t]
  (subtype-of-all s (:types t)))

(defmethod subtype?* [Union Type]
  [s t]
  (supertype-of-all t (:types s)))

;singletons

(defmethod subtype?* [Value Value]
  [s t]
  (= (:val s)
     (:val t)))

(defmethod subtype?* [Value ClassType]
  [s t]
  (subtype?* (->ClassType (-> s :val class))
             t))

;classes

(defmethod subtype?* [ClassType ClassType]
  [s t]
  (isa? (:the-class s)
        (:the-class t)))

;nil

(defmethod subtype?* [NilType NilType]
  [s t]
  true)

;function

(defmethod subtype?* [Fun Fun]
  [{s-arities :arities} {t-arities :arities}]
  (every? true?
          (map #(supertype-of-one % s-arities)
               t-arities)))

(defmethod subtype?* [arity arity]
  [{s-dom :dom 
    s-rng :rng
    s-rest :rest-type
    :as s}
   {t-dom :dom
    t-rng :rng
    t-rest :rest-type
    :as t}]
  (cond
    ;; simple case
    (and (not s-rest)
         (not t-rest))
    (and (subtypes? t-dom s-dom)
         (subtype? s-rng t-rng))

    (not s-rest)
    false

    (and s-rest
         (not t-rest))
    (and (subtypes?*-varargs t-dom s-dom s-rest)
         (subtype? s-rng t-rng))

    (and s-rest
         t-rest)
    (and (subtypes?*-varargs t-dom s-dom s-rest)
         (subtype? t-rest s-rest)
         (subtype? s-rng t-rng))

    :else false))

;default

(defmethod subtype?* [Type Type]
  [s t]
  false)

(defn supertype-of-one
  "True if t is a supertype to at least one ss"
  [t ss]
  (some #(subtype? % t) ss))

(defn subtype-of-all 
  "True if s is subtype of all ts"
  [s ts]
  (every? true?
          (map #(subtype? s %) ts)))

(defn supertype-of-all
  "True if t is a supertype of all ss"
  [t ss]
  (every? true?
          (map #(subtype? % t) ss)))

(defn subtypes? [ss ts]
  (and (= (count ss)
          (count ts))
       (every? true? 
               (map subtype? ss ts))))

(defn subtype? [s t]
  (subtype?* s t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Bidirectional checking (Local Type Inference (2000) Pierce & Turner, Section 4)

(declare tc-expr)

(defn tc-expr-check [expr expected-type]
  (let [expr (tc-expr expr)]
    (assert-subtype (type-key expr) expected-type)
    expr))

(defmulti tc-expr 
  (fn [expr & opts] (:op expr)))

;number

(defmethod tc-expr :number
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;constant

(defmulti constant-type class)

(defmethod constant-type Ratio
  [r]
  (->Value r))

(defmethod constant-type Symbol
  [s]
  (->Value s))

(defmethod constant-type Character
  [c]
  (->Value c))

(defmethod tc-expr :constant
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (constant-type val)))

;keyword

(defmethod tc-expr :keyword
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;string

(defmethod tc-expr :string
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;if

(defmethod tc-expr :if
  [expr & opts]
  (let [{ctest :test
         cthen :then
         celse :else
         :as expr}
        (-> expr
          (update-in [:test] tc-expr)
          (update-in [:then] tc-expr)
          (update-in [:else] tc-expr))]
    (assoc expr
           type-key (union [(type-key cthen)
                            (type-key celse)]))))

;do

(defmethod tc-expr :do
  [expr & opts]
  (let [{cexprs :exprs
         :as expr}
        (-> expr
          (update-in [:exprs] #(map tc-expr %)))]
    (assoc expr
           type-key (-> cexprs last type-key))))

;nil

(defmethod tc-expr :nil
  [expr & opts]
  (assoc expr
         type-key Nil))

;def

(defmethod tc-expr :def
  [{:keys [init-provided var] :as expr}]
  (let [expr (-> expr
               (update-in [:init] #(if init-provided
                                     (tc-expr-check % (type-of var))
                                     (tc-expr %))))]
    (assoc expr
           type-key (->ClassType Var))))

;fn

(defmethod tc-expr :fn-expr
  [{:keys [methods] :as expr} & opts]
  (let [expr (-> expr
               (update-in [:methods] #(doall (map tc-expr %))))]
    (assoc expr
           type-key (->Fun (doall (map type-key (:methods expr)))))))

(defmethod tc-expr :fn-method
  [{:keys [required-params rest-param] :as expr} & opts]
  (letfn [(meta-type-annot [expr]
            (-> expr :sym meta (get '+T) parse))]
    (let [dom-syms (doall (map :sym required-params))
          rest-sym (:sym rest-param)

          meta-annots-reqd (doall (map meta-type-annot required-params))
          meta-annot-rst (when rest-param
                           (meta-type-annot rest-param))

          dom-types meta-annots-reqd
          rest-type meta-annot-rst
          
          expr (-> expr
                 (update-in [:body] #(with-local-types
                                       (into {}
                                             (doall
                                               (map vector 
                                                    (concat dom-syms (when rest-param
                                                                       [rest-sym]))
                                                    (concat dom-types (when rest-param
                                                                        [rest-type])))))
                                       (tc-expr %))))]
      (assoc expr
             type-key (map->arity
                        {:dom dom-types
                         :rest-type rest-type
                         :rng (-> expr :body type-key)})))))

;local binding expr

(defmethod tc-expr :local-binding-expr
  [{:keys [local-binding] :as expr} & opts]
  (assoc expr
         type-key (type-of (:sym local-binding))))

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
    (synthesize-form (defn a [b]
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
    {var-occ [Any -> Boolean]
     arg-not-nil [Object -> Boolean]}
    (check-form 
      (defn var-occ [a]
        (when a
          (arg-not-nil a)))))

  (with-type-anns
    {identity (All [a]
                   [a -> a])
     id-long [Long -> Long]}
    (synthesize-form
      (defn id-long [a]
        (identity a))))

  (with-type-anns
    {inter [(I clojure.lang.Seqable
               clojure.lang.IPersistentCollection)
            -> nil]}
    (synthesize-form
      (do
        (inter '{})
        (inter '())
        (inter []))))

  (with-type-anns
    {float? (predicate Float)
     integer? (predicate Integer)
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

  (with-type-anns
    {identity (All [a]
                   [a -> a])}
    (synthesize-form
      (do
        (defn identity [b]
          b)
        (identity 1))))

  (with-type-anns
    {both-same (All [a]
                    [a a -> a])}
    (synthesize-form
      (do
        (declare both-same)
        (both-same 1 "a"))))


  ;; Literals
  (synthesize-form 1)
  (synthesize-form "a")
  (synthesize-form :a)
  (synthesize-form [1])

)
