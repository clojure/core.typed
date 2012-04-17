(ns typed.core
  (:import (clojure.lang Var Symbol IPersistentList IPersistentVector Keyword Cons
                         Ratio Atom IPersistentMap))
  (:use [trammel.core :only [defconstrainedrecord defconstrainedvar
                             constrained-atom]]
        [analyze.core :only [ast]])
  (:require [analyze.core :as a :refer [analyze-path]]
            [analyze.util :as util]
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Annotation

;(+T *add-type-ann-fn* [Symbol Any -> nil])
(def ^:dynamic 
  *add-type-ann-fn* 
  (fn [sym type-syn]
    [sym :- type-syn]))

(defmacro +T [nme type-syn]
  `(*add-type-ann-fn* 
     ~(if (namespace nme)
        `'~nme
        `(symbol (-> *ns* ns-name name) (name '~nme)))
     '~type-syn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed require

;(+T type-db-var-contract [clojure.lang.IPersistentMap -> Boolean])
(defn ns-deps-contract [m]
  (and (every? symbol? (keys m))
       (every? set? (vals m))
       (every? #(every? symbol? %) (vals m))))

(def ns-deps (constrained-atom {}
                               "Map from symbols to seqs of symbols"
                               [ns-deps-contract]))

(defn add-ns-dep [nsym ns-dep]
  (swap! ns-deps update-in [nsym] #(set/union % #{ns-dep}))
  nil)

(defmacro require-typed [nsym]
  `(add-ns-dep (symbol (-> *ns* ns-name name))
               '~nsym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top levels

(declare add-type-ann parse ^:dynamic *type-db* tc-expr unparse)

(defn check-namespace [nsym]
  (let [ 
        ;; 1. Collect all type annotations
        _ (binding [*add-type-ann-fn* (fn [sym type-syn]
                                        (add-type-ann sym (parse type-syn)))]
            (require :reload 'typed.base)
            (require :reload nsym))
        
        ;; 2. Perform type checking
        asts (analyze-path nsym)
        
        _ (doseq [a asts]
            (try (tc-expr a)
              (catch Exception e
                (println a)
                (throw e))))]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type hierarchy

;(+T type-key Keyword)
(def type-key ::+T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug macros

;(+T debug-mode Atom)
(def debug-mode (atom true))

;(+T print-warnings Atom)
(def print-warnings (atom true))

(defmacro warn [& body]
  `(when @print-warnings
     (println ~@body)))

(defmacro debug [& body]
  `(when @debug-mode
     (println ~@body)))

(defmacro tc-form [frm]
  `(-> (ast ~frm) tc-expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(declare map->PrimitiveClass map->ClassType Type)

;(+T resolve-or-primitive [Symbol -> (U Class nil)])
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

;(+T resolve-class-symbol [Symbol -> (U ClassType PrimitiveClass)])
(defn- resolve-class-symbol 
  [sym]
  (let [t (resolve-or-primitive sym)]
    (assert (or (nil? sym) (= 'void sym) (class? t)) (str sym " expected to resolve to a class, instead " t))
    (if (or (nil? t)
            (.isPrimitive ^Class t))
      (map->PrimitiveClass
        {:the-class t})
      (map->ClassType
        {:the-class t}))))

(declare map->Fun map->arity union Nil PrimitiveClass?)

;(+T method->fun [clojure.reflect.Method -> Fun])
(defn- method->Fun [method]
  (map->Fun
    {:arities [(map->arity 
                 {:dom (->> 
                         (map resolve-class-symbol (:parameter-types method))
                         (map #(if (PrimitiveClass? %)
                                 %                  ; nil cannot substutitute for JVM primtiives
                                 (union [Nil %])))) ; Java Objects can be the nil/null pointer
                  :rng (let [typ (resolve-class-symbol (:return-type method))]
                         (if (PrimitiveClass? typ)
                           typ                       ; nil cannot substutitute for JVM primtiives
                           (union [Nil typ])))})]})) ; Java Objects can be the nil/null pointer

;(+T var-or-class->sym [(U Var Class) -> Symbol])
(defn var-or-class->sym [var-or-class]
  {:pre [(or (var? var-or-class)
             (class? var-or-class))]}
  (cond
    (var? var-or-class) (symbol (str (.name (.ns var-or-class))) (str (.sym var-or-class)))
    :else (symbol (.getName var-or-class))))

(defmacro map-all-true? [& body]
  `(every? true? (map ~@body)))

(declare subtype? unparse-type)

;(+T unp [Type -> String])
(defn unp
  "Unparse a type and return string representation"
  [t]
  (with-out-str (-> t unparse-type pr)))

;(+T assert-subtype [Type Type & Any * -> nil])
(defn assert-subtype [actual-type expected-type & msgs]
  (assert (subtype? actual-type expected-type)
          (apply str "Expected " (unp expected-type) ", found " (unp actual-type)
                 msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type contexts

(declare Type?)

;(+T type-db-var-contract [clojure.lang.IPersistentMap -> Boolean])
(defn type-db-var-contract [m]
  (and (every? namespace (keys @m))
       (every? Type? (vals @m))))

;(+T type-db-atom-contract [clojure.lang.IPersistentMap -> Boolean])
(defn type-db-atom-contract [m]
  (and (every? namespace (keys m))
       (every? Type? (vals m))))

;(+T *type-db* (Mapof Symbol Type))
(defconstrainedvar 
  ^:dynamic *type-db* 
  (constrained-atom {}
                    "Map from qualified symbols to types"
                    [type-db-atom-contract])
  "Map from qualified symbols to types"
  [type-db-var-contract])

;(+T local-type-db-contract [clojure.lang.IPersistentMap -> Boolean])
(defn local-type-db-contract [m]
  (and (every? (complement namespace) (keys m))
       (every? Type? (vals m))))

;(+T *local-type-db* (Mapof Symbol Type))
(defconstrainedvar 
  ^:dynamic *local-type-db* {}
  "Map from unqualified names to types"
  [local-type-db-contract])

;(+T type-var-scope-contract [clojure.lang.IPersistentMap -> Boolean])
(defn type-var-scope-contract [m]
  (and (every? (complement namespace) (keys m))
       (every? Type? (vals m))))

;(+T *type-var-scope* (Mapof Symbol UnboundedTypeVariable))
(defconstrainedvar
  ^:dynamic *type-var-scope* {}
  "Map from unqualified names to types"
  [type-var-scope-contract])

;(+T reset-type-db [-> nil])
(defn reset-type-db []
  (swap! *type-db* (constantly {}))
  nil)

;(+T type-of [(U Symbol Var) -> Type])
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

;(+T add-type-ann [Symbol Type -> (Vector* Symbol :- Any)])
(defn add-type-ann [sym typ]
  (when-let [oldtyp (@*type-db* sym)]
    (warn "Overwriting type for" sym ":" typ "from" (unparse oldtyp)))
  (swap! *type-db* assoc sym typ)
  [sym :- (unparse typ)])

(defmacro with-type-anns [type-map-syn & body]
  `(binding [*type-db* (atom (into {} 
                                   (doall (map #(vector (or (when-let [var-or-class# (resolve (first %))]
                                                              (var-or-class->sym var-or-class#))
                                                            (when (namespace (first %))
                                                              (first %))
                                                            (symbol (str (ns-name *ns*)) (name (first %))))
                                                        (parse-syntax (second %)))
                                               '~type-map-syn))))]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

;(+T Type Keyword)
(def Type ::type-type)

;(+T Type? [Any -> Boolean])
(defn Type? [t]
  (isa? (class t) Type))

(defmacro def-type [nme & body]
  `(let [a# (defconstrainedrecord ~nme ~@body)]
     (derive a# Type)
     a#))

(def-type UnitType []
  "The unit type"
  [])
(def Unit (->UnitType))
(def Unit? (partial = Unit))

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
(def Nil? (partial = Nil))

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

; data structures

(def-type Vector [type]
  "A vector of type type, subtype of clojure.lang.IPersistentVector"
  {:pre [(Type? type)]})

(def-type ConstantVector [types]
  "A constant vector type, subtype of clojure.lang.IPersistentVector"
  [(every? Type? types)])

(def-type Sequential [type]
  "A sequential collection type, subtype of clojure.lang.Sequential"
  [(Type? type)])

(def-type ConstantSequential [types]
  "A constant sequential collection type, subtype of clojure.lang.Sequential"
  [(every? Type? types)])

(def-type Map [ktype vtype]
  "A sequential collection type, subtype of clojure.lang.IPersistentMap"
  [(Type? ktype)
   (Type? vtype)])

(def-type ConstantMap [kvtypes]
  "A constant sequential collection type, subtype of clojure.lang.IPersistentMap"
  [(even? (count kvtypes))
   (every? Type? kvtypes)])


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

                (Type? @res) @res

                (map? @res) (map->TProtocol
                              {:the-protocol @res})

                :else (assert false (str "Could not resolve " res))))))
  
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
(def Vectorof-literal 'Vectorof)
(def Vector*-literal 'Vector*)
(def Sequentialof-literal 'Sequentialof)
(def Sequential*-literal 'Sequential*)
(def Mapof-literal 'Mapof)
(def Map*-literal 'Map*)

(defmethod parse-list-syntax All-literal
  [[_ [& type-var-names] & [syn & more]]]
  (let [_ (assert (not more) "Only one arity allowed in All scope")
        type-vars (map #(map->UnboundedTypeVariable {:nme %})
                       type-var-names)

        type-var-scope (into {} (map vector type-var-names type-vars))]

    (with-type-vars type-var-scope
      (assoc (parse-syntax* syn)
             :type-params type-vars))))
        
(defmethod parse-list-syntax Vector*-literal
  [[_ & syns]]
  (->ConstantVector (doall (map parse syns))))
        
(defmethod parse-list-syntax Vectorof-literal
  [[_ syn]]
  (->Vector (parse syn)))
        
(defmethod parse-list-syntax Sequential*-literal
  [[_ & syns]]
  (->ConstantSequential (doall (map parse syns))))
        
(defmethod parse-list-syntax Sequentialof-literal
  [[_ syn]]
  (->Sequential (parse syn)))
        
(defmethod parse-list-syntax Map*-literal
  [[_ & kvsyns]]
  (->ConstantMap (doall (map parse kvsyns))))

(defmethod parse-list-syntax Mapof-literal
  [[_ ksyn vsyn]]
  (->Map (parse ksyn) (parse vsyn)))

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

(defmethod parse-list-syntax U-literal
  [[_ & syn]]
  (union (doall (map parse-syntax syn))))

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
        (list All-literal (doall (mapv unparse-type (:type-params this)))
              sig)
        sig)))

  Value
  (unparse-type* [{:keys [val]}]
    (if (symbol? val)
      `'~val
      val))

  TProtocol
  (unparse-type* [this]
    (var-or-class->sym (-> this :the-protocol :var)))

  Vector
  (unparse-type* [this]
    (list Vectorof-literal (unparse-type (:type this))))

  ConstantVector
  (unparse-type* [this]
    (list* Vector*-literal (map (unparse-type (:types this)))))

  Sequential
  (unparse-type* [this]
    (list Sequentialof-literal (unparse-type (:type this))))

  ConstantSequential
  (unparse-type* [this]
    (list* Sequential*-literal (map (unparse-type (:types this)))))

  Map
  (unparse-type* [this]
    (list Mapof-literal 
          (unparse-type (:ktype this))
          (unparse-type (:vtype this))))

  ConstantMap
  (unparse-type* [this]
    (list* Map*-literal (map (unparse-type (:kvtypes this)))))

  UnitType
  (unparse-type* [this]
    `Unit)

  UnboundedTypeVariable
  (unparse-type* [{:keys [nme]}]
    nme)
  
  NilType
  (unparse-type* [this]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

;; TODO Type variables

(declare supertype-of-all subtype-of-all supertype-of-one)

(defmulti subtype?* (fn [s t]
                      [(class s) (class t)]))

;unit

(defmethod subtype?* [UnitType UnitType]
  [s t]
  true)

;unions

(defmethod subtype?* [Union Union]
  [{s-types :types :as s} 
   {t-types :types :as t}]
  (if (empty? t-types) ; ie. t is Nothing
    (empty? s-types)   ; s <: Nothing when s = Nothing
    (or (and (empty? t-types)
             (not (empty? s)))
        (= s t)
        (supertype-of-all t s-types))))

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

(defmethod subtype?* [Value PrimitiveClass]
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

;void primitive

(defmethod subtype?* [PrimitiveClass NilType]
  [{s-class :the-class :as s} t]
  (nil? s-class))

(defmethod subtype?* [NilType PrimitiveClass]
  [s {t-class :the-class :as t}]
  (nil? t-class))

;primitives

(defmethod subtype?* [PrimitiveClass PrimitiveClass]
  [{s-class :the-class :as s}
   {t-class :the-class :as t}]
  (isa? s-class t-class))

(def ^:private coersions
  {Double/TYPE #{Double}
   Long/TYPE #{Long}})

(defmethod subtype?* [PrimitiveClass ClassType]
  [{s-pclass :the-class :as s}
   {t-class :the-class :as t}]
  (-> (coersions s-pclass)
    (contains? t-class)))

(defmethod subtype?* [ClassType PrimitiveClass]
  [{s-class :the-class :as s}
   {t-pclass :the-class :as t}]
  (-> (coersions t-pclass)
    (contains? s-class)))

;function

(defmethod subtype?* [Fun ClassType]
  [s {t-class :the-class :as t}]
  (isa? t-class clojure.lang.IFn))

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

;vectors

(defmethod subtype?* [Vector Vector]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [Vector ClassType]
  [s t]
  (subtype? (->ClassType IPersistentVector) t))

(defmethod subtype?* [ConstantVector ClassType]
  [s t]
  (subtype? (->ClassType IPersistentVector) t))

(defmethod subtype?* [ConstantVector ConstantVector]
  [{s-types :types :as s} 
   {t-types :types :as t}]
  (subtypes? s-types t-types))

(defmethod subtype?* [ConstantVector Vector]
  [{s-types :types :as s} 
   {t-type :type :as t}]
  (supertype-of-all t-type s-types))

;sequentials

(defmethod subtype?* [Sequential Sequential]
  [{s-type :type :as s}
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [Sequential ClassType]
  [s t]
  (subtype? (->ClassType clojure.lang.Sequential) t))

(defmethod subtype?* [ConstantVector Sequential]
  [{s-types :types :as s} 
   {t-type :type :as t}]
  (supertype-of-all t-type s-types))

(defmethod subtype?* [Vector Sequential]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [ConstantVector ConstantSequential]
  [{s-types :types :as s} 
   {t-types :types :as t}]
  (subtypes? s-types t-types))

;maps

(def AnyMap ::any-map)

(derive AnyMap Type)
(doseq [c #{Map ConstantMap}]
  (derive c AnyMap))

(defmethod subtype?* [AnyMap ClassType]
  [s {t-class :the-class :as t}]
  (isa? t-class IPersistentMap))

(defmethod subtype?* [Map Map]
  [{s-ktype :ktype s-vtype :vtype :as s} 
   {t-ktype :ktype t-vtype :vtype :as t}]
  (subtypes? [s-ktype s-vtype]
             [t-ktype t-vtype]))

(defmethod subtype?* [ConstantMap ConstantMap]
  [{s-kvtypes :kvtypes :as s} 
   {t-kvtypes :kvtypes :as t}]
  (subtypes? s-kvtypes t-kvtypes))

(defmethod subtype?* [ConstantMap Map]
  [{s-kvtypes :kvtypes :as s} 
   {t-ktype :ktype t-vtype :vtype :as t}]
  (subtypes? s-kvtypes (take (count s-kvtypes)
                             (cycle [t-ktype t-vtype]))))

;Object

(prefer-method subtype?*
  [Union Type]
  [Type ClassType])

;; everything except nil is a subtype of java.lang.Object
(defmethod subtype?* [Type ClassType]
  [s t]
  (and (subtype? (->ClassType Object) t)
       (not (Nil? s))))

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

(defn tc-exprs [exprs]
  (doall (map tc-expr exprs)))

;number

(defmethod tc-expr :number
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;constant

(defmulti constant-type class)

(defmethod constant-type IPersistentMap
  [r]
  (->ClassType IPersistentMap))

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
  (let [{:keys [test then else] :as expr}
        (-> expr
          (update-in [:test] tc-expr)
          (update-in [:then] tc-expr)
          (update-in [:else] tc-expr))]
    (assoc expr
           type-key (union [(type-key then)
                            (type-key else)]))))

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
               (update-in [:methods] tc-exprs))]
    (assoc expr
           type-key (->Fun (map type-key (:methods expr))))))

(defmethod tc-expr :fn-method
  [{:keys [required-params rest-param] :as expr} & opts]
  (letfn [(meta-type-annot [expr]
            (-> expr :sym meta (get '+T) parse))]
    (let [dom-syms (map :sym required-params)
          rest-sym (:sym rest-param)

          meta-annots-reqd (doall (map meta-type-annot required-params))
          meta-annot-rst (when rest-param
                           (meta-type-annot rest-param))

          dom-types meta-annots-reqd
          rest-type meta-annot-rst
          actual-rest-arg-type (when rest-param
                                 (->Sequential rest-type))
          
          {cbody :body
           :as expr} 
          (-> expr
            (update-in [:body] #(with-local-types
                                  (into {}
                                        (map vector 
                                             (concat dom-syms (when rest-param
                                                                [rest-sym]))
                                             (concat dom-types (when rest-param
                                                                 [actual-rest-arg-type]))))
                                  (tc-expr %))))
          
          rng-type (type-key cbody)]
      (assoc expr
             type-key (map->arity
                        {:dom dom-types
                         :rest-type rest-type
                         :rng rng-type})))))

;local binding expr

(defmethod tc-expr :local-binding-expr
  [{:keys [local-binding] :as expr} & opts]
  (assoc expr
         type-key (type-of (:sym local-binding))))

;var

(defmethod tc-expr :var
  [{:keys [var] :as expr} & opts]
  (assoc expr
         type-key (type-of var)))

;invoke

(defn- invoke-type [arg-types {:keys [arities] :as fun-type}]
  (let [dummy-arity (map->arity 
                      {:dom arg-types
                       :rng Nothing})
        
        mtched-arity (first (filter #(subtype? % dummy-arity)
                                    arities))
        _ (assert mtched-arity (str "Invoke args " (with-out-str (pr (map unparse arg-types)))
                                    " do not match any arity in "
                                    (unp fun-type)))]
    (:rng mtched-arity)))

(defmethod tc-expr :invoke
  [expr & opts]
  (let [{cfexpr :fexpr
         cargs :args
         :as expr}
        (-> expr
          (update-in [:fexpr] tc-expr)
          (update-in [:args] tc-exprs))]
  (assoc expr
         type-key (invoke-type (map type-key cargs)
                               (type-key cfexpr)))))

;let

(defn- binding-init-map 
  "Returns a map with a single entry, associating
  the name of the binding with its type"
  [{:keys [local-binding init] :as binding-init}]
  (let [init (tc-expr init)
        {:keys [sym]} local-binding]
    {sym (type-key init)}))

(defmethod tc-expr :let
  [{:keys [binding-inits] :as expr} & opts]
  (let [[binding-inits local-env]
        (loop [local-env {}  ;accumulate local environment
               binding-inits binding-inits
               typed-binits []]
          (if (empty? binding-inits)
            [typed-binits local-env]
            (let [tbinit (-> (first binding-inits)
                           (update-in [:init]
                                      #(with-local-types local-env ;with local environment so far
                                                         (tc-expr %))))
                  env-entry {(-> tbinit :local-binding :sym) 
                             (-> tbinit :init type-key)}
                  local-env (merge local-env env-entry)]
              (recur local-env
                     (next binding-inits)
                     (concat typed-binits [tbinit])))))

        expr
        (-> expr
          (update-in [:binding-inits] (constantly binding-inits))
          (update-in [:body] #(with-local-types local-env
                                (tc-expr %))))]
    (assoc expr
           type-key (type-key (:body expr)))))

;static-method

(defmethod tc-expr :static-method
  [{:keys [method] :as expr} & opts]
  (let [method-type (method->Fun method)
        {cargs :args
         :as expr} 
        (-> expr
          (update-in [:args] tc-exprs))]
    (assoc expr
           type-key (invoke-type (map type-key cargs)
                                 method-type))))

;static-field

;(+T field->Type [java.lang.reflect.Field -> Type]
(defn field->Type [field]
  (let [cls (resolve (:type field))]
    (if (.isPrimitive cls)
      (->PrimitiveClass cls)
      (->ClassType cls))))

(defmethod tc-expr :static-field
  [{:keys [field] :as expr} & opts]
  (assoc expr
         type-key (field->Type field)))

;instance-field

(defmethod tc-expr :instance-field
  [{:keys [field] :as expr} & opts]
  (assoc expr
         type-key (field->Type field)))
;map

(defmethod tc-expr :map
  [expr & opts]
  (let [{ckeyvals :keyvals
         :as expr}
        (-> expr
          (update-in [:keyvals] tc-exprs))]
    (assoc expr
           type-key (->ClassType IPersistentMap))))

;emptyexpr

(defmulti empty-types class)

(defmethod empty-types IPersistentMap
  [m]
  (->Map Unit Unit))

(defmethod tc-expr :empty-expr
  [{:keys [coll] :as expr} & opts]
  (assoc expr
         type-key (empty-types coll)))

(comment

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

  (tc-expr (+ 1 1))

  (check-namespace 'typed-clojure.example.typed)

)
