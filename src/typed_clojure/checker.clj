(ns typed-clojure.checker
  (:import (clojure.lang Symbol Var IPersistentMap IPersistentCollection Keyword Namespace Atom
                         IPersistentList IPersistentVector))
  (:use [analyze.core :only [analyze-path]])
  (:require [analyze.util :as util]
            [typed-clojure.flag :as flag]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Utils

(def third (comp second rest))

(defn var-or-class->sym [var-or-class]
  (assert (or (var? var-or-class)
              (class? var-or-class)))
  (cond
    (var? var-or-class) (symbol (str (.name (.ns var-or-class))) (str (.sym var-or-class)))
    :else (symbol (.getName var-or-class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Database

; Namespaced symbol -> type
(def type-db (atom {}))

(def ^:dynamic *local-type-db* {})

(defn reset-type-db []
  (swap! type-db (constantly {})))

(defn type-of [qual-sym]
  (if-let [local (*local-type-db* qual-sym)]
    local
    (do (assert (find @type-db qual-sym) (str "No type for " qual-sym))
      (@type-db qual-sym))))

(defmacro with-local-types [type-map & body]
  `(binding [*local-type-db* (merge *local-type-db* ~type-map)]
     ~@body))

;(+T fully-qualified [Symbol -> (U nil Object))
(defn fully-qualified [sym]
  (or (namespace sym)
      (some #(= \. %) (str sym))))

;(+T add-type [Symbol ITypedClojureType -> nil])
(defn add-type [sym type]
  (assert (symbol? sym))
  (assert (fully-qualified sym))
  (assert (or (not (find @type-db sym))
              (= (@type-db sym) type))
          (str "Conflicting type annotation for " sym
               " Expected: " (@type-db sym)
               " Found: " type
               " (= (@type-db sym) type): " (= (@type-db sym) type)))
  #_(println "add type for" sym)
  (swap! type-db #(assoc % sym type))
  nil)

(defprotocol IParseType
  (parse-type* [this]))

(defn- split-dom-syntax
  "Splits domain syntax into [fixed variable]"
  [dom]
  (let [[fixed [_ variable]] (split-with #(not= '& %) dom)]
    [fixed variable]))

(defn- split-arity-syntax 
  "Splits arity syntax into [dom rng]"
  [arity-syntax]
  (assert (some #(= '-> %) arity-syntax) (str "Arity syntax " arity-syntax " missing arrow"))
  (let [[dom [_ rng]] (split-with #(not= '-> %) arity-syntax)]
    [dom rng]))

(declare parse-syntax arity fun union)

(extend-protocol IParseType
  Symbol
  (parse-type* [this] 
    (let [res (resolve this)]
      (cond
        (var? res) (deref res) ; handle protocols
        :else res)))

  IPersistentList ; Union and Fun syntax
  (parse-type* [this]
    (if (= 'U (first this))
      (apply union (map parse-syntax (rest this)))
      (apply fun (map parse-type* this)))) ; don't use implicit single arity syntax

  IPersistentVector ; Arity syntax
  (parse-type* [this]
    (let [[dom rng] (split-arity-syntax this)
          [fixed-dom variable-dom] (split-dom-syntax dom)
          fixed-dom-types (map parse-syntax fixed-dom)
          variable-dom-type (when variable-dom
                              (parse-syntax variable-dom))
          rng-type (parse-syntax rng)]
      (arity (vec (concat fixed-dom-types (when (some #(= '& %) this)
                                            [:& variable-dom-type])))
             rng-type)))
  
  nil
  (parse-type* [this] nil))


(defn parse-syntax 
  "Type syntax parser, entry point"
  [syn]
  (parse-type* (if (vector? syn)
                 (list syn) ; handle implicit single arity syntax
                 syn)))

(defmacro +T [nme type]
  (when @flag/type-check-flag
    `(let [sym# (if (fully-qualified '~nme)
                  '~nme
                  (symbol (name (ns-name *ns*)) (name '~nme)))]
       (add-type sym# (parse-syntax '~type)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Protocols

(defprotocol ITypedClojureType
  "A protocol for specifying subtyping rules"
  (subtype* [this sub] "A function to determine if sub is a subtype of this"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Types

(deftype Union [types]
  Object
  (equals [this that]
    (and (instance? Union that)
         (= types (.types that))))
  (toString [this]
    (with-out-str (pr types))))

(defn union [& types]
  (->Union types))

(deftype Fun [arities]
  Object
  (equals [this that]
    (and (instance? Fun that)
         (= arities (.arities that))))
  (toString [this]
    (str arities)))

(defprotocol IArity
  (fixed-params [this])
  (variable-param [this]))

(deftype Arity [dom rng]
  Object
  (equals [this that]
    (and (instance? Arity that)
         (= dom (.dom that))
         (= rng (.rng that))))
  (toString [this]
    (str (with-out-str (pr dom)) ", " (with-out-str (pr rng))))
  
  IArity
  (fixed-params [this]
    (take-while #(not= :& %) dom))
  (variable-param [this]
    (when (= :&
             (-> dom butlast last))
      (last dom))))

(defn fun [& arities]
  (assert (seq arities) "Must provide at least one arity")
  (assert (every? #(instance? Arity %) arities) 
          (str "All arguments to fun must be arities: " (with-out-str (pr arities))))
  (->Fun arities))

(defn arity [dom rng]
  (assert (every? #(or (= :& %)
                       (satisfies? ITypedClojureType %))
                  dom)
          (str "Every type must satisfy ITypedClojureType: " (with-out-str (prn dom))))
  (assert (satisfies? ITypedClojureType rng) rng)
  (->Arity dom rng))

(def arity? (partial instance? Arity))
(def fun? (partial instance? Fun))

(defn variable-arity 
  "Returns the arity if variable, otherwise false"
  [arity]
  (assert (instance? Arity arity))
  (when (= :&
           (-> (.dom arity) butlast last))
    arity))

(def ^{:doc "Returns the arity if fixed, otherwise false"}
  fixed-arity? (complement variable-arity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Subtyping

(declare matching-arity subtype?)

(extend-protocol ITypedClojureType
  Union
  (subtype* [this sub]
    (cond 
      (instance? Union sub) 
      (every? identity (map #(subtype? this %) (.types sub)))

      :else (some #(subtype? % sub) (.types this))))

  Fun
  (subtype* [this sub]
    (when (instance? Fun sub)
      (every? identity
              (for [sub-arity (.arities sub)]
                (let [type-arity (matching-arity this (.dom sub-arity))]
                  (subtype? type-arity sub-arity))))))

  Arity
  (subtype* [this sub]
    (and (instance? Arity sub)
         (cond
           (variable-arity this)
           (every? true? 
                   (cond
                     (variable-arity sub)
                     (map subtype? 
                          (conj (vec (fixed-params this))
                                (variable-param this))
                          (conj (vec (fixed-params sub))
                                (variable-param sub)))

                     :else
                     (map subtype? 
                          (concat (fixed-params this)
                                  (repeat (variable-param this)))
                          (fixed-params sub))))


           :else
           (and (instance? Arity sub)
                (every? true? (map subtype? (.dom sub) (.dom this)))
                (subtype? (.rng this) (.rng sub))))))

  Class
  (subtype* [this sub]
    (cond 
      (and (identical? this Fun) ; (subtype? Fun (fun ...))
           (instance? Fun sub))
      true

      (and (identical? this Arity) ; (subtype? Arity (arity ...))
           (instance? Arity sub))
      true

      (.isPrimitive this) 
      (do (println "WARNING: Assuming" sub "coerces to primitive type" this)
        true)

      (when (class? sub)
        (.isPrimitive sub))
      (do (println "WARNING: Assuming" this "coerces to primitive type" sub)
        true)

      :else
      (if (instance? Union sub)
        (every? true? (map subtype? (repeat this) (.types sub)))
        (isa? sub this))))

  IPersistentMap ;; Protocols
  (subtype* [this sub]
    (if (instance? Union sub)
      (every? true? (map subtype? (repeat this) (.types sub)))
      (satisfies? this sub)))

  nil
  (subtype* [this sub]
    (nil? sub)))

;(+T subtype? [ITypedClojureType ITypedClojureType -> Boolean)))
(defn subtype? [type sub]
  (boolean (or ;(nil? sub) ;; follow Java Type system, nil/null as Bottom
               (subtype* type sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Front end macros

(defmacro new-type [& body]
  `(def ~@body))

(defmacro deftypeT
  [name [& typed-fields] & opts+specs]
  (let [fields (map first typed-fields)
        gname name
        [interfaces methods opts] (@#'clojure.core/parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        [field-args over] (split-at 20 fields)]
    `(let []
       ~(@#'clojure.core/emit-deftype* name gname (vec hinted-fields) (vec interfaces) methods)
       (import ~classname)

       ; Type for generated ->Type factory
       ~(when @flag/type-check-flag
          `(add-type (symbol (str *ns* "/->" '~name)) (parse-syntax '~(vec (concat (map third typed-fields)
                                                                                   ['-> classname])))))
       
       ~(@#'clojure.core/build-positional-factory gname classname fields)

       ; Type for interop dot constructor
       ~(when @flag/type-check-flag
          `(add-type '~classname (parse-syntax '~(vec (concat (map third typed-fields)
                                                              ['-> classname])))))

       ~classname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Annotations for core functions

(+T add-type [Symbol ITypedClojureType -> nil])
(+T var-or-class->sym [(U Class Var) -> Symbol])

(+T fun [& Arity -> Fun])
(+T arity [IPersistentCollection ITypedClojureType -> Arity])

(+T union [& ITypedClojureType -> Union])

(+T fully-qualified [Symbol -> Object])
(+T parse-syntax [IParseType -> ITypedClojureType])

;clojure.core

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/resolve ([Symbol -> (U nil Var Class)]
                          [IPersistentMap Symbol -> (U nil Var Class)]))
(+T clojure.core/symbol ([(U Symbol String) -> Symbol]
                         [String String -> Symbol]))
(+T clojure.core/str [& Object -> String])
(+T clojure.core/*ns* Namespace)
(+T clojure.core/atom [Object & Object -> Atom])
(+T clojure.core/first [(U nil IPersistentCollection) -> (U nil Object)])
(+T clojure.core/rest [(U nil IPersistentCollection) -> IPersistentCollection])
(+T clojure.core/next [(U nil IPersistentCollection) -> (U nil IPersistentCollection)])
(+T clojure.core/namespace [(U String Symbol Keyword) -> (U nil String)])
(+T clojure.core/name [(U String Symbol Keyword) -> String])
(+T clojure.core/ns-name [Namespace -> Symbol])
(+T clojure.core/refer [Symbol & Object -> nil])
(+T clojure.core/use [& Object -> nil])
(+T clojure.core/seq? [(U nil Object) -> Boolean])
(+T clojure.core/apply [Fun & (U nil Object) -> (U nil Object)])
(+T clojure.core/hash-map [& (U nil Object) -> IPersistentMap])
(+T clojure.core/println [& (U nil Object) -> nil])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Checker

(defn type-error []
  (throw (Exception. "Type error")))

(defmulti type-check :op)

(defmethod type-check :literal
  [{:keys [val]}]
  (class val))

(defmethod type-check :empty-expr
  [{:keys [coll]}]
  (class coll))

(defmethod type-check :map
  [{:keys [keyvals]}]
  (assert (every? identity (map type-check keyvals)))
  clojure.lang.IPersistentMap)

(defmethod type-check :vector
  [{:keys [args]}]
  (assert (every? identity (map type-check args)))
  clojure.lang.IPersistentVector)

(defmethod type-check :if
  [{:keys [test then else]}]
  (do
    (type-check test)
    (union (type-check then)
           (type-check else))))

(defmethod type-check :new
  [{:keys [ctor class args] :as expr}]
  (let [_ (assert class (util/print-expr expr :env :children))
        fn-type (type-of (symbol (.getName class)))]
    (assert fn-type "Constructors only typed if declared with deftypeT")
    (let [matched-arity (matching-arity fn-type args)
          arg-types (map type-check args)]
      (assert (every? true? (map subtype? (.dom matched-arity) arg-types))
              (str arg-types " is not a subtype of " matched-arity " , when trying to invoke constructor " ctor))
      (.rng matched-arity))))

;(+T resolve-class-symbol [Symbol -> Class])
(defn- resolve-class-symbol 
  [sym]
  {:post [(or (nil? %) (class? %))]}
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
    (do (assert (fully-qualified sym) sym)
      (resolve sym))))

;(+T method->fun [clojure.reflect.Method -> Fun])
(defn- method->fun [method]
  (fun (arity (->> (map resolve-class-symbol (:parameter-types method))
                (map #(union nil %)))
              (-> (resolve-class-symbol (:return-type method))
                (union nil)))))

(defmethod type-check :static-field
  [{:keys [field]}]
  (assert (:type field) "No field resolvable, needs type hint")
  (union nil (:type field)))

(defmethod type-check :instance-field
  [{:keys [field]}]
  (assert (:type field) "No field resolvable, needs type hint")
  (union nil (:type field)))

(defn- check-method [method args]
  (let [method-type (method->fun method)
        arg-types (map type-check args)
        matched-arity (matching-arity method-type args)]
    (assert (every? true? (map subtype? (.dom matched-arity) arg-types))
            (str (with-out-str (pr arg-types)) " is not a subtype of " (with-out-str (pr matched-arity)) 
                 " when checking method "  (with-out-str (pr method))))
    (.rng matched-arity)))

(defmethod type-check :static-method
  [{:keys [method args]}]
  (check-method method args))

(defmethod type-check :instance-method
  [{:keys [method args]}]
  (check-method method args))

(defmethod type-check :try
  [{:keys [try-expr catch-exprs finally-expr]}]
  (do
    (type-check finally-expr)
    (apply union (map type-check (cons try-expr catch-exprs)))))

(defmethod type-check :var
  [{:keys [var env]}]
  (let [sym (var-or-class->sym var)]
    (type-of sym)))

(defn matching-arity 
  "Returns the matching arity type corresponding to the args"
  [fun args]
  (assert (instance? Fun fun))
  (assert (not (instance? Arity args)))
  (or (some #(and (= (count (.dom %))
                     (count args))
                  %)
            (filter fixed-arity? (.arities fun))) ; fixed arities
      (some variable-arity (.arities fun)))) ; variable arity

(defn rest-type [arity]
  (assert (variable-arity arity))
  (last (.dom arity)))

(defn fixed-args [arity]
  (assert (arity? arity))
  (take-while #(not (= :& %)) (.dom arity)))


(defmethod type-check :invoke
  [{:keys [fexpr args] :as expr}]
  (let [fn-type (type-check fexpr)
        arg-types (doall (map type-check args))
        matched-arity (matching-arity fn-type args)]
    #_(println "invoke" fexpr)
    (assert matched-arity (do
                           (util/print-expr expr :children :env :Expr-obj :ObjMethod-obj)
                           (println arg-types)
                           (println fn-type)))
    (assert (or (variable-arity matched-arity)
                (= (count (.dom matched-arity))
                   (count arg-types))))

    ; Typecheck args compared to fn signature
    (assert (every? true? (doall (map subtype? 
                                      (concat (fixed-args matched-arity)
                                              (when (variable-arity matched-arity)
                                                (repeat (rest-type matched-arity))))
                                      arg-types)))
            (str "Invoke: type error: " 
                 (with-out-str (pr arg-types)) " is not a subtype of " (.dom matched-arity)))
    (.rng matched-arity)))

(defmethod type-check :import*
  [{:keys [class-str]}]
  nil)

(defmethod type-check :deftype*
  [{:keys []}]
  nil)

(defn- matching-arity-expr [{:keys [required-params rest-params] :as expr} ^Fun fn-type]
  (assert fn-type)
  (if rest-params
    (some variable-arity fn-type)
    (some #(and (= (count (.dom ^Arity %))
                   (count required-params))
                %)
          (filter fixed-arity? (.arities fn-type)))))


(defmethod type-check :fn-expr
  [{:keys [methods] :as expr}]
  (let [expected-type (::expected-type expr)
 ;       _ (assert expected-type (str "No type found for function" (util/print-expr expr :env :children :Expr-obj)))

        actual-type 
        (apply fun (for [method methods]
                     (try
                       (type-check (assoc method ::expected-type (when expected-type
                                                                   (matching-arity-expr method expected-type))))
                       (catch AssertionError e
                         (util/print-expr expr :children :env :Expr-obj)
                         (throw e)))))]
    actual-type))

(defmethod type-check :fn-method
  [{:keys [required-params rest-param body] :as expr}]
  (let [expected-type (::expected-type expr)
        fixed-types (when (seq required-params)
                      (vec (map vector 
                                (map :sym required-params) 
                                (if expected-type
                                  (.dom ^Arity expected-type)
                                  (let [ann-doms (map #(-> % :sym meta :+T resolve) required-params)]
                                    (assert (= (count ann-doms) 
                                               (count required-params)))
                                    (assert (every? #(satisfies? ITypedClojureType %) ann-doms) 
                                            (with-out-str (pr ann-doms)))
                                    ann-doms)))))
        rest-type (when rest-param
                    [[(:sym rest-param) clojure.lang.ISeq]])
        local-types (apply hash-map (flatten (concat fixed-types rest-type)))
        rng-type (with-local-types local-types
                   (type-check body))]
    (arity (if expected-type
             (.dom expected-type)
             (vec (concat (map second fixed-types) 
                          (when rest-type
                            [:& Object])))) ; TODO hardwired, recover this info
           rng-type)))

(defmethod type-check :local-binding-expr
  [{:keys [local-binding]}]
  (type-check local-binding))

(defmethod type-check :local-binding
  [{:keys [sym init]}]
  (type-of sym))

(defmethod type-check :let
  [{:keys [env binding-inits body is-loop]}]
  (assert (not is-loop))
  (let [local-types
        (loop [local-types *local-type-db*
               binding-inits binding-inits]
          (if (seq binding-inits)
            (let [{sym :sym, init :init} (:local-binding (first binding-inits))
                  bnd-type (with-local-types local-types
                             (type-check init))]
              (recur (assoc local-types sym bnd-type)
                     (rest binding-inits)))
            local-types))]
    (with-local-types local-types
      (type-check body))))

(defmethod type-check :do
  [{:keys [exprs]}]
  (do
    (doseq [expr (butlast exprs)]
      (type-check expr))
    (type-check (last exprs))))

(defmethod type-check :def
  [{:keys [env init-provided init var]}]
  (println "type checking" var)
  (if init-provided
    (let [expected-type (type-of (var-or-class->sym var))
          actual-type (type-check (assoc init ::expected-type expected-type))]
      (assert (subtype? expected-type actual-type) (str "Found " (with-out-str (pr actual-type))
                                                        " where expecting " (with-out-str (pr expected-type))))
      actual-type)
    (println "No init provided for" (var-or-class->sym var))))

(defmethod type-check :default
  [expr]
  (util/print-expr expr :children :Expr-obj :ObjMethod-obj)
  (type-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type checker interface

(defn type-check-path [path nsym]
  (swap! flag/type-check-flag (constantly false))
  (require nsym)
  (swap! flag/type-check-flag (constantly true))
  (let [analysis (analyze-path path nsym)]
    (doseq [a analysis]
      (type-check a)))
  (swap! flag/type-check-flag (constantly false)))

