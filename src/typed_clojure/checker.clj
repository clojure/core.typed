(ns typed-clojure.checker
  (:import (clojure.lang Symbol Var IPersistentMap IPersistentCollection Keyword Namespace Atom))
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

;(+T fully-qualified (fun (arity [Symbol] (union nil Object))))
(defn fully-qualified [sym]
  (or (namespace sym)
      (some #(= \. %) (str sym))))

;(+T add-type (fun (arity [Symbol ITypedClojureType] nil)))
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

(defmacro +T [nme type]
  (when @flag/type-check-flag
    `(let [sym# (if (fully-qualified '~nme)
                  '~nme
                  (symbol (name (ns-name *ns*)) (name '~nme)))]
       (add-type sym# ~type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Protocols

(defprotocol ITypedClojureType
  "A protocol for specifying subtyping rules"
  (subtype [this sub] "A function to determine if sub is a subtype of this"))

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

(deftype Arity [dom rng]
  Object
  (equals [this that]
    (and (instance? Arity that)
         (= dom (.dom that))
         (= rng (.rng that))))
  (toString [this]
    (str (with-out-str (pr dom)) ", " (with-out-str (pr rng)))))

(defn fun [& arities]
  (assert (<= 1 (count arities)))
  (assert (every? #(instance? Arity %) arities))
  (->Fun arities))

(defn arity [dom rng]
  (assert (every? #(or (= :& %)
                       (satisfies? ITypedClojureType %))
                  dom)
          (with-out-str (prn dom)))
  (assert (satisfies? ITypedClojureType (class rng)) rng)
  (->Arity dom rng))

(def arity? (partial instance? Arity))
(def fun? (partial instance? Fun))

(defn variable-arity [arity]
  (assert (instance? Arity arity))
  (and (= :&
          (nth (.dom arity) (- (count (.dom arity)) 2) nil))
       arity))

(def fixed-arity? (complement variable-arity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Subtyping

(declare matching-arity subtype?)

(extend-protocol ITypedClojureType
  Union
  (subtype [this sub]
    (cond 
      (instance? Union sub) 
      (every? identity (map #(subtype? this %) (.types sub)))

      :else (some #(subtype? % sub) (.types this))))

  Fun
  (subtype [this sub]
    (when (instance? Fun sub)
      (every? identity
              (for [sub-arity (.arities sub)]
                (let [type-arity (matching-arity this (.dom sub-arity))]
                  (subtype? type-arity sub-arity))))))

  Arity
  (subtype [this sub]
    (and (instance? Arity sub)
         (cond
           (variable-arity this)
           (let [fixed-dom (take-while #(not (= :& %)) (.dom this))
                 rest-type (last (.dom this))]
             (every? true? (map subtype? (concat fixed-dom (repeat rest-type)) (.dom sub))))


           :else
           (and (instance? Arity sub)
                (every? true? (map subtype? (.dom sub) (.dom this)))
                (subtype? (.rng this) (.rng sub))))))

  Class
  (subtype [this sub]
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
  (subtype [this sub]
    (if (instance? Union sub)
      (every? true? (map subtype? (repeat this) (.types sub)))
      (satisfies? this sub)))

  nil
  (subtype [this sub]
    (nil? sub)))

;(+T subtype? (fun (arity [ITypedClojureType ITypedClojureType] Boolean)))
(defn subtype? [type sub]
  (boolean (or ;(nil? sub) ;; follow Java Type system, nil/null as Bottom
               (subtype type sub))))

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
          `(add-type (symbol (str *ns* "/->" '~name)) (fun (arity ~(vec (map third typed-fields))
                                                                  ~classname))))
       
       ~(@#'clojure.core/build-positional-factory gname classname fields)

       ; Type for interop dot constructor
       ~(when @flag/type-check-flag
          `(add-type '~classname (fun (arity ~(vec (map third typed-fields))
                                             ~classname))))

       ~classname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Annotations for core functions

(+T add-type (fun (arity [Symbol ITypedClojureType] nil)))
(+T var-or-class->sym (fun (arity [(union Class Var)] Symbol)))

(+T fun (fun (arity [:& Arity] Fun)))
(+T arity (fun (arity [IPersistentCollection ITypedClojureType] Arity)))

(+T union (fun (arity [:& ITypedClojureType] Union)))

(+T fully-qualified (fun (arity [Symbol] Object)))
;clojure.core

(+T clojure.core/in-ns (fun (arity [Symbol] Namespace)))
(+T clojure.core/resolve (fun (arity [Symbol] (union nil Var Class))
                              (arity [IPersistentMap Symbol] (union nil Var Class))))
(+T clojure.core/symbol (fun (arity [(union Symbol String)] Symbol)
                             (arity [String String] Symbol)))
(+T clojure.core/str (fun (arity [:& Object] String)))
(+T clojure.core/*ns* Namespace)
(+T clojure.core/atom (fun (arity [Object :& Object] Atom)))
(+T clojure.core/first (fun (arity [(union nil IPersistentCollection)] (union nil Object))))
(+T clojure.core/rest (fun (arity [(union nil IPersistentCollection)] IPersistentCollection)))
(+T clojure.core/next (fun (arity [(union nil IPersistentCollection)] (union nil IPersistentCollection))))
(+T clojure.core/namespace (fun (arity [(union String Symbol Keyword)] (union nil String))))
(+T clojure.core/name (fun (arity [(union String Symbol Keyword)] String)))
(+T clojure.core/ns-name (fun (arity [Namespace] Symbol)))
(+T clojure.core/refer (fun (arity [Symbol :& Object] nil)))
(+T clojure.core/use (fun (arity [:& Object] nil)))
(+T clojure.core/seq? (fun (arity [Object] Boolean)))
(+T clojure.core/apply (fun (arity [Fun :& Object] Object)))
(+T clojure.core/hash-map (fun (arity [:& Object] IPersistentMap)))

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

;(+T resolve-class-symbol (fun (arity [Symbol] Class)))
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

;(+T method->fun (fun (arity [clojure.reflect.Method] Fun)))
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

