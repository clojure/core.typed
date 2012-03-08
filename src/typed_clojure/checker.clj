(ns typed-clojure.checker
  (:import (clojure.lang Symbol Var IPersistentMap IPersistentCollection))
  (:use [analyze.core :only [analyze-path]])
  (:require [analyze.util :as util]))

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

(defn type-of [qual-sym]
  (do (assert (find @type-db qual-sym) (str "No type for " qual-sym))
    (@type-db qual-sym)))

;(+T add-type [Symbol Object :> Object])
(defn add-type [sym type]
  (assert (symbol? sym))
  (assert (or (not (find @type-db sym))
              (= (@type-db sym) type))
          (str "Conflicting type annotation for " sym
               " Expected: " (@type-db sym)
               " Found: " type
               " (= (@type-db sym) type): " (= (@type-db sym) type)))
  (swap! type-db #(assoc % sym type))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Types

(deftype Union [types]
  Object
  (equals [this that]
    (and (instance? Union that)
         (= types (.types that))))
  (toString [this]
    (str types)))

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
    (str dom " " rng)))

(defn fun [& arities]
  (assert (<= 1 (count arities)))
  (assert (every? #(instance? Arity %) arities))
  (->Fun arities))

(defn arity [dom rng]
  (assert (instance? clojure.lang.IPersistentCollection dom))
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

(declare matching-arity)

(defprotocol ISubtype
  "A protocol for specifying subtyping rules"
  (subtype [this sub] "A function to determine if sub is a subtype of this"))

(extend-protocol ISubtype
  Union
  (subtype [this sub]
    (cond 
      (instance? Union sub) 
      (every? identity (map #(subtype this %) (.types sub)))

      :else (boolean (some #(subtype % sub) (.types this)))))

  Fun
  (subtype [this sub]
    (when (instance? Fun sub)
      (assert (= 1 (count (.arities sub))))
      (let [[type-arity sub-arity :as arities] (matching-arity this (first (.arities sub)))]
        (when arities
          (subtype type-arity sub-arity)))))

  Arity
  (subtype [this sub]
    (and (instance? Arity sub)
         (cond
           (variable-arity this)
           (let [fixed-dom (take-while #(not (= :& %)) (.dom this))
                 rest-type (last (.dom this))]
             (every? identity (map subtype (concat fixed-dom (repeat rest-type)) (.dom sub))))


           :else
           (and (instance? Arity sub)
                (every? identity (map subtype (.dom this) (.dom sub)))
                (subtype (.rng sub) (.rng this))))))

  Class
  (subtype [this sub]
    (if (instance? Union sub)
      (every? identity (map subtype (repeat this) (.types sub)))
      (isa? sub this)))

  nil
  (subtype [this sub]
    (isa? sub this)))

(defn subtype? [type sub]
  (boolean (subtype type sub)))

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
       (add-type (symbol (str "->" '~name)) (fun (arity ~(vec (map third typed-fields))
                                                       (resolve '~name))))
       
       ; Type for interop dot constructor
       (add-type '~classname (fun (arity ~(vec (map third typed-fields))
                                         (resolve '~name))))

       ~(@#'clojure.core/build-positional-factory gname classname fields)
       (swap! type-db #(assoc % (resolve '~name)
                              '~(apply merge (map (fn [[n _ t]] {n t}) typed-fields))))
       ~classname)))

(defmacro +T [nme type]
  `(do
     ~(when (not (namespace nme))
        `(declare ~nme))
     (add-type (var-or-class->sym (resolve '~nme)) ~type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Annotations for core functions

(+T add-type (fun (arity [Symbol Object] Object)))
(+T var-or-class->sym (fun (arity [(union Class Var)] Symbol)))

(+T fun (fun (arity [:& Arity] Fun)))
(+T arity (fun (arity [IPersistentCollection Object] Arity)))

;clojure.core

(+T clojure.core/resolve (fun (arity [Symbol] (union Var Class))
                              (arity [IPersistentMap Symbol] (union Var Class))))
(+T clojure.core/symbol (fun (arity [(union Symbol String)] Symbol)
                             (arity [clojure.lang.Namespace (union Symbol String)] Symbol)))
(+T clojure.core/str (fun (arity [:& Object] String)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Checker

(defn type-error []
  (throw (Exception. "Type error")))

(defmulti type-check :op)

(defmethod type-check :literal
  [{:keys [val]}]
  (class val))

(defmethod type-check :new
  [{:keys [ctor class args] :as expr}]
  (let [_ (assert class (util/print-expr expr :env :children))
        fn-type (type-of (symbol (.getName class)))]
    (assert fn-type "Constructors only typed if declared with deftypeT")
    (let [matched-arity (matching-arity fn-type args)
          arg-types (map type-check args)]
      (every? true? (map subtype? (.dom matched-arity) arg-types)))))

(defmethod type-check :var
  [{:keys [var env]}]
  (let [sym (var-or-class->sym var)]
    (if-let [local-type (-> env ::local-types sym)]
      local-type
      (type-of sym))))

(defn matching-arity 
  "Returns the matching arity type corresponding to the args"
  [fun args]
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
                 arg-types " is not a subtype of " (.dom matched-arity)))
    (.rng matched-arity)))

(defmethod type-check :import*
  [{:keys [class-str]}]
  nil)

(defmethod type-check :deftype*
  [{:keys []}]
  nil)

(defmethod type-check :fn-expr
  [{:keys [methods]}]
  (doseq [method methods]
    (type-check method))) ;; TODO what does this return?

(defmethod type-check :fn-method
  [{:keys [required-params rest-params body]}]
  (type-check body))

(defmethod type-check :let
  [{:keys [env binding-inits body]}]
  (let [local-types
        (loop [local-types (-> env ::local-types)
               binding-inits binding-inits]
          (if (seq binding-inits)
            (let [{sym :sym, init :init} (:local-binding (first binding-inits))
                  bnd-type (type-check (assoc-in init [:env ::local-types] local-types))]
              (recur (assoc local-types sym bnd-type)
                     (rest binding-inits)))
            local-types))]
    (type-check (assoc-in body [:env ::local-types] local-types))))

(defmethod type-check :do
  [{:keys [exprs]}]
  (do
    (doseq [expr (butlast exprs)]
      (type-check expr))
    (type-check (last exprs))))

(defmethod type-check :def
  [{:keys [env init-provided init var]}]
  (if init-provided
    (let [actual-type (type-check init)
          expected-type (type-of (var-or-class->sym var))]
      (assert (subtype? expected-type actual-type))
      actual-type)
    (println "No init provided for" (var-or-class->sym var))))

(defmethod type-check :default
  [expr]
  (util/print-expr expr :children :env :Expr-obj :ObjMethod-obj)
  (type-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type checker interface

(defn type-check-path [path nsym]
  (let [analysis (analyze-path path nsym)]
    (doseq [a (rest analysis)]
      (type-check a))))

