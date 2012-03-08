(ns typed-clojure.checker
  (:import (clojure.lang Symbol Var IPersistentMap))
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

;(+T add-type [Symbol :-> Object])
(defn add-type [sym type]
  (assert (symbol? sym))
  (assert (or (not (find @type-db sym))
              (= (@type-db sym) type))
          (str "Conflicting type annotation for " sym
               " Expected: " (@type-db sym)
               " Found: " type
               " (= (@type-db sym) type): " (= (@type-db sym) type)))
  (println "add type for" sym)
  (swap! type-db #(assoc % sym type)))

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

(defn variable-arity? [arity]
  (assert (instance? Arity arity))
  (= :&
     (nth (.dom arity) (- (count (.dom arity)) 2) nil)))

(defn find-matching-arity [fn-type arity]
  ; Try fixed arity
  (let [arg-num (count (.dom arity))
        fn-arity (some #(and (= (count (.dom %)) arg-num)
                             %)
                       (.arities fn-type))
        _ (assert fn-arity "Variable-arity not supported")]
    [fn-arity arity]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Subtyping

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
      (let [[type-arity sub-arity :as arities] (find-matching-arity this (first (.arities sub)))]
        (when arities
          (subtype type-arity sub-arity)))))

  Arity
  (subtype [this sub]
    (and (instance? Arity sub)
         (every? identity (map subtype (.dom this) (.dom sub)))
         (subtype (.rng sub) (.rng this))))

  Class
  (subtype [this sub]
    (isa? sub this))

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
       (+T ~(symbol (str "->" name)) (fun (arity ~(vec (map third typed-fields))
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

(+T add-type (fun (arity [Symbol] Object)))
(+T var-or-class->sym (fun (arity [(union Class Var)] Symbol)))
;; TODO type for fun, variable arity
;(+T fun (fun (arity [Object] Fun)))

;clojure.core

(+T clojure.core/resolve (fun (arity [Symbol] (union Var Class))
                              (arity [IPersistentMap Symbol] (union Var Class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Checker

(defn type-error []
  (throw (Exception. "Type error")))

(defmulti type-check :op)

(defmethod type-check :literal
  [{:keys [val]}]
  (class val))

(defmethod type-check :var
  [{:keys [var]}]
  (let [sym (var-or-class->sym var)]
    (type-of sym)))

(defn matching-arity 
  "Returns the matching arity type corresponding to the args"
  [fun args]
  (some #(and (= (count (.dom %))
                 (count args))
              %)
        (.arities fun)))

(defmethod type-check :invoke
  [{:keys [fexpr args] :as expr}]
  (let [fn-type (type-check fexpr)
        arg-types (doall (map type-check args))
        matched-arity (matching-arity fn-type args)]
    (assert matched-arity (do
                           (util/print-expr expr :children :env :Expr-obj :ObjMethod-obj)
                           (println arg-types)
                           (println fn-type)))
    (assert (= (count (.dom matched-arity))
               (count arg-types)))
    (assert (every? true? (doall (map subtype? (.dom matched-arity) arg-types)))
            (println (.dom matched-arity) arg-types (map subtype? (.dom matched-arity) arg-types)))
    (.rng matched-arity)))

(defmethod type-check :import*
  [{:keys [class-str]}]
  (println "Import" class-str)
  nil)

(defmethod type-check :deftype*
  [{:keys []}]
  (println "Defined type") ;TODO get name
  nil)

(defmethod type-check :fn-expr
  [{:keys [methods]}]
  (doseq [method methods]
    (type-check method))) ;; TODO what does this return?

(defmethod type-check :fn-method
  [{:keys [required-params rest-params body]}]
  (type-check body))

(defmethod type-check :let
  [{:keys [binding-inits body]}]
  (type-check body))

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

