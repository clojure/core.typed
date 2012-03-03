(ns typed-clojure.checker
  (:import (clojure.lang Symbol))
  (:use [analyze.core :only [analyze-path]])
  (:require [analyze.util :as util]))

;; # Utils

(defn var->sym [var]
  (assert (var? var))
  (symbol (str (.name (.ns var))) (str (.sym var))))

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
               " Found: " type))
  (swap! type-db #(assoc % sym type)))

;; # Subtyping

(defn subtype [super sub]
  (assert (= super sub))
  sub)

;; # Types and front end Macros

(defmacro new-type [& body]
  `(def ~@body))

(def union vector)

(def third (comp second rest))

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
       (+T ~(symbol (str "->" name)) ~(conj (vec (interleave (map third typed-fields) (repeat :->))) name))
       ~(@#'clojure.core/build-positional-factory gname classname fields)
       (swap! type-db #(assoc % (resolve '~name)
                              '~(apply merge (map (fn [[n _ t]] {n t}) typed-fields))))
       ~classname)))

(defmacro +T [nme type]
  `(do
     (declare ~nme)
     (add-type (var->sym (resolve '~nme)) ~type)))

;; # Type Annotations for core functions

(+T add-type [Symbol :-> Object])

;; # Type Checker

(defn type-error []
  (throw (Exception. "Type error")))

(defmulti type-check :op)

(defmethod type-check :var
  [{:keys [var]}]
  (let [sym (var->sym var)]
    (println @type-db)
    (type-of sym)))

(defmethod type-check :invoke
  [{:keys [fexpr args]}]
  (let [type (type-check fexpr)]
    (type-error)))

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
    (let [init-type (type-check init)
          expected-type (type-of (var->sym var))]
      (subtype expected-type init-type))
    (println "No init provided for" (var->sym var))))

(defmethod type-check :default
  [expr]
  (util/print-expr expr :children :env :Expr-obj :ObjMethod-obj))

; # Type checker interface

(defn type-check-path [path nsym]
  (let [analysis (analyze-path path nsym)]
    (doseq [a (rest analysis)]
      (type-check a))))

