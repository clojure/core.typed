(ns typed-clojure.frontend
  (:use [analyze.core :only [analyze-path]])
  (:require [analyze.util :as util]
            [typed-clojure.primitives :as prim]))

(defn type-error []
  (throw (Exception. "Type error")))

(defmulti type-check :op)

(defmethod type-check :var
  [{:keys [var]}]
  (let [sym (symbol (.name (.ns var)) (.sym var))
        type (prim/type-db sym)]
    (assert type)
    type))

(defmethod type-check :invoke
  [{:keys [fexpr args]}]
  (let [type (type-check fexpr)]
    (type-error)))

(defmethod type-check :new-instance-expr
  [{:keys [methods mmap]}]
  (type-error))

(defmethod type-check :do
  [{:keys [children]}]
  (doseq [child children]
    (type-check child)))

(defmethod type-check :def
  [{:keys [env form init-provided init name]}]
  (if init-provided
    (type-error)
    (println "No init provided")))

(defn type-check-path [path nsym]
  (let [analysis (analyze-path path nsym)]
    (map type-check (rest analysis))))

(type-check "typed_clojure/tree.clj" 'typed-clojure.tree)

(comment
  (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree)
  (util/print-expr (first (analyze-path "typed_clojure/tree.clj" 'typed-clojure.tree))
                   :children :env :Expr-obj :ObjMethod-obj)
  )
