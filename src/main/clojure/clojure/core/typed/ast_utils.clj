(ns clojure.core.typed.ast-utils
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST ops


;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(defn emit-form-fn [expr]
  (impl/impl-case
    :clojure (emit-form/emit-form expr)
    :cljs (do
            (require '[clojure.core.typed.util-cljs])
            ((impl/v 'clojure.core.typed.util-cljs/emit-form) expr))))

(defn constant-expr [expr]
  {:pre [(#{:quote} (:op expr))
         (#{:const} (:op (:expr expr)))]}
  (-> expr :expr :val))

(defn constant-exprs [exprs]
  (map constant-expr exprs))
