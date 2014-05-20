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

(defn quote-expr-val [{:keys [op expr] :as q}]
  {:pre [(or (and (#{:quote} op)
                  (#{:const} (:op expr)))
             (#{:const} op))]}
  (if (#{:quote} op)
    (:val expr)
    (:val q)))

(defn dummy-invoke-expr [fexpr args env]
  {:op :invoke
   :env env
   :fn fexpr
   :args args})

(defn dummy-fn-method-expr [body required-params rest-param env]
  {:op :fn-method
   :env env
   :body body
   :params (vec (concat required-params (when rest-param [rest-param])))
   :variadic? (boolean rest-param)})

(defn dummy-fn-expr [methods variadic-method env]
  {:op :fn
   :env env
   :methods (vec (concat methods (when variadic-method [variadic-method])))
   :variadic? (boolean variadic-method)})

(defn dummy-local-binding-expr [sym env]
  {:op :local
   :env env
   :name sym})

(defn dummy-var-expr [vsym env]
  (let [v (resolve vsym)]
    (assert (var? v))
    {:op :var
     :env env
     :var v}))

(defn dummy-do-expr [statements ret env]
  {:op :do
   :statements statements
   :ret ret
   :env env})

(defn dummy-const-expr [val env]
  {:op :const
   :val val
   :env env})
