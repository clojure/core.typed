;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc ^:skip-wiki clojure.core.typed.ast-utils
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.coerce-utils :as coerce]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST ops


;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(let [emit-form-clj (delay (impl/dynaload 'clojure.tools.analyzer.passes.jvm.emit-form/emit-form))
      emit-form-cljs (delay (impl/dynaload 'clojure.core.typed.util-cljs/emit-form))]
  (defn emit-form-fn [expr]
    (impl/impl-case
      :clojure (@emit-form-clj expr)
      :cljs (@emit-form-cljs expr))))

(defn constant-expr [expr]
  {:pre [(#{:quote} (:op expr))
         (#{:const} (:op (:expr expr)))]}
  (-> expr :expr :val))

(let [constant-lift (delay (impl/dynaload 'clojure.tools.analyzer.passes.jvm.constant-lifter/constant-lift))]
  (defn map-expr-at [expr key]
    (impl/impl-case
      :clojure (case (:op expr)
                 :map (let [const (@constant-lift expr)]
                        (assert (#{:const} (:op const)))
                        (map-expr-at const key))
                 :const (let [v (:val expr)]
                          (assert (contains? v key) key)
                          (get v key)))
      :cljs (let [_ (assert (#{:map} (:op expr)))
                  m (zipmap (map :form (:keys expr))
                            (:vals expr))
                  _ (assert (contains? m key))
                  vexpr (get m key)]
              (:form vexpr)))))

(defn constant-exprs [exprs]
  (map constant-expr exprs))

(defn quote-expr-val [{:keys [op expr] :as q}]
  {:pre [(or (and (#{:quote} op)
                  (#{:const} (:op expr)))
             (#{:const} op))]}
  (if (#{:quote} op)
    (:val expr)
    (:val q)))

(defn dummy-if-expr [test then else env]
  {:op :if
   :test test
   :then then
   :else else
   :children [:test :then :else]
   :env env})

(defn dummy-invoke-expr [fexpr args env]
  {:op :invoke
   :env env
   :children [:fn :args]
   :fn fexpr
   :args args})

(defn dummy-fn-method-expr [body required-params rest-param env]
  (let [params (vec (concat required-params (when rest-param [rest-param])))]
    {:op :fn-method
     :env env
     :children [:body]
     :body body
     :params params
     :fixed-arity (count params)
     :variadic? (boolean rest-param)}))

(defn dummy-fn-expr [methods variadic-method env]
  {:op :fn
   :env env
   :children [:methods]
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
     :var v
     :form vsym}))

(defn dummy-do-expr [statements ret env]
  {:op :do
   :statements statements
   :ret ret
   :env env})

(defn dummy-const-expr [val env]
  {:op :const
   :val val
   :env env
   :form val})

;; FIXME delete
(defn method-body-kw []
  :body)

(defn method-required-params [method]
  (case (:op method)
    (:fn-method) ((if (:variadic? method) butlast identity)
                  (:params method))
    ;include deftype's 'this' param
    (:method) (concat [(:this method)] (:params method))))

(defn method-rest-param [method]
  (case (:op method)
    ;deftype methods are never variadic
    (:method) nil
    (:fn-method) ((if (:variadic? method) last (constantly nil))
                  (:params method))))

(defn reconstruct-arglist [method required-params rest-param]
  (impl/impl-case
    :clojure (case (:op method) 
               :fn-method (assoc method
                                 :params (vec (concat required-params
                                                      (when rest-param
                                                        [rest-param]))))
               :method (do (assert (nil? rest-param))
                           (assert (seq required-params))
                           (assoc method
                                  :this (first required-params)
                                  :params (vec (rest required-params)))))
    :cljs (assoc method
                 :params (vec (concat required-params
                                      (when rest-param
                                        [rest-param]))))))

(defn let-body-kw []
  :body)

(defn def-var-name [expr]
  {:post [(symbol? %)]}
  (impl/impl-case
    :clojure (coerce/var->symbol (:var expr))
    :cljs (:name expr)))

(defn new-op-class [expr]
  {:pre [(#{:new} (:op expr))
         (#{:const} (:op (:class expr)))]
   :post [(class? %)]}
  (-> expr :class :val))

(defn catch-op-class [expr]
  {:pre [(#{:catch} (:op expr))]
   :post [(class? %)]}
  ; future tools.analyzer
  (-> expr :class :val))

(def deftype-method? (fn [m]
                       (impl/impl-case
                         :clojure ((every-pred map? (comp #{:method} :op))
                                   m)
                         ; FIXME should be nyi-error but c.c.t.errors depends on this namespace
                         :cljs (assert nil "Method for CLJS"))))

(def fn-method? (fn [m]
                  ((every-pred map? (comp #{:fn-method} :op))
                   m)))
(def fn-methods? (fn [ms]
                   (impl/impl-case
                     :clojure ((con/vec-c? fn-method?) ms)
                     :cljs ((every-pred (con/every-c? fn-method?)
                                        seq?)
                            ms))))

(defn variadic-method? [m]
  {:pre [((some-fn fn-method? deftype-method?) m)]
   :post [(boolean? %)]}
  (cond
    (fn-method? m) (:variadic? m)
    ; :method does not have :variadic? field
    :else false))

(defn fixed-arity 
  "Returns the number of parameters for a :fn-method or :method.
  Note :method AST nodes include the 'this' parameter."
  [m]
  {:pre [((some-fn fn-method? deftype-method?) m)]
   :post [(integer? %)]}
  (let [fixed (:fixed-arity m)]
    (assert (integer? fixed))
    ((if (fn-method? m) identity inc) fixed)))

(defn walk-children [check {:keys [children] :as expr}]
  (reduce
    (fn [expr c]
      (update expr c 
              (fn [ce]
                (if (vector? ce)
                  (mapv check ce)
                  (check ce)))))
    expr
    children))
