(ns ^:skip-wiki clojure.core.typed.ast-utils
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.coerce-utils :as coerce]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST ops


;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(defn emit-form-fn [expr]
  (impl/impl-case
    :clojure (do (require '[clojure.tools.analyzer.passes.jvm.emit-form])
                 ((impl/v 'clojure.tools.analyzer.passes.jvm.emit-form/emit-form) expr))
    :cljs (do
            (require '[clojure.core.typed.util-cljs])
            ((impl/v 'clojure.core.typed.util-cljs/emit-form) expr))))

(defn constant-expr [expr]
  {:pre [(#{:quote} (:op expr))]}
  (do (assert (#{:const} (:op (:expr expr))))
      (-> expr :expr :val)))

(defn do-statements-value [cexprs]
  ((impl/impl-case
     :clojure vec
     :cljs seq)
   (butlast cexprs)))

(defn map-expr-at [expr key]
  (impl/impl-case
    :clojure (let [_ (assert (#{:const} (:op expr)))
                   v (:val expr)]
               (assert (contains? v key) key)
               (get v key))
    :cljs (let [_ (assert (#{:map} (:op expr)))
                m (zipmap (map :form (:keys expr))
                          (:vals expr))
                _ (assert (contains? m key))
                vexpr (get m key)]
            (:form vexpr))))

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

(defn method-body-kw []
  (impl/impl-case
    :clojure :body
    :cljs :expr))

(defn method-required-params [method]
  (impl/impl-case
    ; :variadic? in tools.analyzer
    :clojure (case (:op method)
               (:fn-method) ((if (:variadic? method) butlast identity)
                             (:params method))
               ;include deftype's 'this' param
               (:method) (concat [(:this method)] (:params method)))
    ; :variadic in CLJS
    :cljs ((if (:variadic method) butlast identity)
           (:params method))))

(defn method-rest-param [method]
  (impl/impl-case
    ; :variadic? in tools.analyzer
    :clojure (case (:op method)
               ;deftype methods are never variadic
               (:method) nil
               (:fn-method) ((if (:variadic? method) last (constantly nil))
                             (:params method)))
    ; :variadic in CLJS
    :cljs ((if (:variadic method) last (constantly nil))
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
  (impl/impl-case
    :clojure :body
    :cljs :expr))

(defn def-var-name [expr]
  {:post [(symbol? %)]}
  (impl/impl-case
    :clojure (coerce/var->symbol (:var expr))
    :cljs (:name expr)))

(defn new-op-class [expr]
  {:pre [(#{:new} (:op expr))]
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
                  (impl/impl-case
                    :clojure ((every-pred map? (comp #{:fn-method} :op))
                              m)
                    :cljs (map? m))))
(def fn-methods? (fn [ms]
                   (impl/impl-case
                     :clojure ((con/vec-c? fn-method?) ms)
                     :cljs ((every-pred (con/every-c? fn-method?)
                                        seq?)
                            ms))))

(defn variadic-method? [m]
  {:pre [((some-fn fn-method? deftype-method?) m)]
   :post [(con/boolean? %)]}
  (cond
    (fn-method? m)
    (impl/impl-case
      :clojure (do (contains? m :variadic?)
                   (:variadic? m))
      :cljs (do (assert (contains? m :variadic))
                (boolean (:variadic m))))
    ; :method does not have :variadic? field
    :else false))

(defn fixed-arity 
  "Returns the number of parameters for a :fn-method or :method.
  Note :method AST nodes include the 'this' parameter."
  [m]
  {:pre [((some-fn fn-method? deftype-method?) m)]
   :post [(integer? %)]}
  (impl/impl-case
    :clojure (let [fixed (:fixed-arity m)]
               (assert (integer? fixed))
               ((if (fn-method? m) identity inc) fixed))
    :cljs (do (assert (fn-method? m))
              (assert (contains? m :params))
              (count (:params m)))))

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
