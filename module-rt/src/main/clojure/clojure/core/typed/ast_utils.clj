(ns clojure.core.typed.ast-utils
  (:require [clojure.core.typed.current-impl :as impl]
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
  {:pre [(#{:quote} (:op expr))
         (#{:const} (:op (:expr expr)))]}
  (-> expr :expr :val))

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
  (or ; tools.analyzer 0.3.x
      (let [cls (:class expr)]
        (when (class? cls)
          cls))
      ; future tools.analyzer
      (let [{:keys [val]} (:class expr)]
        val)))

(defn catch-op-class [expr]
  {:pre [(#{:catch} (:op expr))]
   :post [(class? %)]}
  (or ; tools.analyzer 0.3.x
      (let [cls (:class expr)]
        (when (class? cls)
          cls))
      ; future tools.analyzer
      (let [{:keys [val]} (:class expr)]
        val)))

(defn protocol-invoke->invoke [expr]
  {:pre [(map? expr)
         (#{:protocol-invoke} (:op expr))]
   :post [(#{:invoke} (:op expr))]}
  (-> expr
      (dissoc :protocol-fn :target)
      (assoc 
        :op :invoke
        :children [:fn :args]
        :fn (:protocol-fn expr)
        :args (vec (concat (:target expr)
                           (:args expr))))))

(defn invoke->protocol-invoke [expr]
  {:pre [(map? expr)
         (#{:invoke} (:op expr))]
   :post [(#{:protocol-invoke} (:op expr))]}
  (-> expr
      (dissoc :meta :fn)
      (assoc :op :protocol-invoke
             :children [:protocol-fn :target :args]
             :protocol-fn (:fn expr)
             :target (first (:args expr))
             :args (vec (rest (:args expr))))))

(defn primitive-invoke->invoke [expr]
  {:pre [(map? expr)
         (#{:primitive-invoke} (:op expr))]
   :post [(#{:invoke} (:op expr))]}
  (-> expr
      (assoc :op :invoke)))

(defn invoke->primitive-invoke [expr]
  {:pre [(map? expr)
         (#{:invoke} (:op expr))]
   :post [(#{:primitive-invoke} (:op expr))]}
  (-> expr
      (assoc :op :primitive-invoke)))
