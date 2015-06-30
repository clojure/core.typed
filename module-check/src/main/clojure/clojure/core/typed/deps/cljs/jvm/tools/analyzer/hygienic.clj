(ns clojure.core.typed.deps.cljs.jvm.tools.analyzer.hygienic
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.core.typed.deps.cljs.jvm.tools.analyzer.fold :refer [derive-default-fold add-fold-case fold-expr]]
            [clojure.core.typed.deps.cljs.jvm.tools.analyzer.emit-form :as emit :refer [map->form derive-emit-default]]
            [clojure.core.typed.deps.cljs.jvm.tools.analyzer :as ana]
            [cljs.analyzer]
            [cljs.compiler]
            [cljs.core]))

(declare hygienic-emit hygienic-ast)

(defn ast-hy 
  "Perform hygienic transformation on an AST

  eg. (-> (ast ...) ast-hy)"
  [expr]
  (hygienic-ast expr {}))

(defn emit-hy 
  "Emit a AST as a form."
  [expr]
  (emit/emit-form expr))

(defn macroexpand 
  "Fully macroexpand a form, using hygiene."
  [f]
  (-> f
      ana/analyze-form
      ast-hy
      emit-hy))

;; fold

(derive-default-fold ::hygienic)

(declare hygienic-ast)

(defn hygienic-ast [expr scope]
  (assert expr)
  (assert scope)
  (fold-expr ::hygienic
    {:expr-rec #(hygienic-ast % scope)
     :locals {::scope scope}}
    expr))

(defn hygienic-sym [scope sym]
  ;only generate unique when shadowing
  (if (scope sym)
    (gensym sym)
    sym))

(defn hygienic-var-info [{:keys [name] :as local-binding} scope new-sym?]
  {:pre [name]}
  (let [hy-sym (if new-sym?
                 (hygienic-sym scope name)
                 (scope name))
        _ (assert hy-sym (str "Local " name " not in scope."))]
    (assoc local-binding
           :name hy-sym)))

;[(IPersistentMap Symbol HSymbol) Symbol HSymbol -> (IPersistentMap Symbol HSymbol)]
(defn add-scope [scope sym hy-sym]
  {:pre [sym hy-sym scope]}
  (assoc scope sym hy-sym))

(defn hygienic-let-and-loop
  [{:keys [bindings expr] :as let-loop-expr}
   {{scope ::scope} :locals :as locals}]
  {:pre [bindings expr]}
  (assert scope locals)
  (let [[hy-binding-inits scope]
        (reduce (fn [[hy-binding-inits scope] binding-init]
                  {:pre [(vector? hy-binding-inits)]}
                  (let [sym (-> binding-init :name)
                        _ (assert (symbol? sym))
                        update-init #(hygienic-ast % scope)
                        hy-sym (hygienic-sym scope sym)
                        _ (assert (symbol? hy-sym))
                        hy-binding-init (-> binding-init
                                            (update-in [:init] update-init)
                                            (assoc-in [:name] hy-sym))
                        new-scope (add-scope scope sym hy-sym)]
                    [(conj hy-binding-inits hy-binding-init) new-scope]))
                [[] scope] bindings)

        ;with new scope
        hy-body (hygienic-ast expr scope)]
    (assoc let-loop-expr 
           :bindings hy-binding-inits 
           :expr hy-body)))
;let, loop
(add-fold-case ::hygienic :let hygienic-let-and-loop)
(add-fold-case ::hygienic :loop hygienic-let-and-loop)

(defn hygienic-name [name scope]
  (let [hy-name (when name
                  (hygienic-sym scope name))
        new-scope (if hy-name
                    (add-scope scope name hy-name)
                    scope)]
    [hy-name new-scope]))

(declare hygienic-fn-method)

;fn-expr
(add-fold-case ::hygienic
  :fn
  (fn [{:keys [name methods] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-name scope] (hygienic-name (:name name) scope)
          fields (-> expr :form meta :cljs.analyzer/fields)
          hy-methods (map #(hygienic-fn-method % scope fields) methods)]
      (assoc expr
             :name (assoc name :name hy-name)
             :methods hy-methods))))

(defn hygienic-lbs [lbs scope]
  (reduce (fn [[hy-lbs scope] {:keys [name] :as local-binding}]
            {:pre [(vector? hy-lbs) name]}
            (let [hy-local-binding (hygienic-var-info local-binding scope true)
                  hy-sym (:name hy-local-binding)
                  new-scope (add-scope scope name hy-sym)]
              [(conj hy-lbs hy-local-binding) new-scope]))
          [[] scope] lbs))

; Note: :variadic is confusing here. Should be :variadic?
; :params have all fixed and variable parameters
;fn-method
(defn hygienic-fn-method [{:keys [params expr] :as method-expr} scope fields]
  (let [; extend-type sometimes puts implicit fields in fn* metadata
        scope (reduce (fn [scope fld] (add-scope scope fld fld))
                      scope
                      fields)
        [hy-required-params scope] (hygienic-lbs params scope)
        ; use new scope
        hy-body (hygienic-ast expr scope)]
    (assoc method-expr
           :params hy-required-params
           :expr hy-body)))

;local-binding-expr
(add-fold-case ::hygienic
  :var
  (fn [{:keys [] :as expr}
       {{scope ::scope} :locals}]
    (if (namespace (-> expr :info :name))
      expr
      (-> expr
          (update-in [:info] hygienic-var-info scope false)))))

;Note: :name is the variable bound by catch
(add-fold-case ::hygienic
  :try
  (fn [{:keys [try catch finally name] :as expr}
       {{scope ::scope} :locals}]
    (let [htry (hygienic-ast try scope)
          ; scope the caught exception. Usually is already gensym'd?
          hcatch (hygienic-ast catch (add-scope name name))
          hfinally (hygienic-ast finally scope)]
      (assoc expr
             :try htry
             :catch hcatch
             :finally hfinally))))

(add-fold-case ::hygienic
  :letfn
  (fn [{:keys [bindings expr] :as letfn-expr}
       {{scope ::scope} :locals}]
    (let [;find scope of each binding init and body
          ;binit-hsyms is a vector of symbols, corresponding to the hsym for each binit
          [binit-hsyms scope]
          (reduce (fn [[hsyms scope] sym]
                    (let [hsym (hygienic-sym scope sym)]
                      [(conj hsyms hsym) (add-scope scope sym hsym)]))
                  [[] scope]
                  (map :name bindings))
          hy-binding-inits
          (reduce (fn [hy-binding-inits [hy-sym binding-init]]
                    (let [hy-binding-init (-> binding-init
                                            (update-in [:init] hygienic-ast scope)
                                            (assoc-in [:name] hy-sym))]
                      (conj hy-binding-inits hy-binding-init)))
                  [] (map vector binit-hsyms bindings))
          hy-body (hygienic-ast expr scope)]
      (assoc letfn-expr
             :bindings hy-binding-inits
             :expr hy-body))))

(comment
  (require '[clojure.core.typed.deps.cljs.jvm.tools.analyzer :refer [ast]])
  (require '[cljs.repl.reflect :as repl])
  (-> (ast (let [a 1] a)) ast-hy emit-hy)
  (-> (ast (let [a 1 a a b a a a] a)) ast-hy emit/emit-form)

  (-> (ast (fn a [a a] a)) ast-hy emit-hy)
  (-> (ast (let [a 1] (fn a [a a] a))) ast-hy emit-hy)
  (-> (ast (fn [a a & a] a)) ast-hy emit-hy)
  (-> (ast (let [a 1] (fn a [] a))) ast-hy emit-hy)
  (-> (ast (let [a 1] (try a (catch Exception a a)))) #_ast-hy emit-hy)

  (-> (ast (deftype A [a] Object (toString [_] a))) ast-hy emit-hy)
  (-> (ast (deftype A [a] Object (toString [a] a))) ast-hy emit-hy)
  
  (->
    (repl/macroexpand
      '(deftype Pair [lhs rhs]
        clojure.lang.Counted
        (count [_] lhs 2)))
    clojure.pprint/pprint)
  (->
    (ast
      (deftype Pair [lhs rhs]
        clojure.lang.Counted
        (count [_] 2)))
    ast-hy emit-hy)

  (->
    (ast
      (deftype Pair [lhs rhs]
        clojure.lang.Counted
        (count [_] 2)
        clojure.lang.Indexed
        (nth [_ i] (case i
                     0 lhs
                     1 rhs
                     (throw (IndexOutOfBoundsException.))))
        (nth [_ i not-found] (case i
                               0 lhs
                               1 rhs
                               not-found))
        java.util.Map$Entry
        (getKey [_] lhs)
        (getValue [_] rhs)
        Object
        (toString [_]
          (str "(" lhs " . " rhs ")")))
      )
    ast-hy emit-hy)

  (-> (ast (letfn [(a [])
                   (b [a] a)
                   (c [c] a)
                   (a [b c] b)]
             (a b c)))
    ast-hy emit-hy)

  (-> (binding [cljs.analyzer/*cljs-ns* cljs.analyzer/*cljs-ns*]
        (-> (ast (ns foo))
            ast-hy emit-hy)))

  (-> (binding [cljs.analyzer/*cljs-ns* cljs.analyzer/*cljs-ns*]
        (-> (ast (def a))
            ast-hy emit-hy)))

  (-> (ast {1 1})
      ast-hy emit-hy)

  (-> (ast #{1})
      ast-hy emit-hy)
  (-> (ast (let [a 1 a a] `('~a 2)))
      :expr
      :ret :op)
  (-> (ast (let [a 1 a a] `('~a 2)))
      ast-hy
      emit-hy)
  )
