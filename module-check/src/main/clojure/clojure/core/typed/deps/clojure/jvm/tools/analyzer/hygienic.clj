(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.hygienic
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.core.typed.deps.clojure.jvm.tools.analyzer.fold :refer [derive-default-fold add-fold-case fold-expr]]
            [clojure.core.typed.deps.clojure.jvm.tools.analyzer.emit-form :refer [map->form derive-emit-default]]
            [clojure.core.typed.deps.clojure.jvm.tools.analyzer :as ana]))

(declare hygienic-emit hygienic-ast)

(defn ast-hy 
  "Perform hygienic transformation on an AST

  eg. (-> (ast ...) ast-hy)"
  [expr]
  (hygienic-ast expr {}))

(defn emit-hy 
  "Emit an already-hygienic AST as a form.

  eg. (-> (ast ...) ast-hy emit-hy)"
  [expr]
  (map->form expr hygienic-emit))

(def hsym-key ::hygienic-sym)
(def hname-key ::hygienic-name)

(defn macroexpand 
  "Fully macroexpand a form, using hygiene."
  [f]
  (-> f
      ana/analyze-form
      ast-hy
      emit-hy))

;; emit

(def hygienic-emit ::hygienic-emit)

(derive-emit-default hygienic-emit)

(defmethod map->form [:local-binding hygienic-emit]
  [expr _]
  (hsym-key expr))

(defmethod map->form [:fn-expr hygienic-emit]
  [{:keys [methods] :as expr} mode]
  (list* 'fn* 
         (concat
           (when-let [name (hname-key expr)]
             [name])
           (map #(map->form % mode) methods))))

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

(defn hygienic-local-binding [{:keys [op init sym] :as local-binding} scope new-sym?]
  {:pre [(= :local-binding op)]}
  (let [hy-init (when init
                  (hygienic-ast init scope))
        hy-sym (if new-sym?
                 (hygienic-sym scope sym)
                 (scope sym))
        _ (assert hy-sym (str "Local " sym " not in scope."))]
    (assoc local-binding
           :init hy-init
           hsym-key hy-sym)))

;[(IPersistentMap Symbol HSymbol) Symbol HSymbol -> (IPersistentMap Symbol HSymbol)]
(defn add-scope [scope sym hy-sym]
  {:pre [sym hy-sym scope]}
  (assoc scope sym hy-sym))

;let
(add-fold-case ::hygienic
  :let
  (fn [{:keys [binding-inits body] :as expr}
       {{scope ::scope} :locals :as locals}]
    (assert scope locals)
    (let [[hy-binding-inits scope]
          (reduce (fn [[hy-binding-inits scope] binding-init]
                    {:pre [(vector? hy-binding-inits)]}
                    (let [sym (-> binding-init :local-binding :sym)
                          update-init #(when % (hygienic-ast % scope))
                          hy-sym (hygienic-sym scope sym)
                          hy-binding-init (-> binding-init
                                            (update-in [:init] update-init)
                                            (update-in [:local-binding :init] update-init)
                                            (assoc-in [:local-binding hsym-key] hy-sym))
                          new-scope (add-scope scope sym hy-sym)]
                      [(conj hy-binding-inits hy-binding-init) new-scope]))
                  [[] scope] binding-inits)

          ;with new scope
          hy-body (hygienic-ast body scope)]
      (assoc expr 
             :binding-inits hy-binding-inits 
             :body hy-body))))

(defn hygienic-name [name scope]
  (let [hy-name (when name
                  (hygienic-sym scope name))
        new-scope (if hy-name
                    (add-scope scope name hy-name)
                    scope)]
    [hy-name new-scope]))

;fn-expr
(add-fold-case ::hygienic
  :fn-expr
  (fn [{:keys [name methods] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-name scope] (hygienic-name name scope)
          hy-methods (map #(hygienic-ast % scope) methods)]
      (assoc expr
             hname-key hy-name
             :methods hy-methods))))

(defn hygienic-lbs [lbs scope]
  (reduce (fn [[hy-lbs scope] {:keys [sym] :as local-binding}]
            {:pre [(vector? hy-lbs)]}
            (let [hy-local-binding (hygienic-local-binding local-binding scope true)
                  hy-sym (hsym-key hy-local-binding)
                  new-scope (add-scope scope sym hy-sym)]
              [(conj hy-lbs hy-local-binding) new-scope]))
          [[] scope] lbs))

;fn-method
(add-fold-case ::hygienic
  :fn-method
  (fn [{:keys [required-params rest-param body] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-required-params scope] (hygienic-lbs required-params scope)
          [[hy-rest-param] scope] (if rest-param
                                    (hygienic-lbs [rest-param] scope)
                                    [[rest-param] scope])
          ; use new scope
          hy-body (hygienic-ast body scope)]
      (assoc expr
             :required-params hy-required-params
             :rest-param hy-rest-param
             :body hy-body))))

;local-binding-expr
(add-fold-case ::hygienic
  :local-binding
  (fn [{:keys [sym init] :as expr}
       {{scope ::scope} :locals}]
    (hygienic-local-binding expr scope false)))

(add-fold-case ::hygienic
  :catch
  (fn [{:keys [local-binding handler] :as expr}
       {{scope ::scope} :locals}]
    (let [hy-local-binding (hygienic-local-binding local-binding scope true)
          scope (add-scope scope (:sym hy-local-binding) (hsym-key hy-local-binding))
          hy-handler (hygienic-ast handler scope)]
    (assoc expr
           :local-binding hy-local-binding
           :handler hy-handler))))

(add-fold-case ::hygienic
  :deftype*
  (fn [{:keys [fields methods] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-fields scope]
          (reduce (fn [[hy-lbs scope] lb]
                    (let [hy-lb (hygienic-local-binding lb scope true)
                          scope (add-scope scope (:sym hy-lb) (hsym-key hy-lb))]
                      [(conj hy-lbs hy-lb) scope]))
                  [#{} scope] fields)
          hy-methods (map #(hygienic-ast % scope) methods)]
      (assoc expr
             :fields hy-fields
             :methods hy-methods))))

(add-fold-case ::hygienic
  :new-instance-method
  (fn [{:keys [name required-params body] :as expr}
       {{scope ::scope} :locals}]
    (let [[hy-name scope] (hygienic-name name scope)
          [hy-required-params scope] (hygienic-lbs required-params scope)
          ; use new scope
          hy-body (hygienic-ast body scope)]
      (assoc expr
             :name hy-name
             :required-params hy-required-params
             :body hy-body))))

(add-fold-case ::hygienic
  :letfn
  (fn [{:keys [binding-inits body] :as expr}
       {{scope ::scope} :locals}]
    (let [;find scope of each binding init and body
          ;binit-hsyms is a vector of symbols, corresponding to the hsym for each binit
          [binit-hsyms scope]
          (reduce (fn [[hsyms scope] sym]
                    (let [hsym (hygienic-sym scope sym)]
                      [(conj hsyms hsym) (add-scope scope sym hsym)]))
                  [[] scope]
                  (map #(-> % :local-binding :sym) binding-inits))
          hy-binding-inits
          (reduce (fn [hy-binding-inits [hy-sym binding-init]]
                    (let [hy-binding-init (-> binding-init
                                            (update-in [:init] hygienic-ast scope)
                                            (update-in [:local-binding :init] hygienic-ast scope)
                                            (assoc-in [:local-binding hsym-key] hy-sym))]
                      (conj hy-binding-inits hy-binding-init)))
                  [] (map vector binit-hsyms binding-inits))
          hy-body (hygienic-ast body scope)]
      (assoc expr
             :binding-inits hy-binding-inits
             :body hy-body))))

(comment
  (refer 'clojure.core.typed.deps.clojure.jvm.tools.analyzer :only '(ast))
  (-> (ast (let [a 1 a a b a a a] a)) ast-hy emit-hy)

  (-> (ast (fn a [a a] a)) ast-hy emit-hy)
  (-> (ast (fn [a a & a] a)) ast-hy emit-hy)
  (-> (ast (let [a 1] (fn a [] a))) ast-hy emit-hy)
  (-> (ast (let [a 1] (try a (catch Exception a a)))) ast-hy emit-hy)

  (-> (ast (deftype A [a] Object (toString [_] a))) ast-hy emit-hy)
  (-> (ast (deftype A [a] Object (toString [a] a))) ast-hy emit-hy)
  
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
  )
