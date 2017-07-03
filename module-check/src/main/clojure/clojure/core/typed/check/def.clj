(ns clojure.core.typed.check.def
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed :as T]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.type-ctors :as c])
  (:import (clojure.lang Var)))

(defn init-provided? [expr]
  (contains? expr :init))

;[Expr (Option TCResult) -> Expr]
(defn check-normal-def
  "Checks a def that isn't a macro definition."
  [check-fn {:keys [meta init env] :as expr} & [expected]]
  {:post [(:init %)]}
  (let [init-provided (init-provided? expr)
        _ (assert init-provided)
        vsym (ast-u/def-var-name expr)
        warn-if-unannotated? (ns-opts/warn-on-unannotated-vars? (cu/expr-ns expr))
        t (var-env/lookup-Var-nofail vsym)
        ;_ (prn "lookup var" vsym t)
        check? (var-env/check-var? vsym)
        ;_ (prn "check? var" vsym check?)
        cljs-ret (r/ret r/-any)]
    (cond
      ; check against an expected type
      (and check? t)
      (let [cinit (when init-provided
                    (binding [vs/*current-env* (:env init)
                              vs/*current-expr* init]
                      (check-fn init (r/ret t))))
            cmeta (when meta
                    (binding [vs/*current-env* (:env meta)
                              vs/*current-expr* meta]
                      (check-fn meta)))
            _ (when cinit
                ; now consider this var as checked
                (var-env/add-checked-var-def vsym))]
        (p/p :check/checked-def)
        (assoc expr
               :init cinit
               :meta cmeta
               u/expr-type (below/maybe-check-below
                             (impl/impl-case
                               :clojure (r/ret (c/RClass-of Var [t t])
                                               (fo/-true-filter))
                               :cljs cljs-ret)
                             expected)))

      ; if warn-if-unannotated?, don't try and infer this var,
      ; just skip it
      (or (not check?) 
          (and warn-if-unannotated?
               (not t)))
      (do (println (when-let [line (-> expr :env :line)] 
                     (str line ": ")) 
                   "Not checking" vsym "definition")
          (flush)
          (p/p :check/def-not-checking-definition)
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (impl/impl-case
                                 :clojure (r/ret (c/RClass-of Var [(or t r/-nothing) (or t r/-any)])
                                                 (fo/-true-filter))
                                 :cljs cljs-ret)
                               expected)))

      ;otherwise try and infer a type
      :else
      (let [_ (assert (not t))
            cinit (when init-provided
                    (check-fn init))
            cmeta (when meta
                    (binding [vs/*current-env* (:env meta)
                              vs/*current-expr* meta
                              ;; emit-form does not currently
                              ;; emit :meta nodes in a :def. Don't
                              ;; try and rewrite it, just type check.
                              vs/*can-rewrite* false]
                      (check-fn meta)))
            inferred (r/ret-t (u/expr-type cinit))
            _ (assert (r/Type? inferred))
            _ (when cinit
                ; now consider this var as checked
                (var-env/add-checked-var-def vsym)
                ; and add the inferred static type (might be Error)
                (var-env/add-var-type vsym inferred))]
        (p/p :check/checked-def)
        (assoc expr
               :init cinit
               :meta cmeta
               u/expr-type (below/maybe-check-below
                             (impl/impl-case
                               :clojure (r/ret (c/RClass-of Var [inferred inferred])
                                               (fo/-true-filter))
                               :cljs cljs-ret)
                             expected))))))

(defn defmacro-or-declare? 
  "Returns true if this :def AST originally a defmacro or declare."
  [{:keys [^Var var] :as expr}]
  (or (.isMacro var)
      (not (init-provided? expr))))

(defn check-defmacro-or-declare
  "To check a defmacro or declare, just assign it the most general
  Var type and ignore the body."
  [expr expected]
  (p/p :check/ignored-typed-defmacro
       (assoc expr
              u/expr-type (below/maybe-check-below
                            (r/ret (c/RClass-of Var [r/-nothing r/-any]))
                            expected))))

(defn check-def
  "Check a def. If it is a declare or a defmacro, don't try and check it."
  [check-fn {:keys [var init env] :as expr} expected]
  ;(prn " Checking def" var)
  (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
            vs/*current-expr* expr]
    (cond 
      ;ignore macro definitions and declare
      (defmacro-or-declare? expr) (check-defmacro-or-declare expr expected)

      :else (check-normal-def check-fn expr expected))))

(defn add-checks-normal-def
  "Add runtime checks to a def with an initial value."
  [check-fn expr expected]
  (let [_ (assert (init-provided? expr))
        vsym (ast-u/def-var-name expr)
        check? (var-env/check-var? vsym)
        t (when check?
            (var-env/lookup-Var-nofail vsym))]
    (assoc expr
           :init (if t
                   ;;cast immediately, don't propagate type.
                   (cu/add-cast
                     (check-fn (:init expr) nil)
                     t
                     {:positive "cast"
                      :negative "cast"})
                   ;;
                   (check-fn (:init expr) nil)))))
