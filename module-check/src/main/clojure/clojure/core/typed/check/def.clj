(ns clojure.core.typed.check.def
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.ns-options :as ns-opts]
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

;[Expr (Option TCResult) -> Expr]
(defn check-normal-def [check-fn {:keys [init env] :as expr} & [expected]]
  {:post [(:init %)]}
  (let [init-provided (contains? expr :init)
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
            _ (when cinit
                ; now consider this var as checked
                (var-env/add-checked-var-def vsym))]
        (p/p :check/checked-def)
        (assoc expr
               :init cinit
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
               u/expr-type (below/maybe-check-below
                             (impl/impl-case
                               :clojure (r/ret (c/RClass-of Var [inferred inferred])
                                               (fo/-true-filter))
                               :cljs cljs-ret)
                             expected))))))
