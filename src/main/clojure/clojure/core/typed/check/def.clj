(ns clojure.core.typed.check.def
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c])
  (:import (clojure.lang Var)))

;[Expr (Option TCResult) -> Expr]
(defn check-normal-def [check-fn {:keys [var init env] :as expr} & [expected]]
  {:post [(:init %)]}
  (let [init-provided (contains? expr :init)
        _ (assert init-provided)
        vsym (coerce/var->symbol var)
        warn-if-unannotated? (ns-opts/warn-on-unannotated-vars? (cu/expr-ns expr))
        t (var-env/lookup-Var-nofail vsym)
        check? (var-env/check-var? vsym)]
    (cond
      ; check against an expected type
      (and check? t)
      (let [cinit (when init-provided
                    (check-fn init (r/ret t)))
            _ (when cinit
                (when-not (sub/subtype? (r/ret-t (u/expr-type cinit)) t)
                  (cu/expected-error (r/ret-t (u/expr-type cinit)) t))
                ; now consider this var as checked
                (var-env/add-checked-var-def vsym))]
        (assoc expr
               :init cinit
               u/expr-type (r/ret (c/RClass-of Var [t t]))))

      ; if warn-if-unannotated?, don't try and infer this var,
      ; just skip it
      (or (not check?) 
          (and warn-if-unannotated?
               (not t)))
      (do (println (when-let [line (-> expr :env :line)] 
                     (str line ": ")) 
                   "Not checking" vsym "definition")
          (flush)
          (assoc expr
                 u/expr-type (r/ret (c/RClass-of Var [(or t r/-nothing) (or t r/-any)]))))

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
        (assoc expr
               :init cinit
               u/expr-type (r/ret (c/RClass-of Var [inferred inferred])))))))
