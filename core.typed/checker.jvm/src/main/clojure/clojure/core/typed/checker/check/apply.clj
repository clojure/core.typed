;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.apply
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.string :as str]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.cs-gen :as cgen]
            [clojure.core.typed.checker.subst :as subst])
  (:import (clojure.lang Seqable)))

; we should be able to remove check-apply completely, but we should also instantiate all poly function in test case
(defn maybe-check-apply
  [check-fn -invoke-apply {[fexpr & args] :args :as expr} expected]
  {:post [((some-fn nil? (comp r/TCResult? u/expr-type)) %)]}
  (or (-invoke-apply expr expected)
      (binding [vs/*current-expr* expr]
        (let [cfexpr (check-fn fexpr)
              ftype (r/ret-t (u/expr-type cfexpr))
              [fixed-args tail] [(butlast args) (last args)]]
          (cond
            ;; apply of a simple polymorphic function
            (r/Poly? ftype)
            (let [vars (c/Poly-fresh-symbols* ftype)
                  bbnds (c/Poly-bbnds* vars ftype)
                  body (c/Poly-body* vars ftype)
                  _ (assert (r/FnIntersection? body))
                  fixed-args (mapv check-fn fixed-args)
                  arg-tys (mapv (comp r/ret-t u/expr-type) fixed-args)
                  ctail (check-fn tail)
                  tail-ty (r/ret-t (u/expr-type ctail))
                  expr (assoc expr
                              :args (vec (cons cfexpr (conj fixed-args ctail))))]
              (loop [[{:keys [dom rng rest drest prest] :as ftype0} :as fs] (:types body)]
                (cond
                  (empty? fs) (err/tc-delayed-error (str "Bad arguments to polymorphic function in apply")
                                                    :return (assoc expr
                                                                   u/expr-type (cu/error-ret expected)))

                  prest nil

                  ;the actual work, when we have a * function and a list final argument
                  :else 
                  (if-let [substitution (cgen/handle-failure
                                          (and rest
                                               (<= (count dom)
                                                   (count arg-tys))
                                               (cgen/infer-vararg (zipmap vars bbnds)
                                                                  {}
                                                                  (cons tail-ty arg-tys)
                                                                  (cons (c/Un r/-nil (c/RClass-of Seqable [rest])) dom)
                                                                  rest
                                                                  (r/Result-type* rng)
                                                                  expected)))]
                    (assoc expr
                           u/expr-type (r/ret (subst/subst-all substitution (r/Result-type* rng))))
                    (recur (next fs)))))))))))
