;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.do
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.object-rep :as orep]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.update :as update]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.contract-utils :as con]))

(defn internal-form? [expr]
  (u/internal-form? expr spec/special-form))

(defn enforce-do-folding [{:keys [statements] :as expr} kw]
  (when-not (#{0 1} (count 
                      (filter #{kw}
                              (map :val statements))))
    (err/int-error (str "Folded special-forms detected " (ast-u/emit-form-fn expr)))))

(defn check-do [check internal-special-form expr expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:statements %))]}
  (enforce-do-folding expr spec/special-form)
  (cond
    (internal-form? expr)
    (internal-special-form expr expected)

    :else
    (let [exprs (conj (vec (:statements expr)) (:ret expr))
          nexprs (count exprs)
          reachable-atom (atom true)
          [env cexprs]
          (reduce (fn [[env cexprs] ^long n]
                    {:pre [(lex/PropEnv? env)
                           (integer? n)
                           (< n nexprs)]
                     ; :post checked after the reduce
                     }
                    (cond
                      (not @reachable-atom) [env (assoc-in cexprs [n u/expr-type]
                                                      (r/ret (r/Bottom)
                                                             (fo/-unreachable-filter)
                                                             orep/-empty
                                                             (r/-flow fl/-bot)))]
                      :else
                      (let [expr (get cexprs n)
                            _ (assert (map? expr))
                            cexpr (binding [; always prefer envs with :line information, even if inaccurate
                                            vs/*current-env* (if (:line (:env expr))
                                                               (:env expr)
                                                               vs/*current-env*)
                                            vs/*current-expr* expr]
                                    (var-env/with-lexical-env env
                                      (check expr
                                             ;propagate expected type only to final expression
                                             (when (== (inc n) nexprs)
                                               expected))))
                            res (u/expr-type cexpr)
                            ;_ (prn "cexpr in do" res)
                            flow (-> res r/ret-flow r/flow-normal)
                            ;_ (prn flow)
                            ;add normal flow filter
                            nenv (if (fl/NoFilter? flow)
                                   env
                                   (update/env+ env [flow] reachable-atom))
                            _ (u/trace-when-let
                                [ls (seq (cu/find-updated-locals (:l env) (:l nenv)))]
                                (str "Updated local in exceptional control flow (do): " ls))
                            ;_ (prn nenv)
                            ]
                        (if @reachable-atom
                          ;reachable
                          [nenv (assoc cexprs n cexpr)]
                          ;unreachable
                          (do ;(prn "Detected unreachable code")
                              [nenv (assoc cexprs n
                                           (assoc cexpr 
                                                  u/expr-type (r/ret (r/Bottom)
                                                                     (fo/-unreachable-filter)
                                                                     orep/-empty
                                                                     (r/-flow fl/-bot))))])))))
                  [(lex/lexical-env) exprs] (range nexprs))
          _ (assert (= (count cexprs) nexprs))
          actual-types (mapv u/expr-type cexprs)
          _ (assert (lex/PropEnv? env))
          _ (assert ((every-pred vector? seq) cexprs)) ; make sure we conj'ed in the right order
          _ (assert ((every-pred (con/every-c? r/TCResult?) seq) actual-types)
                    actual-types)]
      ;(prn "do actual-types" actual-types)
      (assoc expr
             :statements (pop cexprs)
             :ret (peek cexprs)
             u/expr-type (peek actual-types))))) ;should be a r/ret already
