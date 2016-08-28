(ns clojure.core.typed.check.do
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.object-rep :as orep]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.update :as update]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.contract-utils :as con]))

(defn internal-form? [expr]
  (u/internal-form? expr spec/special-form))

(defn check-do [check internal-special-form expr expected]
  (u/enforce-do-folding expr spec/special-form)
  (cond
    (internal-form? expr)
    (internal-special-form expr expected)

    :else
    (let [exprs (vec (concat (:statements expr) [(:ret expr)]))
          nexprs (count exprs)
          [env cexprs]
          (reduce (fn [[env cexprs] [^long n expr]]
                    {:pre [(lex/PropEnv? env)
                           (integer? n)
                           (< n nexprs)]
                     ; :post checked after the reduce
                     }
                    (let [cexpr (binding [; always prefer envs with :line information, even if inaccurate
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
                          flow-atom (atom true)
                          ;_ (prn flow)
                          ;add normal flow filter
                          nenv (if (fl/NoFilter? flow)
                                 env
                                 (update/env+ env [flow] flow-atom))
                          _ (u/trace-when-let
                              [ls (seq (cu/find-updated-locals (:l env) (:l nenv)))]
                              (p/p :check.do/updated-exceptional-control-flow)
                              (str "Updated local in exceptional control flow (do): " ls))
                          ;_ (prn nenv)
                          ]
                      (if @flow-atom
                        ;reachable
                        [nenv (conj cexprs cexpr)]
                        ;unreachable
                        (do ;(prn "Detected unreachable code")
                          (reduced [nenv (conj cexprs 
                                               (assoc cexpr 
                                                      u/expr-type (r/ret (r/Bottom)
                                                                         (fo/-unreachable-filter)
                                                                         orep/-empty
                                                                         (r/-flow fl/-bot))))])))))
                  [(lex/lexical-env) []] (map-indexed vector exprs))
          actual-types (mapv u/expr-type cexprs)
          _ (assert (lex/PropEnv? env))
          _ (assert ((every-pred vector? seq) cexprs)) ; make sure we conj'ed in the right order
          _ (assert ((every-pred (con/every-c? r/TCResult?) seq) actual-types))]
      ;(prn "do actual-types" actual-types)
      (assoc expr
             :statements (ast-u/do-statements-value cexprs)
             :ret (peek cexprs)
             u/expr-type (peek actual-types))))) ;should be a r/ret already
