;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.check.let
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.update :as update]
            [clojure.core.typed.checker.check.print-env :as print-env]
            [clojure.core.typed.checker.check.recur-utils :as recur-u]
            [clojure.core.typed.checker.subst-obj :as subst-obj]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.contract-utils :as con]))

(defn update-env [env sym {:keys [t fl flow o] :as r} is-reachable]
  {:pre [(lex/PropEnv? env)
         (symbol? sym)
         (r/TCResult? r)
         (instance? clojure.lang.IAtom is-reachable)]
   :post [(lex/PropEnv? %)]}
  (let [{:keys [then else]} fl
        p* (cond
             ;; init has object `o`.
             ;; we don't need any new info -- aliasing and the
             ;; lexical environment will have the needed info.
             (obj/Path? o) []

             ;; FIXME can we push this optimisation further into
             ;; the machinery? like, check-below?
             (not (c/overlap t r/-falsy)) [then]

             ;; TODO (c/overlap t (NOT r/-falsy)) case,
             ;; which requires thorough testing of Not + (overlap, In, Un)

             ;; init does not have an object so remember new binding `sym`
             ;; in our propositions
             :else [(fo/-or (fo/-and (fo/-not-filter r/-falsy sym)
                                     then)
                            (fo/-and (fo/-filter r/-falsy sym) 
                                     else))])
        new-env (-> env
                    ;update binding type
                    (lex/extend-env sym t o)
                    (update/env+ (cons (:normal flow) p*) is-reachable))]
    new-env))

;now we return a result to the enclosing scope, so we
;erase references to any bindings this scope introduces
;FIXME is this needed in the presence of hygiene?
(defn erase-objects [syms ret]
  {:pre [(every? symbol? syms)
         (r/TCResult? ret)]
   :post [(r/TCResult? %)]}
  (reduce (fn [ty sym]
            {:pre [(r/TCResult? ty)
                   (symbol? sym)]}
            (-> ty
                (update :t subst-obj/subst-type sym obj/-empty true)
                (update :fl subst-obj/subst-filter-set sym obj/-empty true)
                (update :o subst-obj/subst-object sym obj/-empty true)
                (update-in [:flow :normal] subst-obj/subst-filter sym obj/-empty true)))
          ret
          syms))

(defn check-let [check {:keys [body bindings] :as expr} expected & [{is-loop :loop? :keys [expected-bnds]}]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (cond
    (and is-loop (seq bindings) (not expected-bnds))
    (do
      (err/tc-delayed-error "Loop requires more annotations")
      (assoc expr
             u/expr-type (or expected (r/ret (c/Un)))))
    :else
    (let [is-reachable (atom true)
          [env cbindings] 
          (reduce
            (fn [[env cbindings] [n expected-bnd]]
              {:pre [@is-reachable
                     (lex/PropEnv? env)
                     ((some-fn nil? r/Type?) expected-bnd)
                     (= (boolean expected-bnd) (boolean is-loop))]
               :post [((con/maybe-reduced-c? (con/hvector-c? lex/PropEnv? vector?)) %)]}
              (let [expr (get cbindings n)
                    ; check rhs
                    {sym :name :as cexpr} (var-env/with-lexical-env env
                                            (check expr (when is-loop
                                                          (r/ret expected-bnd))))
                    new-env (update-env env sym (u/expr-type cexpr) is-reachable)
                    maybe-reduced (if @is-reachable identity reduced)]
                (maybe-reduced
                  [new-env (assoc cbindings n cexpr)])))
            [(lex/lexical-env) bindings]
            (map vector
                 (range (count bindings))
                 (or expected-bnds
                     (repeat nil))))
          _ (assert (= (count bindings) (count cbindings)))]
      (cond
        (not @is-reachable) (assoc expr 
                                   :bindings cbindings
                                   u/expr-type (or expected (r/ret (c/Un))))

        :else
        (let [cbody (var-env/with-lexical-env env
                      (if is-loop
                        (binding [recur-u/*recur-target* (recur-u/RecurTarget-maker expected-bnds nil nil nil)]
                          (check body expected))
                        (binding [vs/*current-expr* body]
                          (check body expected))))
             unshadowed-ret (erase-objects (map :name cbindings) (u/expr-type cbody))]
          (assoc expr
                 :body cbody
                 :bindings cbindings
                 u/expr-type unshadowed-ret))))))

