(ns clojure.core.typed.check.let
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.update :as update]
            [clojure.core.typed.check.print-env :as print-env]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.subst-obj :as subst-obj]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.contract-utils :as con]))

(defn check-let [check {:keys [bindings] :as expr} expected & [{is-loop :loop? :keys [expected-bnds]}]]
  (let [_ (assert (contains? expr (ast-u/let-body-kw))
                  (keys expr))
        body ((ast-u/let-body-kw) expr)]
    (u/p :check/check-let
     (cond
       (and is-loop (seq bindings) (not expected-bnds) )
       (do
         (err/tc-delayed-error "Loop requires more annotations")
         (assoc expr
                u/expr-type (r/ret (c/Un))))
       :else
       (let [[env cbindings] 
             (reduce 
               (fn [[env cexprs] [{sym :name :keys [init] :as expr} expected-bnd]]
                 {:pre [(lex/PropEnv? env)
                        init
                        sym
                        ((some-fn nil? r/Type?) expected-bnd)
                        (identical? (boolean expected-bnd) (boolean is-loop))]
                  :post [((con/hvector-c? lex/PropEnv? vector?) %)]}
                 (let [; check rhs
                       cinit (binding [vs/*current-expr* init]
                               (var-env/with-lexical-env env
                                 (check init (when is-loop
                                               (r/ret expected-bnd)))))
                       cexpr (assoc expr
                                    :init cinit
                                    u/expr-type (u/expr-type cinit))
                       {:keys [t fl flow]} (u/expr-type cinit)
                       _ (when (and expected-bnd
                                    (not (sub/subtype? t expected-bnd)))
                           (err/tc-delayed-error 
                             (str "Loop variable " sym " initialised to "
                                  (pr-str (prs/unparse-type t))
                                  ", expected " (pr-str (prs/unparse-type expected-bnd))
                                  "\n\nForm:\n\t" (ast-u/emit-form-fn init))))
                       t (or expected-bnd t)]
                   (cond
                     (fl/FilterSet? fl)
                     (let [{:keys [then else]} fl
                           p* [(fo/-imp (fo/-not-filter (c/Un r/-nil r/-false) sym) then)
                               (fo/-imp (fo/-filter (c/Un r/-nil r/-false) sym) else)]
                           flow-f (r/flow-normal flow)
                           flow-atom (atom true)
                           new-env (-> env
                                       ;update binding type
                                       (assoc-in [:l sym] t)
                                       ;update props
                                       (update-in [:props] #(set 
                                                              (apply concat 
                                                                     (update/combine-props p* % (atom true)))))
                                       (update/env+ [(if (= fl/-bot flow-f) fl/-top flow-f)] flow-atom))
                           _ (when-not @flow-atom 
                               (binding [vs/*current-expr* init]
                                 (err/int-error
                                   (str "Applying flow filter resulted in local being bottom"
                                        "\n"
                                        (with-out-str (print-env/print-env* new-env))
                                        "\nOld: "
                                        (with-out-str (print-env/print-env* env))))))]
                       [new-env (conj cexprs cexpr)])

                     (fl/NoFilter? fl) (do
                                         (assert (= (r/-flow fl/-top) flow))
                                         [(-> env
                                              ;no propositions to add, just update binding type
                                              (assoc-in [:l sym] t))
                                          (conj cexprs cexpr)])
                     :else (err/int-error (str "What is this?" fl)))))
               [lex/*lexical-env* []] (map vector bindings (or expected-bnds
                                                               (repeat nil))))

             cbody (var-env/with-lexical-env env
                     (if is-loop
                       (binding [recur-u/*recur-target* (recur-u/->RecurTarget expected-bnds nil nil nil)]
                         (check body expected))
                       (binding [vs/*current-expr* body]
                         (check body expected))))
             ;now we return a result to the enclosing scope, so we
             ;erase references to any bindings this scope introduces
             unshadowed-ret
             (reduce (fn [ty sym]
                       {:pre [(r/TCResult? ty)
                              (symbol? sym)]}
                       (-> ty
                           (update-in [:t] subst-obj/subst-type sym obj/-empty true)
                           (update-in [:fl] subst-obj/subst-filter-set sym obj/-empty true)
                           (update-in [:o] subst-obj/subst-object sym obj/-empty true)
                           (update-in [:flow :normal] subst-obj/subst-filter sym obj/-empty true)))
                     (u/expr-type cbody)
                     (map :name bindings))]
         (assoc expr
                (ast-u/let-body-kw) cbody
                :bindings cbindings
                u/expr-type unshadowed-ret))))))

