(ns clojure.core.typed.check.if
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.update :as update]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.check-below :as below]))

(defn update-lex+reachable [fs]
  (let [reachable (atom true :validator con/boolean?)
        env (update/env+ (lex/lexical-env) [fs] reachable)]
    [env @reachable]))

(defn combine-rets [{fs+ :then fs- :else :as tst-ret}
                    {ts :t fs2 :fl os2 :o flow2 :flow :as then-ret}
                    env-thn
                    {us :t fs3 :fl os3 :o flow3 :flow :as else-ret}
                    env-els]
  (let [then-reachable? (not= r/-nothing ts)
        else-reachable? (not= r/-nothing us)
        type (c/Un ts us)
        filter (let [{f2+ :then f2- :else} fs2
                     {f3+ :then f3- :else} fs3
                     new-thn-props (:props env-thn)
                     new-els-props (:props env-els)
                     ; +ve test, +ve then
                     +t+t (if then-reachable?
                            (apply fo/-and fs+ f2+ new-thn-props)
                            fl/-bot)
                     ; +ve test, -ve then
                     +t-t (if then-reachable?
                            (apply fo/-and fs+ f2- new-thn-props)
                            fl/-bot)
                     ; -ve test, +ve else
                     -t+e (if else-reachable?
                            (apply fo/-and fs- f3+ new-els-props)
                            fl/-bot)
                     ; -ve test, -ve else
                     -t-e (if else-reachable?
                            (apply fo/-and fs- f3- new-els-props)
                            fl/-bot)

                     final-thn-prop (fo/-or +t+t -t+e)
                     final-els-prop (fo/-or +t-t -t-e)
                     fs (fo/-FS final-thn-prop final-els-prop)]
                 fs)
        object (cond
                 (not then-reachable?) os3
                 (not else-reachable?) os2
                 :else (if (= os2 os3) os2 obj/-empty))
        flow (cond
               (not then-reachable?) flow3
               (not else-reachable?) flow2
               :else
               (r/-flow (fo/-or (:normal flow2)
                                (:normal flow3))))
        ]
    (r/ret type filter object flow)))

(defn unreachable-ret []
  (r/ret r/-nothing
         (fo/-unreachable-filter)
         obj/-empty
         (r/-flow fl/-bot)))

(defn check-if-reachable [check-fn expr lex-env reachable? expected]
  (if (not reachable?)
    (assoc expr 
           u/expr-type (unreachable-ret))
    (binding [vs/*current-expr* expr]
      (var-env/with-lexical-env lex-env
        (check-fn expr expected)))))

;[TCResult Expr Expr (Option Type) -> TCResult]
(defn check-if [check-fn expr ctest thn els expected]
  {:pre [(-> ctest u/expr-type r/TCResult?)
         ((some-fn r/TCResult? nil?) expected)]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [tst (u/expr-type ctest)
        {fs+ :then fs- :else :as tst-f} (r/ret-f tst)

        [env-thn reachable+] (update-lex+reachable fs+)
        [env-els reachable-] (update-lex+reachable fs-)

        cthen (check-if-reachable check-fn thn env-thn reachable+ expected)
        then-ret (u/expr-type cthen)

        celse (check-if-reachable check-fn els env-els reachable- expected)
        else-ret (u/expr-type celse)]
    (let [if-ret (combine-rets tst-f then-ret env-thn else-ret env-els)]
      (assoc expr
             :test ctest
             :then cthen
             :else celse
             ;; already called `check-below` down each branch
             u/expr-type if-ret))))
