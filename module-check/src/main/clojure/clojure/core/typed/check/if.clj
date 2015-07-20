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

;[TCResult Expr Expr (Option Type) -> TCResult]
(defn check-if [check-fn expr ctest thn els expected]
  {:pre [(-> ctest u/expr-type r/TCResult?)
         ((some-fn r/TCResult? nil?) expected)]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [check-if-checkfn (if-let [c check-fn]
                           c
                           (err/int-error "No checkfn passed for if"))]
    (letfn [(tc [expr reachable?]
              {:post [(-> % u/expr-type r/TCResult?)]}
              (when-not reachable?
                #_(prn "Unreachable code found.. " expr))
              (cond
                ;; if reachable? is #f, then we don't want to verify that this branch has the appropriate type
                ;; in particular, it might be (void)
                (and expected reachable?)
                (check-if-checkfn expr (-> expected
                                                 (assoc :fl (fo/-FS fl/-top fl/-top))
                                                 (assoc :o obj/-empty)
                                                 (assoc :flow (r/-flow fl/-top))))
                ;; this code is reachable, but we have no expected type
                reachable? (check-if-checkfn expr)
                ;; otherwise, this code is unreachable
                ;; and the resulting type should be the empty type
                :else (do ;(prn "Not checking unreachable code")
                          (assoc expr 
                                 u/expr-type (r/ret (c/Un))))))]
      (let [tst (u/expr-type ctest)
            {fs+ :then fs- :else :as f1} (r/ret-f tst)
            ;          _ (prn "check-if: fs+" fs+)
            ;          _ (prn "check-if: fs-" fs-)
            flag+ (atom true :validator con/boolean?)
            flag- (atom true :validator con/boolean?)

            ;_ (print-env)
            ;idsym (gensym)
            env-thn (update/env+ (lex/lexical-env) [fs+] flag+)
            ;          _ (do (pr "check-if: env-thn")
            ;              (print-env* env-thn))
            env-els (update/env+ (lex/lexical-env) [fs-] flag-)
            ;          _ (do (pr "check-if: env-els")
            ;              (print-env* env-els))
            ;          new-thn-props (set
            ;                          (filter atomic-filter?
            ;                                  (set/difference
            ;                                    (set (:props (lex/lexical-env)))
            ;                                    (set (:props env-thn)))))
            ;_ (prn idsym"env+: new-thn-props" (map unparse-filter new-thn-props))
            ;          new-els-props (set
            ;                          (filter atomic-filter?
            ;                                  (set/difference
            ;                                    (set (:props (lex/lexical-env)))
            ;                                    (set (:props env-els)))))
            ;_ (prn idsym"env+: new-els-props" (map unparse-filter new-els-props))
            cthen
            (binding [vs/*current-expr* thn]
              (var-env/with-lexical-env env-thn
                (tc thn @flag+)))

            {ts :t fs2 :fl os2 :o flow2 :flow :as then-ret}
            (u/expr-type cthen)

            celse
            (binding [vs/*current-expr* els]
              (var-env/with-lexical-env env-els
                (tc els @flag-)))

            {us :t fs3 :fl os3 :o flow3 :flow :as else-ret} 
            (u/expr-type celse)

            ;_ (prn "flow2" (prs/unparse-flow-set flow2))
            ;_ (prn "flow3" (prs/unparse-flow-set flow3))
            ]

        ;some optimization code here, contraditions etc? omitted

        ;      (prn "check-if: then branch:" (prs/unparse-TCResult then-ret))
        ;      (prn "check-if: else branch:" (prs/unparse-TCResult else-ret))
        (let [if-ret 
              (cond
                ;both branches reachable
                (and (not (= (c/Un) ts))
                     (not (= (c/Un) us)))
                (let [;_ (prn "both branches reachable")
                      r (let [filter (cond
                                       (or (fl/NoFilter? fs2)
                                           (fl/NoFilter? fs3)) (fo/-FS fl/-top fl/-top)
                                       (and (fl/FilterSet? fs2)
                                            (fl/FilterSet? fs3))
                                       (let [{f2+ :then f2- :else} fs2
                                             {f3+ :then f3- :else} fs3
                                             ; +ve test, +ve then
                                             new-thn-props (:props env-thn)
                                             ;_ (prn "new-thn-props" (map prs/unparse-filter new-thn-props))
                                             new-els-props (:props env-els)
                                             ;_ (prn "new-els-props" (map prs/unparse-filter new-els-props))
                                             +t+t (apply fo/-and fs+ f2+ new-thn-props)
                                             ;_ (prn "+t+t" (prs/unparse-filter +t+t))
                                             ; -ve test, +ve else
                                             -t+e (apply fo/-and fs- f3+ new-els-props)
                                             ;_ (prn "-t+e" (prs/unparse-filter -t+e))
                                             ; +ve test, -ve then
                                             +t-t (apply fo/-and fs+ f2- new-thn-props)
                                             ;_ (prn "+t-t" (prs/unparse-filter +t-t))
                                             ; -ve test, -ve else
                                             -t-e (apply fo/-and fs- f3- new-els-props)
                                             ;_ (prn "-t-e" (prs/unparse-filter -t-e))

                                             final-thn-prop (fo/-or +t+t -t+e)
                                             ;_ (prn "final-thn-prop" (prs/unparse-filter final-thn-prop))
                                             final-els-prop (fo/-or +t-t -t-e)
                                             ;_ (prn "final-els-prop" (prs/unparse-filter final-els-prop))
                                             fs (fo/-FS final-thn-prop final-els-prop)]
                                         fs)
                                       :else (err/int-error (str "What are these?" fs2 fs3)))
                              type (c/Un ts us)
                              object (if (= os2 os3) os2 (obj/EmptyObject-maker))

                              ;only bother with something interesting if a branch is unreachable (the next two cond cases)
                              ;Should be enough for `assert`
                              ;flow (r/-flow (fo/-or flow2 flow3))
                              flow (r/-flow fl/-top)
                              ]
                          (r/ret type filter object flow))]
                  ;(prn "check if:" "both branches reachable, with combined result" (prs/unparse-TCResult r))
                  (if expected (below/check-below r expected) r))
                ;; both branches unreachable, flow-set is ff
                (and (= us (c/Un))
                     (= ts (c/Un)))
                (r/ret (c/Un) (fo/-FS fl/-top fl/-top) obj/-empty (r/-flow fl/-bot))
                ;; special case if one of the branches is unreachable
                (= us (c/Un))
                (if expected (below/check-below (r/ret ts fs2 os2 flow2) expected) (r/ret ts fs2 os2 flow2))
                (= ts (c/Un))
                (if expected (below/check-below (r/ret us fs3 os3 flow3) expected) (r/ret us fs3 os3 flow3))
                :else (err/int-error "Something happened"))
              _ (assert (r/TCResult? if-ret))]
          (assoc expr
                 :test ctest
                 :then cthen
                 :else celse
                 u/expr-type if-ret))))))
