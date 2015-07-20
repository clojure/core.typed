(ns clojure.core.typed.check.case
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.update :as update]
            [clojure.core.typed.tc-equiv :as equiv]))

(defn check-case-thens [check-fn target-ret tst-rets case-thens expected]
  {:pre [(r/TCResult? target-ret)
         (every? r/TCResult? tst-rets)
         (== (count tst-rets)
             (count case-thens))]
   :post [((every-pred vector?
                       (con/every-c? (comp #{:case-then} :op)))
           %)]}
  (letfn [(check-case-then [tst-ret {:keys [then] :as case-then}]
            (let [{{fs+ :then} :fl :as rslt} (equiv/tc-equiv := [target-ret tst-ret] nil)
                  flag+ (atom true)
                  env-thn (update/env+ (lex/lexical-env) [fs+] flag+)
                  _ (when-not @flag+
                      ;; FIXME should we ignore this branch?
                      (u/tc-warning "Local became bottom when checking case then"))
                  cthen (var-env/with-lexical-env env-thn
                          (check-fn then expected))]
              (assoc case-then
                     :then cthen
                     u/expr-type (u/expr-type cthen))))]
    (mapv check-case-then
      tst-rets 
      case-thens)))
