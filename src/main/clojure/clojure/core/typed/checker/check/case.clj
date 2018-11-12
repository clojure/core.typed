;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.case
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.update :as update]
            [clojure.core.typed.checker.jvm.tc-equiv :as equiv]))

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
