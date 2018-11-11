;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.check.special.cast
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.analyzer :as ana2]
            [clojure.core.typed.checker.filter-rep :as fl]))

(defn check-cast
  [check expr expected]
  (let [{:keys [pre post]} ana2/scheduled-passes
        {[_ _ texpr :as statements] :statements :keys [env] frm :ret :as expr}
        (-> expr 
            (update-in [:statements 2] ana2/run-passes)
            (update :ret pre))
        _ (assert (#{3} (count statements)))
        tsyn-quoted (ast-u/map-expr-at texpr :type)
        _ (impl/impl-case
            :clojure (assert (and (seq? tsyn-quoted)
                                  ('#{quote} (first tsyn-quoted)))
                             tsyn-quoted)
            :cljs nil)
        tsyn (impl/impl-case
               :clojure (second tsyn-quoted)
               :cljs tsyn-quoted)
        parsed-t (binding [vs/*current-env* env
                           prs/*parse-type-in-ns* (cu/expr-ns expr)]
                   ;; unwrap quoted syntax with second
                   (prs/parse-type tsyn))
        ;; frm is of the form ((fn [x] ...) x), we want to type check
        ;; x, but not the lambda.
        _ (assert (= :invoke (:op frm)))
        _ (assert (== 1 (count (:args frm))))
        ;; allows silly down casts, might want to change that.
        expr (-> expr
                 (update-in [:ret :fn] ana2/run-passes)
                 (update-in [:ret :args 0] check)
                 (update :ret post))]
    (assoc expr
           :statements statements
           u/expr-type (r/ret parsed-t))))
