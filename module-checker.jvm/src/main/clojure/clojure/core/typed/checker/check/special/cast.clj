;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.special.cast
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.checker.filter-rep :as fl]))

(defn check-cast
  [check expr expected]
  (let [{[_ _ texpr :as statements] :statements :keys [env] frm :ret :as expr}
        (-> expr 
            ; don't need to check these statements because it's just metadata
            ; embedded in the expression by the `t/cast` macro
            (update :statements #(mapv ana2/run-passes %))
            ; but we do want to check (a subset) of this, so just run pre-passes
            (update :ret (comp ana2/run-pre-passes ana2/analyze-outer-root)))
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
        _ (assert (= 1 (count (:args frm))))
        ;; allows silly down casts, might want to change that.
        expr (-> expr
                 ; just need to traverse :fn using the analyzer
                 (update-in [:ret :fn] ana2/run-passes)
                 ; check the expression being cast
                 (update-in [:ret :args 0] check)
                 ; top-level could be propagated here since this is a :do form,
                 ; so call eval-top-level
                 (update :ret (comp ana2/eval-top-level ana2/run-post-passes)))]
    (assoc expr
           u/expr-type (r/ret parsed-t))))
