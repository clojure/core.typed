;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.special.ann-form
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
            [clojure.core.typed.checker.filter-rep :as fl]))

(defn ann-form-annotation
  "Return the raw type annotation from the ann-form expression."
  [{:keys [statements] :as expr}]
  (let [[_ _ texpr] statements
        tsyns-quoted (ast-u/map-expr-at texpr :type)
        _ (assert (and (seq? tsyns-quoted)
                       (= 'quote (first tsyns-quoted)))
                  (pr-str tsyns-quoted))]
    ;; always quoted
    (second tsyns-quoted)))

(defn parse-annotation
  "Parse the raw type annotation tsyn in the context of expr"
  [tsyn {:keys [env] :as expr}]
  (let [parsed-t (binding [vs/*current-env* env
                           prs/*parse-type-in-ns* (cu/expr-ns expr)]
                   (prs/parse-type tsyn))]
    parsed-t))

(defn check-ann-form
  "Type check an ann-form expression. Propagates its annotation
  inwards to the inner expression."
  [check {:keys [statements env] frm :ret :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (let [expr (-> expr
                 (update :statements #(mapv check %)))
        tsyn (ann-form-annotation expr)
        parsed-t (parse-annotation tsyn expr)
        ;; TODO let users add expected filters etc
        this-expected (or (some-> expected (assoc :t parsed-t))
                          (r/ret parsed-t))
        _ (binding [vs/*current-expr* expr
                    vs/*current-env* env]
            (below/maybe-check-below
              this-expected
              expected))
        cret (check frm this-expected)]
    (assoc expr
           :ret cret
           u/expr-type (u/expr-type cret))))

(defn add-checks-ann-form
  "Add runtime checks to an ann-form expression. Propagates its annotation
  inwards to the inner expression."
  [check {:keys [statements env] frm :ret :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (let [tsyn (ann-form-annotation expr)
        parsed-t (parse-annotation tsyn expr)
        cret (check frm (r/ret parsed-t))]
    ;; don't think we need to propagate types back up
    (assoc expr
           :ret cret)))
