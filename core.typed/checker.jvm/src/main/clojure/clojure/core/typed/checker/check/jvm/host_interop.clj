;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.jvm.host-interop
  (:require [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.analyzer.jvm.passes.analyze-host-expr :as ana-host]
            [clojure.core.typed.analyzer.jvm.passes.validate :as validate]
            [clojure.core.typed.checker.check.jvm.field :as field]
            [clojure.core.typed.checker.check.jvm.method :as method]
            [clojure.core.typed.checker.check.jvm.type-hints :as type-hints]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.errors :as err]))

(defn try-resolve-reflection [ast]
  (-> ast
      ana-host/analyze-host-expr
      validate/validate))

;; from clojure.tools.analyzer.utils
(defn- obj?
  "Returns true if x implements IObj"
  [x]
  (instance? clojure.lang.IObj x))

;; from clojure.core.typed.analyzer.jvm.passes.emit-form
(defn- class->sym [class]
  (if (symbol? class)
    class
    (symbol (.getName ^Class class))))

(defn add-type-hints
  "Add type hints to an expression, only if it can
  be preserved via emit-form to the Clojure compiler.

  The most reliable AST node to convey a type hint
  is :local, so we restrict adding type hints to only
  :local nodes."
  [expr]
  (let [{:keys [t]} (u/expr-type expr)
        cls (cu/Type->Class t)]
    (if (and cls
             (#{:local} (:op expr)))
      (-> expr
          (assoc :o-tag cls
                 :tag cls)
          (update :form
                  (fn [f]
                    (if (obj? f)
                      (vary-meta f assoc :tag (class->sym cls))
                      f))))
      expr)))

(defn check-host-interop
  [check-expr expr expected]
  {:pre [(#{:host-interop :host-call :host-field} (:op expr))
         (#{:unanalyzed} (:op (:target expr)))
         (every? (comp #{:unanalyzed} :op) (:args expr))]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [expr (-> expr
                 (update :target check-expr)
                 (update :args #(mapv check-expr %))
                 ana2/run-post-passes)
        expr (cond-> expr
               (and (#{:host-interop} (:op expr))
                    (cu/should-rewrite?))
               (-> (update :target add-type-hints)
                   (update :args #(mapv add-type-hints %))
                   try-resolve-reflection))]
    (case (:op expr)
      (:instance-call :static-call) (method/check-invoke-method expr expected)
      :static-field (field/check-static-field expr expected)
      :instance-field (field/check-instance-field expr expected)
      (:host-interop :host-field :host-call)
      (let [m-or-f-kw (case (:op expr)
                        :host-interop :m-or-f
                        :host-field :field
                        :host-call :method)]
        (err/tc-delayed-error
          (str "Unresolved host interop: " (m-or-f-kw expr)
               (type-hints/suggest-type-hints 
                 (m-or-f-kw expr)
                 (-> expr :target u/expr-type r/ret-t) 
                 (map (comp r/ret-t u/expr-type) (:args expr)))
               "\n\nHint: use *warn-on-reflection* to identify reflective calls")
          :return (assoc expr 
                         u/expr-type (cu/error-ret expected)))))))

(defn check-host-call
  [check-expr -host-call-special expr expected]
  {:pre [(#{:host-call} (-> expr :op))
         (#{:unanalyzed} (-> expr :target :op))
         (every? (comp #{:unanalyzed} :op) (:args expr))]
   :post [(-> % u/expr-type r/TCResult?)
          (#{:static-call :instance-call} (:op %))]}
  (or (when-some [expr (-host-call-special expr expected)]
        (let [expr (-> expr
                       ana2/run-post-passes
                       (merge (select-keys expr [u/expr-type])))]
          (case (:op expr)
            (:static-call :instance-call) expr
            :host-call
            (err/tc-delayed-error
              (str "Unresolved method call: " (:method expr)
                   (type-hints/suggest-type-hints
                     (:method expr)
                     (-> expr :target u/expr-type r/ret-t) 
                     (->> expr :args (map (comp r/ret-t u/expr-type))))
                   "\n\nHint: use *warn-on-reflection* to identify reflective calls")
              :return expr))))
      (check-host-interop check-expr expr expected)))

(defn check-maybe-host-form
  [check-expr expr expected]
  {:pre [(#{:maybe-host-form} (:op expr))]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [expr (ana2/run-pre-passes expr)]
    (if (#{:maybe-host-form} (:op expr))
      (err/tc-delayed-error (str "Unresolved host interop: " (:form expr)
                                 "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            :return (assoc expr u/expr-type r/-error))
      (check-expr expr expected))))
