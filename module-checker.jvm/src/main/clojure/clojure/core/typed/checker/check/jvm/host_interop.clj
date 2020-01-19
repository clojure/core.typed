;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.jvm.host-interop
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.tools.analyzer.passes.jvm.validate :as validate]
            [clojure.core.typed.analyzer.jvm.passes.analyze-host-expr :as ana-host]
            [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.check.jvm.type-hints :as type-hints]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.jvm.method :as method]))

(defn try-resolve-reflection [ast]
  (-> ast
      ana-host/analyze-host-expr
      validate/validate))

;; from clojure.tools.analyzer.utils
(defn- obj?
  "Returns true if x implements IObj"
  [x]
  (instance? clojure.lang.IObj x))

;; from clojure.tools.analyzer.passes.jvm.emit-form
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

;FIXME some spaghetti logic/shadowing in this code, maybe do explicit `case`
; on the various :host-* forms. Remember, some use :target, others :instance.
(defn check-host-interop
  [check-expr {original-op :op :as expr} expected & {:keys [no-check]}]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [check-expr (if no-check
                     (fn [ast & _] #_(ana2/run-passes ast) ast)
                     check-expr)
        ;_ (prn (->> expr target :env :locals
        ;            (map (fn [[k v]]
        ;                   [k (select-keys v [:op :tag :form])]))))
        ;_ (prn "old keys" original-op (vec (keys expr)))
        {ctarget :target cargs :args :keys [m-or-f] :as expr}
        (-> expr
            (update :target check-expr)
            (update :args #(mapv check-expr %))
            ana2/run-post-passes)
        ;_ (prn "new op" (:op expr) (vec (keys expr)))
        give-up (fn []
                  (do
                    (err/tc-delayed-error (str "Unresolved host interop: " (or m-or-f (:method expr))
                                               (type-hints/suggest-type-hints 
                                                 (or m-or-f (:method expr))
                                                 (-> ctarget u/expr-type r/ret-t) 
                                                 [])
                                               "\n\nHint: use *warn-on-reflection* to identify reflective calls"))
                    (assoc expr 
                           u/expr-type (cu/error-ret expected))))]
    ;; try to rewrite, otherwise error on reflection
    (cond
      (not= original-op (:op expr)) (check-expr expr expected)

      (cu/should-rewrite?)
      (let [ctarget (add-type-hints ctarget)
            cargs (mapv add-type-hints cargs)
            nexpr (assoc expr 
                         :target ctarget
                         :args cargs)
            ;_ (prn "host interop" (-> nexpr :target ((juxt :o-tag :tag))))
            rewrite (try-resolve-reflection nexpr)]
        (case (:op rewrite)
          (:static-call :instance-call)
          (if no-check
            rewrite
            (let [e (method/check-invoke-method check-expr rewrite expected
                                                :ctarget ctarget
                                                :cargs cargs)]
              e))
          ;; TODO field cases
          (give-up)))
      :else (give-up))))

(defn check-host-call
  [check-expr -host-call-special expr expected]
  (let [; should this be a type check instead? -host-call-special basically uses
        ; :target's :tag information to help check :args
        expr (update expr :target ana2/run-passes)
        maybe-checked-expr (-host-call-special expr expected)]
    (if (= :default maybe-checked-expr)
      (check-host-interop check-expr expr expected)
      (let [expr maybe-checked-expr]
        ;(assert (:post-done (:target expr)))
        ;(assert (every? :post-done (:args expr)))
        (check-host-interop check-expr maybe-checked-expr expected :no-check true)))))

(defn check-maybe-host-form
  [check-expr expr expected]
  (let [expr (ana2/run-pre-passes expr)]
    (if (= :maybe-host-form (:op expr))
      (err/tc-delayed-error (str "Unresolved host interop: " (:form expr)
                                 "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            :return (assoc expr u/expr-type r/-error))
      (check-expr expr expected))))
