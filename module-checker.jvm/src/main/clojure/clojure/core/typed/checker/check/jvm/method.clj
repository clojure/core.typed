;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.jvm.method
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.check.jvm.type-hints :as type-hints]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.check.funapp :as funapp]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.jvm.method-override-env :as mth-override]))

;[MethodExpr Type Any -> Expr]
(defn check-invoke-method [check-fn {c :class method-name :method :keys [args env] :as expr} expected
                           & {:keys [ctarget cargs method-override]}]
  {:pre [((some-fn nil? r/TCResult?) expected)
         ((some-fn nil? r/Type?) method-override)]
   :post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (let [inst? (= :instance-call (:op expr))
          method (cu/MethodExpr->Method expr)
          msym (cu/MethodExpr->qualsym expr)
          rfin-type (or method-override
                        (when msym
                          (mth-override/get-method-override msym))
                        (when method
                          (cu/Method->Type method)))
          _ (assert ((some-fn nil? r/Type?) rfin-type))
          ctarget (when inst?
                    (assert (:instance expr))
                    (or ctarget (check-fn (:instance expr))))
          cargs (or cargs (mapv check-fn args))]
      (if-not rfin-type
        (err/tc-delayed-error (str "Unresolved " (if inst? "instance" "static") 
                                 " method invocation " method-name "." 
                                 (type-hints/suggest-type-hints 
                                   method-name 
                                   (when ctarget
                                     (-> ctarget u/expr-type r/ret-t))
                                   (map (comp r/ret-t u/expr-type) cargs))
                                 "\n\nHint: use *warn-on-reflection* to identify reflective calls")
                            :form (ast-u/emit-form-fn expr)
                            :return (merge
                                      (assoc expr 
                                             :args cargs
                                             u/expr-type (cu/error-ret expected))
                                      (when ctarget {:instance ctarget})))
        (let [_ (when inst?
                  (let [target-class (resolve (:declaring-class method))
                        _ (assert (class? target-class))]
                    ;                (prn "check target" (prs/unparse-type (r/ret-t (u/expr-type ctarget)))
                    ;                     (prs/unparse-type (c/RClass-of (coerce/Class->symbol (resolve (:declaring-class method))) nil)))
                    (when-not (sub/subtype? (r/ret-t (u/expr-type ctarget)) (c/RClass-of-with-unknown-params target-class))
                      (err/tc-delayed-error (str "Cannot call instance method " (cu/Method->symbol method)
                                               " on type " (pr-str (prs/unparse-type (r/ret-t (u/expr-type ctarget)))))
                                          :form (ast-u/emit-form-fn expr)))))
              result-type (funapp/check-funapp expr args (r/ret rfin-type) (map u/expr-type cargs) expected)
              _ (when expected
                  (when-not (sub/subtype? (r/ret-t result-type) (r/ret-t expected))
                    (err/tc-delayed-error (str "Return type of " (if inst? "instance" "static")
                                             " method " (cu/Method->symbol method)
                                             " is " (prs/unparse-type (r/ret-t result-type))
                                             ", expected " (prs/unparse-type (r/ret-t expected)) "."
                                             (when (sub/subtype? r/-nil (r/ret-t result-type))
                                               (str "\n\nHint: Use `non-nil-return` and `nilable-param` to configure "
                                                    "where `nil` is allowed in a Java method call. `method-type` "
                                                    "prints the current type of a method.")))
                                        :form (ast-u/emit-form-fn expr))))]
          (merge
            (assoc expr
                   :args cargs
                   u/expr-type result-type)
            (when ctarget {:instance ctarget})))))))
