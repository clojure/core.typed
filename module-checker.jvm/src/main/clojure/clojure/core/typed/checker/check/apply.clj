;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.apply
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.string :as str]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.cs-gen :as cgen]
            [clojure.core.typed.checker.subst :as subst])
  (:import (clojure.lang Seqable)))

; we should be able to remove check-apply completely, but we should also instantiate all poly function in test case
(defn check-apply
  [check-fn {[fexpr & args] :args :as expr} expected]
  {:post [((some-fn r/TCResult? #{cu/not-special}) %)]}
  (binding [vs/*current-expr* expr]
    (let [ftype (r/ret-t (u/expr-type (check-fn fexpr)))
          [fixed-args tail] [(butlast args) (last args)]]
      (cond
        ;apply of a simple function
        (r/FnIntersection? ftype)
        cu/not-special
        #_(do
          (when (empty? (:types ftype))
            (err/int-error (str "Empty function intersection given as argument to apply")))
          (let [arg-tres (mapv check-fn fixed-args)
                arg-tys (mapv (comp r/ret-t u/expr-type) arg-tres)
                tail-ty (r/ret-t (u/expr-type (check-fn tail)))]
            (loop [[{:keys [dom rng rest drest prest]} :as fs] (:types ftype)]
              (cond
                ;we've run out of cases to try, so error out
                (empty? fs)
                (err/tc-delayed-error 
                  (prs/with-unparse-ns (cu/expr-ns expr)
                    (str "Bad arguments to apply: "
                         "\n\nTarget: \t" (prs/unparse-type ftype) 
                         "\n\nArguments:\t" (str/join " " (mapv prs/unparse-type (concat arg-tys [tail-ty])))))
                  :return (cu/error-ret expected))

                ;this case of the function type has a rest argument
                (and rest
                     ;; check that the tail expression is a subtype of the rest argument
                     (sub/subtype? tail-ty (c/Un r/-nil (c/RClass-of Seqable [rest])))
                     (sub/subtypes-varargs? arg-tys dom rest nil))
                (r/ret (r/Result-type* rng)
                       (r/Result-filter* rng)
                       (r/Result-object* rng))

                ;other cases go here

                ;next case
                :else (recur (next fs))))))

        ;; apply of a simple polymorphic function
        (r/Poly? ftype)
        (let [vars (c/Poly-fresh-symbols* ftype)
              bbnds (c/Poly-bbnds* vars ftype)
              body (c/Poly-body* vars ftype)
              _ (assert (r/FnIntersection? body))
              arg-tres (mapv check-fn fixed-args)
              arg-tys (mapv (comp r/ret-t u/expr-type) arg-tres)
              tail-bound nil
              tail-ty (r/ret-t (u/expr-type (check-fn tail)))]
          (loop [[{:keys [dom rng rest drest prest] :as ftype0} :as fs] (:types body)]
            ;          (when (seq fs)
            ;            (prn "checking fn" (prs/unparse-type (first fs))
            ;                 (mapv prs/unparse-type arg-tys)))
            (cond
              (empty? fs) (err/tc-delayed-error (str "Bad arguments to polymorphic function in apply")
                                                :return (cu/error-ret expected))

              prest cu/not-special

              ;the actual work, when we have a * function and a list final argument
              :else 
              (if-let [substitution (cgen/handle-failure
                                      (and rest (not tail-bound) 
                                           (<= (count dom)
                                               (count arg-tys))
                                           (cgen/infer-vararg (zipmap vars bbnds) {}
                                                              (cons tail-ty arg-tys)
                                                              (cons (c/Un r/-nil (c/RClass-of Seqable [rest])) dom)
                                                              rest
                                                              (r/Result-type* rng))))]
                (r/ret (subst/subst-all substitution (r/Result-type* rng)))
                (recur (next fs))))))

        :else cu/not-special))))
