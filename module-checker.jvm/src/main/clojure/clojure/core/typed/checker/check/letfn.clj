;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.letfn
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.analyzer :as ana2]
            [clojure.core.typed.checker.check.let :as let]
            [clojure.core.typed.checker.type-rep :as r]))

; annotations are in the first expression of the body (a :do)
(defn check-letfn [bindings body letfn-expr expected check]
  (let [;; must pass over bindings first to uniquify
        bindings (mapv ana2/run-pre-passes bindings)
        body (update-in body [:statements 0] ana2/run-passes)
        inits-expected
        ;try and find annotations, and throw a delayed error if not found
        ;(this expression returns nil)
        (when (and (#{:quote} (-> body :statements first :op))
                   (#{:const} (-> body :statements first :expr :op))
                   (vector? (-> body :statements first :expr :val)))
          (if-not (= (count (-> body :statements first :expr :val))
                     (count bindings))
            (do (err/tc-delayed-error "letfn requires each binding be annotated")
                nil)
            (into {}
                  (for [[nme type-syn] (mapv vector (map :name bindings) (-> body :statements first :expr :val))]
                    [nme (binding [prs/*parse-type-in-ns* (cu/expr-ns letfn-expr)]
                           (prs/parse-type type-syn))]))))]
    (if-not inits-expected
      (err/tc-delayed-error (str "letfn requires annotation, see: "
                               (impl/impl-case :clojure 'clojure :cljs 'cljs) ".core.typed/letfn>")
                          :return (assoc letfn-expr
                                         u/expr-type (cu/error-ret expected)))

      (let [cbindings
            (lex/with-locals inits-expected
              (vec
                (for [{:keys [name init] :as b} bindings]
                  (let [expected-fn (inits-expected name)
                        _ (assert expected-fn (str "No expected type for " name
                                                   " " (keys inits-expected)))
                        ; we already uniquified bindings above, so I don't think
                        ; we want to check the :binding node
                        cinit (check init (r/ret expected-fn))]
                    (assoc b
                           :init cinit
                           u/expr-type (u/expr-type cinit))))))

            cbody (lex/with-locals inits-expected
                    (check body expected))
            unshadowed-ret (let/erase-objects (map :name cbindings) (u/expr-type cbody))]
        (assoc letfn-expr
               :bindings cbindings
               :body cbody
               u/expr-type unshadowed-ret)))))
