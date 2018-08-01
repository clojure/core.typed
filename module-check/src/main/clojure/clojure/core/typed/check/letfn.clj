(ns clojure.core.typed.check.letfn
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.analyzer2 :as ana2]
            [clojure.core.typed.check.let :as let]
            [clojure.core.typed.type-rep :as r]))

; annotations are in the first expression of the body (a :do)
(defn check-letfn [bindings body letfn-expr expected check]
  (let [;; must pass over bindings first to uniquify
        bindings (mapv #((:pre ana2/scheduled-passes) %) bindings)
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
