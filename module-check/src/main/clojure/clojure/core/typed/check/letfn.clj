(ns clojure.core.typed.check.letfn
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.type-rep :as r]))

; annotations are in the first expression of the body (a :do)
(defn check-letfn [bindings body letfn-expr expected check-fn-letfn]
  (let [inits-expected
        ;try and find annotations, and throw a delayed error if not found
        ;(this expression returns nil)
        (when (#{:map} (-> body :statements first :op))
          (into {}
                (for [[lb-expr type-syn-expr] 
                      (map vector 
                           (-> body :statements first :keys)
                           (-> body :statements first :vals))]
                  (impl/impl-case
                    :clojure (do
                               (assert (#{:local} (:op lb-expr)))
                               [(-> lb-expr :name)
                                (binding [prs/*parse-type-in-ns* (cu/expr-ns letfn-expr)]
                                  (prs/parse-type (ast-u/constant-expr type-syn-expr)))])
                    :cljs [(-> lb-expr :info :name)
                           (binding [prs/*parse-type-in-ns* (cu/expr-ns letfn-expr)]
                             (prs/parse-type (:form type-syn-expr)))]))))]
    (if-not inits-expected
      (err/tc-delayed-error (str "letfn requires annotation, see: "
                               (impl/impl-case :clojure 'clojure :cljs 'cljs) ".core.typed/letfn>")
                          :return (assoc letfn-expr
                                         u/expr-type (cu/error-ret expected)))

      (let [cbinding-inits
            (lex/with-locals inits-expected
              (vec
                (for [{:keys [name init] :as b} bindings]
                  (let [expected-fn (inits-expected name)
                        _ (assert expected-fn (str "No expected type for " name))
                        cinit (check-fn-letfn init (r/ret expected-fn))]
                    (assoc b
                           :init cinit
                           u/expr-type (u/expr-type cinit))))))

            cbody (lex/with-locals inits-expected
                    (check-fn-letfn body expected))]
        (assoc letfn-expr
               :bindings cbinding-inits
               :body cbody
               u/expr-type (u/expr-type cbody))))))
