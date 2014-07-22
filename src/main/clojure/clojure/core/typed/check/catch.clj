(ns clojure.core.typed.check.catch
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.ast-utils :as ast-u]))

(defn check-catch [check {handler :body :keys [local] :as expr} expected]
  (let [ecls (ast-u/catch-op-class expr)
        local-sym (:name local)
        local-type (impl/impl-case
                     :clojure (c/RClass-of-with-unknown-params ecls)
                     :cljs (err/nyi-error "catch in CLJS"))
        chandler (lex/with-locals {local-sym local-type}
                   (check handler expected))]
    (assoc expr
           u/expr-type (u/expr-type chandler))))
