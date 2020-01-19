;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.catch
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.ast-utils :as ast-u]))

(defn check-catch [check {handler :body :keys [local] :as expr} expected]
  (let [expr (-> expr
                 (update :class ana2/run-passes))
        ecls (ast-u/catch-op-class expr)
        local-sym (:name local)
        local-type (impl/impl-case
                     :clojure (c/RClass-of-with-unknown-params ecls)
                     :cljs (err/nyi-error "catch in CLJS"))
        chandler (lex/with-locals {local-sym local-type}
                   (check handler expected))]
    (assoc expr
           :body chandler
           u/expr-type (u/expr-type chandler))))
