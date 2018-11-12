;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.local-result
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.check.utils :as cu]))

(defn local-ret [sym]
  {:pre [(symbol? sym)]
   :post [(r/TCResult? %)]}
  (let [[obj t] ((juxt lex/lookup-alias lex/lookup-local) sym)]
    (when-not t
      (err/int-error (str "Could not find type for local variable " sym)))
    (r/ret t 
           (if (c/overlap t (c/Un r/-nil r/-false))
             (fo/-FS (fo/-not-filter-at r/-falsy obj)
                     (fo/-filter-at r/-falsy obj))
             (fo/-true-filter))
           obj)))

(defn local-result [expr sym expected]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (prs/with-unparse-ns (cu/expr-ns expr)
      (below/maybe-check-below
        (local-ret sym)
        expected))))
