;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.with-meta
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.utils :as u]))

(defn visit-tail-pos [ast f]
  (let [rec #(visit-tail-pos % f)]
    (case (:op ast)
      :do (update ast :ret rec)
      ;; would be ambiguous when calculating whether to erase the :with-meta node
      :if (err/int-error "Not allowed :with-meta around :if")
      (:let :letfn) (update ast :body rec)
      ;; probably possible to handle, but seems likely to never occur in practice
      :with-meta (err/int-error "Not allowed nested :with-meta")
      (f ast))))

(defn check-with-meta
  [check {:keys [expr meta] :as with-meta-expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [erase-atom (atom nil)
        expr (visit-tail-pos expr (fn [ast]
                                    (assoc ast ::erase-atom erase-atom)))
        cexpr (check expr expected)
        cmeta (check meta)]
    (if @erase-atom
      cexpr
      (assoc with-meta-expr 
             :expr cexpr
             :meta cmeta
             u/expr-type (u/expr-type cexpr)))))
