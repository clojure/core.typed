;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.get
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check.invoke-kw :as invoke-kw]
            [clojure.core.typed.checker.check.utils :as cu]))

(defn invoke-get [{:keys [args] :as expr} expected & {:keys [cargs]}]
  {:pre [(vector? cargs)]
   :post [((some-fn 
             #(-> % u/expr-type r/TCResult?)
             #{cu/not-special})
           %)]}
  (assert (#{:invoke :static-call} (:op expr)) (:op expr))
  (assert (vector? cargs))
  (assert (#{2 3} (count args)) "Wrong number of args to clojure.core/get")
  (let [[ctarget ckw cdefault] cargs
        kwr (u/expr-type ckw)]
    (cond
      (c/keyword-value? (r/ret-t kwr))
      (assoc expr
             :args cargs
             u/expr-type (invoke-kw/invoke-keyword 
                           expr
                           kwr
                           (u/expr-type ctarget)
                           (when cdefault
                             (u/expr-type cdefault))
                           expected))

;      ((every-pred r/Value? (comp integer? :val)) (ret-t kwr))
;      (err/nyi-error (str "get lookup of vector (like nth) NYI"))

      :else cu/not-special)))
