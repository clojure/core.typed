;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.throw
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.utils :as u]))

(defn check-throw
  [check {:keys [exception] :as expr} expected exception-expected]
  {:pre [((some-fn nil? r/TCResult?) exception-expected)]}
  (let [cexception (check exception exception-expected)
        ret (below/maybe-check-below
              (r/ret (c/Un)
                     (fo/-unreachable-filter)
                     obj/-empty
                     ;never returns normally
                     (r/-flow fl/-bot))
              expected)]
    (assoc expr
           :exception cexception
           u/expr-type ret)))
