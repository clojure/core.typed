;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.monitor
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.check-below :as below]))

(defn check-monitor
  "monitor-enter and monitor-exit both take any object and return nil"
  [check {:keys [target] :as expr} expected]
  {:pre [((some-fn nil? r/TCResult?) expected)]}
  (binding [vs/*current-expr* expr]
    (assoc expr
           :target (check target (r/ret (c/RClass-of Object)))
           u/expr-type (below/maybe-check-below (r/ret r/-nil (fo/-false-filter))
                                                expected))))
