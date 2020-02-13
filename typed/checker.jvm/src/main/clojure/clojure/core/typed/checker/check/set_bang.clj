;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.set-bang
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.jvm.subtype :as sub]))

(defn check-set! [check {:keys [target val env] :as expr} expected]
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [ctarget (check target expected)
          cval (check val (u/expr-type ctarget))]
      (assoc expr
             u/expr-type (u/expr-type cval)
             :target ctarget
             :val cval))))
