;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.ext.clojure.core
  "Typing rules for base Clojure distribution."
  (:require [clojure.core.typed.checker.jvm.check :refer [-custom-rule]]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed :as t]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.analyzer.common :as ana2]))

(defmethod -custom-rule 'clojure.core/ns
  [expr expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (-> expr
      ana2/run-passes 
      (assoc 
        u/expr-type (below/maybe-check-below
                      (r/ret r/-nil
                             (fo/-FS fl/-bot fl/-top))
                      expected))))

(comment
        (binding [*ns* *ns*]
          (t/cf (ns foo)))
        (binding [*ns* *ns*]
          (t/check-form-info '(ns foo)
                             :expected 't/Str
                             :type-provided? true))
        )
