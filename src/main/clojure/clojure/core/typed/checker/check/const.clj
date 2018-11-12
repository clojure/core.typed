;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.const
  (:require [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.type-rep :as r]))

(defn flow-for-value []
  (let [props (:props (lex/lexical-env))
        flow (r/-flow (apply fo/-and fl/-top props))]
    flow))

(defn filter-for-value [val]
  (if val
    (fo/-FS fl/-top fl/-bot)
    (fo/-FS fl/-bot fl/-top)))

(defn check-const
  "Given a :const node and an expected type returns a new :const
  node annotated with its type.
  
  quoted? should be true if this :const node is nested inside a
  :quote node, otherwise should be false"
  [constant-type quoted? {:keys [val] :as expr} expected]
  {:pre [(#{:const} (:op expr))
         ((some-fn nil? r/TCResult?) expected)]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [inferred-ret (r/ret (constant-type val quoted?)
                            (filter-for-value val)
                            obj/-empty
                            (flow-for-value))]
    (assoc expr
           u/expr-type (below/maybe-check-below inferred-ret expected))))
