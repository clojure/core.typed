;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.try
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.object-rep :as o]
            [clojure.core.typed.checker.type-ctors :as c]))

(defn combine-rets [rs]
  {:pre [(seq rs)
         (every? r/TCResult? rs)]
   :post [(r/TCResult? %)]}
  (r/ret (apply c/Un (map r/ret-t rs))
         (fo/-FS (apply fo/-or (map (comp :then r/ret-f) rs))
                 (apply fo/-or (map (comp :else r/ret-f) rs)))
         (if (apply = (map r/ret-o rs))
           (r/ret-o (first rs))
           o/-empty)
         (r/-flow (apply fo/-or (map (comp :normal r/ret-flow) rs))))) 

; filters don't propagate between components of a `try`, nor outside of it.
(defn check-try [check {:keys [body catches finally] :as expr} expected]
  (let [chk #(check % expected)
        cbody (chk body)
        ;_ (prn "cbody ret" (u/expr-type cbody))
        ;_ (prn cbody)
        ccatches (mapv chk catches)
        ;_ (prn "ccatches ret" (mapv u/expr-type ccatches))
        ; finally result is thrown away
        cfinally (when finally
                   (check finally))
        ret (binding [vs/*current-expr* expr]
              (below/maybe-check-below
                (combine-rets
                  (map u/expr-type (concat [cbody] ccatches)))
                expected))]
    ;(prn "try ret" ret)
    (assoc expr
           :body cbody
           :catches ccatches
           :finally cfinally
           u/expr-type ret)))
