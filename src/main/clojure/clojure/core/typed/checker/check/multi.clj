;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.multi
  (:require [clojure.core.typed.checker.fold-rep :as fold]
            [clojure.core.typed.checker.type-rep :as r])
  (:import (clojure.core.typed.checker.type_rep Function)))

;; Multimethod definition

(def ExpectedDispatchType ::expected-dispatch-type)

(fold/derive-default ExpectedDispatchType)

(fold/add-fold-case ExpectedDispatchType
                    Function
                    (fn [ty _]
                      (assoc ty :rng (r/make-Result r/-any))))

;return the expected type for the dispatch fn of the given multimethod's expected type
;[Type -> Type]
(defn expected-dispatch-type [mm-type]
  {:pre [(r/AnyType? mm-type)]
   :post [(r/AnyType? %)]}
  (fold/fold-rhs ExpectedDispatchType
                 {:type-rec expected-dispatch-type}
                 mm-type))
