(ns clojure.core.typed.check.multi
  (:require [clojure.core.typed.fold-rep :as fold]
            [clojure.core.typed.type-rep :as r])
  (:import (clojure.core.typed.type_rep Function)))

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
