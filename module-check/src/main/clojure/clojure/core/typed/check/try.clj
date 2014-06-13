(ns clojure.core.typed.check.try
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]))

; filters don't propagate between components of a `try`, nor outside of it.
(defn check-try [check {:keys [body catches finally] :as expr} expected]
  (let [chk #(check % expected)
        cbody (chk body)
        ccatches (mapv chk catches)
        ; finally result is thrown away
        cfinally (when finally
                   (check finally))]
    (assoc expr
           :body cbody
           :catches ccatches
           :finally cfinally
           u/expr-type (r/ret (apply c/Un (-> cbody u/expr-type r/ret-t) 
                                     (map (comp r/ret-t u/expr-type) ccatches))))))
