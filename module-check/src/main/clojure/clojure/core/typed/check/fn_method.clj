(ns clojure.core.typed.check.fn-method
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.fn-method-one :as fn-method1]))

;[MethodExpr FnIntersection & :optional {:recur-target-fn (U nil [Function -> RecurTarget])}
;   -> (Seq {:ftype Function :cmethod Expr})]
(defn check-fn-method [method fin & {:keys [recur-target-fn]}]
  {:pre [(r/FnIntersection? fin)]
   :post [(seq %)
          (every? (comp r/Function? :ftype) %)
          (every? :cmethod %)]}
  (u/p :check/check-fn-method
  (let [required-params (ast-u/method-required-params method)
        rest-param (ast-u/method-rest-param method)
        mfns (cu/relevant-Fns required-params rest-param fin)]
    (cond
      ;If no matching cases, assign parameters to Any
      (empty? mfns) [(fn-method1/check-fn-method1 
                       method 
                       (r/make-Function (repeat (count required-params) r/-any) ;doms
                                        r/-any  ;rng
                                        :rest
                                        (when rest-param ;rest
                                          r/-any))
                       :recur-target-fn recur-target-fn)]
      :else (vec
              (for [f mfns]
                (fn-method1/check-fn-method1 
                  method 
                  f
                  :recur-target-fn recur-target-fn)))))))

