(ns clojure.core.typed.check.get
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.check.invoke-kw :as invoke-kw]
            [clojure.core.typed.check.utils :as cu]))

(defn invoke-get [{:keys [args] :as expr} expected & {:keys [cargs]}]
  {:pre [(vector? cargs)]
   :post [((some-fn 
             #(-> % u/expr-type r/TCResult?)
             #{cu/not-special})
           %)]}
  (assert (#{:invoke :static-call} (:op expr)) (:op expr))
  (assert (vector? cargs))
  (assert (#{2 3} (count args)) "Wrong number of args to clojure.core/get")
  (let [[ctarget ckw cdefault] cargs
        kwr (u/expr-type ckw)]
    (cond
      (c/keyword-value? (r/ret-t kwr))
      (assoc expr
             :args cargs
             u/expr-type (invoke-kw/invoke-keyword 
                           expr
                           kwr
                           (u/expr-type ctarget)
                           (when cdefault
                             (u/expr-type cdefault))
                           expected))

;      ((every-pred r/Value? (comp integer? :val)) (ret-t kwr))
;      (err/nyi-error (str "get lookup of vector (like nth) NYI"))

      :else cu/not-special)))
