(ns clojure.core.typed.check.value
  (:require [clojure.core.typed.constant-type :as const]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.type-rep :as r]))

(defn flow-for-value []
  (let [props (:props lex/*lexical-env*)
        flow (r/-flow (apply fo/-and fl/-top props))]
    flow))

(defn filter-for-value [val]
  (if val
    (fo/-FS fl/-top fl/-bot)
    (fo/-FS fl/-bot fl/-top)))

(defn check-value
  [{:keys [val] :as expr} expected]
  {:pre [(#{:const} (:op expr))
         ((some-fn nil? r/TCResult?) expected)]
   :post [(-> % u/expr-type r/TCResult?)]}
  ;(prn "check-value" val expected)
  (binding [vs/*current-expr* expr]
    (let [inferred-ret (r/ret (const/constant-type val)
                              (filter-for-value val)
                              obj/-empty
                              (flow-for-value))]
      (assoc expr
             u/expr-type (below/maybe-check-below inferred-ret expected)))))
