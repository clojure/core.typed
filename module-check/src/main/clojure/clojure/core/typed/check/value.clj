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
            [clojure.core.typed.type-rep :as r]))

(defn flow-for-value []
  (let [props (:props lex/*lexical-env*)
        flow (r/-flow (apply fo/-and fl/-top props))]
    flow))

(defn check-value
  [{:keys [val] :as expr} expected]
  {:pre [(#{:const} (:op expr))]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [actual-type (const/constant-type val)
        _ (when (and expected (not (sub/subtype? actual-type (r/ret-t expected))))
            (binding [vs/*current-expr* expr]
              (cu/expected-error actual-type (r/ret-t expected))))
        flow (flow-for-value)]
    (assoc expr
           u/expr-type (if val
                         (r/ret actual-type
                                (fo/-FS fl/-top fl/-bot)
                                obj/-empty
                                flow)
                         (r/ret actual-type
                                (fo/-FS fl/-bot fl/-top)
                                obj/-empty
                                flow)))))
