(ns clojure.core.typed.check.set-bang
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.subtype :as sub]))

(defn check-set! [check {:keys [target val env] :as expr} expected]
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [ctarget (check target expected)
          cval (check val (u/expr-type ctarget))]
      (assoc expr
             u/expr-type (u/expr-type cval)
             :target ctarget
             :val cval))))
