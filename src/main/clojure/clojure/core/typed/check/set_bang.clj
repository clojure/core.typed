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
    (let [ctarget (check target)
          cval (check val (u/expr-type ctarget))
          _ (when-not (sub/subtype? 
                        (-> cval u/expr-type r/ret-t)
                        (-> ctarget u/expr-type r/ret-t))
              (err/tc-delayed-error (str "Cannot set! " (-> ctarget u/expr-type r/ret-t prs/unparse-type pr-str)
                                         " to " (-> cval u/expr-type r/ret-t prs/unparse-type pr-str))))
          _ (when expected
              (let [actual (-> cval u/expr-type r/ret-t)
                    et (r/ret-t expected)]
                (when-not (sub/subtype? actual et)
                  (cu/expected-error actual et))))]
      (assoc expr
             u/expr-type (u/expr-type cval)
             :target ctarget
             :val cval))))
