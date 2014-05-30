(ns clojure.core.typed.check.special.ann-form
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.subtype :as sub]))

(defn check-ann-form
  [check {[_ _ {{tsyn :type} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (let [parsed-ty (binding [vs/*current-env* env
                            prs/*parse-type-in-ns* (cu/expr-ns expr)]
                    (prs/parse-type tsyn))
        cty (check frm (r/ret parsed-ty))
        checked-type (r/ret-t (u/expr-type cty))
        _ (binding [vs/*current-expr* frm]
            (when (not (sub/subtype? checked-type parsed-ty))
              (cu/expected-error checked-type parsed-ty)))
        _ (when (and expected (not (sub/subtype? checked-type (r/ret-t expected))))
            (binding [vs/*current-expr* frm
                      vs/*current-env* env]
              (cu/expected-error checked-type (r/ret-t expected))))]
    (assoc expr
           :ret cty
           u/expr-type (r/ret parsed-ty))))
