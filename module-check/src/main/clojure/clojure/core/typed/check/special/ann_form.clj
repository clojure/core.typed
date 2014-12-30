(ns clojure.core.typed.check.special.ann-form
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]))

(defn check-ann-form
  [check {:keys [statements env] frm :ret :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (let [[_ _ texpr] statements
        tsyn (ast-u/map-expr-at texpr :type)
        parsed-t (binding [vs/*current-env* env
                           prs/*parse-type-in-ns* (cu/expr-ns expr)]
                   (prs/parse-type tsyn))
        cret (check frm 
                    (or (when expected
                          (assoc expected :t parsed-t))
                        (r/ret parsed-t
                               ;; TODO let users add expected filters
                               (fo/-FS fl/-top fl/-top)
                               obj/-empty
                               (r/-flow fl/-top))))]
    (assoc expr
           :ret cret
           u/expr-type (binding [vs/*current-expr* expr
                                 vs/*current-env* env]
                         (below/maybe-check-below
                           (u/expr-type cret)
                           expected)))))
