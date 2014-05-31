(ns clojure.core.typed.collect.typed-deps
  (:require [clojure.core.typed.collect-utils :as clt-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.check.utils :as chk-u]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.ns-deps :as dep]
            [clojure.core.typed.coerce-utils :as coerce]))

(defn collect-typed-deps
  [collect-ns {:keys [args] :as expr}]
  (clt-u/assert-expr-args expr #{1})
  (let [prs-ns (chk-u/expr-ns expr)
        [deps] (ast-u/constant-exprs args)
        _ (assert (and deps (seq deps) (every? symbol? deps)))]
    (if vs/*already-collected*
      (do (dep/add-ns-deps prs-ns (set deps))
          (doseq [dep deps]
            (if (coerce/ns->URL dep)
              (collect-ns dep)
              (err/int-error (str "Cannot find dependency declared with typed-deps: " dep)))))
      (do (println "WARNING: Not collecting namespaces, must call typed-deps via check-ns")
          (flush)))
    nil))

