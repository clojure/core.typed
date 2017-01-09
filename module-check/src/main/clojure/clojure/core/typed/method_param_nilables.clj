(ns clojure.core.typed.method-param-nilables
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defn reset-method-nilable-param-env! [m]
  (env/swap-checker! assoc impl/method-param-nilable-env-kw m)
  nil)

(defn merge-method-nilable-param-env! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/method-param-nilable-env-kw merge m)
  nil)

(def add-method-nilable-param impl/add-method-nilable-param)

(defn nilable-param-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/method-param-nilable-env-kw {}))

(defn nilable-param? [sym arity param]
  (boolean 
    (when-let [nilables (get (nilable-param-env) sym)]
      (when-let [params (or (nilables :all)
                            (nilables arity))]
        (or (#{:all} params)
            (params param))))))
