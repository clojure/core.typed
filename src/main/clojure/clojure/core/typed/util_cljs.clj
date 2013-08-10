(ns clojure.core.typed.util-cljs
  (:require [cljs.analyzer :as ana]))

(defn ^:private empty-env-in-ns [nsym]
  {:ns {:name nsym} :context :statement :locals {}})

(defn resolve-var [nsym sym]
  (assert (contains? @ana/namespaces nsym)
          (str "Namespace " nsym " does not exist"))
  (ana/resolve-var (empty-env-in-ns nsym) sym))
