(ns ^:skip-wiki clojure.core.typed.all-envs
  (:require [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.current-impl :as impl]
            ))

(defn name-env []
  (let [nme-env (nme-env/name-env)]
    (binding [vs/*verbose-types* true]
      (into {}
            (for [[k v] nme-env]
              (when-not (keyword? v)
                [k (prs/unparse-type v)]))))))

(defn var-env []
  (let [var-env (var-env/var-annotations)]
    (assert var-env)
    (binding [vs/*verbose-types* true]
      (into {}
            (for [[k v] var-env]
              [k (prs/unparse-type (force v))])))))

(defn all-envs-clj []
  (impl/with-clojure-impl
    {:aliases (name-env)
     :vars (var-env)}))
