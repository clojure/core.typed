(ns ^:skip-wiki clojure.core.typed.all-envs
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-if-needed :refer [load-if-needed]]))

(defn name-env []
  (load-if-needed)
  (let [nme-env ((impl/v 'clojure.core.typed.name-env/name-env))]
    (binding [vs/*verbose-types* true]
      (into {}
            (for [[k v] nme-env]
              (when-not (keyword? v)
                [k ((impl/v 'clojure.core.typed.parse-unparse/unparse-type)
                    v)]))))))

(defn var-env []
  (load-if-needed)
  (let [var-env ((impl/v 'clojure.core.typed.var-env/var-annotations))]
    (assert var-env)
    (binding [vs/*verbose-types* true]
      (into {}
            (for [[k v] var-env]
              [k ((impl/v 'clojure.core.typed.parse-unparse/unparse-type)
                  (force v))])))))

(defn all-envs-clj []
  (impl/with-clojure-impl
    {:aliases (name-env)
     :vars (var-env)}))
