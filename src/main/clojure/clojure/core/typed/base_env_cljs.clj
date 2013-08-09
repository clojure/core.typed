(ns clojure.core.typed.base-env-cljs
  (:require [clojure.core.typed.base-env-helper-cljs :as h]
            [clojure.core.typed.current-impl :as impl :refer [v]]))

(def init-var-env
  (merge
    (h/var-mappings

cljs.core.typed/ann* [Any Any -> Any]
      )))

(def init-var-nochecks
  (set (keys init-var-env)))

(defn reset-cljs-envs! []
  (impl/with-cljs-impl
    ((v 'clojure.core.typed.var-env/reset-var-type-env!)
     init-var-env init-var-nochecks)))
