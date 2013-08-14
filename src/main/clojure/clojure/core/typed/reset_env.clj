(ns ^:skip-wiki clojure.core.typed.reset-env
  (:require [clojure.core.typed.base-env :as bse-clj]
            [clojure.core.typed.base-env-cljs :as bse-cljs]
            [clojure.core.typed.ns-deps :as deps]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.current-impl :as impl]))

(defn reset-envs! 
  "Reset all environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  []
  (bse-clj/reset-clojure-envs!)
  (bse-cljs/reset-cljs-envs!)
  (impl/with-clojure-impl
    (deps/reset-deps!)
    (ns-opts/reset-ns-opts!))
  (impl/with-cljs-impl
    (deps/reset-deps!)
    (ns-opts/reset-ns-opts!))
  nil)

