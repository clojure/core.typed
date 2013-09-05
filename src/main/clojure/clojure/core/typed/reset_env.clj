(ns ^:skip-wiki clojure.core.typed.reset-env
  (:require [clojure.core.typed.base-env :as bse-clj]
            [clojure.core.typed.base-env-cljs :as bse-cljs]
            [clojure.core.typed.ns-deps :as deps]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.current-impl :as impl]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn load-cljs? []
  (try (require 'cljs.analyzer)
       true
       (catch Throwable e
         false)))

(defn reset-envs! 
  "Reset all environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  []
  (let [cljs? (load-cljs?)]
    (bse-clj/reset-clojure-envs!)
    (when cljs?
      (bse-cljs/reset-cljs-envs!))
    (impl/with-clojure-impl
      (deps/reset-deps!)
      (ns-opts/reset-ns-opts!))
    (when cljs?
      (impl/with-cljs-impl
        (deps/reset-deps!)
        (ns-opts/reset-ns-opts!)))
    nil))

