(ns ^:skip-wiki clojure.core.typed.reset-env
  (:require [clojure.core.typed.base-env :as bse-clj]
            [clojure.core.typed.base-env-cljs :as bse-cljs]
            [clojure.core.typed.ns-deps :as deps]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-cljs :as load]
            [clojure.core.typed.mm-env :as mmenv]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn load-cljs? []
  (load/load-cljs)
  (boolean (find-ns 'cljs.analyzer)))

(defn reset-envs! 
  "Reset all environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  []
  (let [cljs? (load-cljs?)]
    (bse-clj/reset-clojure-envs!)
    (mmenv/reset-mm-dispatch-env!)
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

