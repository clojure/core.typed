(ns ^:skip-wiki clojure.core.typed.reset-env
  (:require [clojure.core.typed.base-env :as bse-clj]
            [clojure.core.typed.ns-deps :as deps]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.mm-env :as mmenv]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn load-cljs? []
  ((impl/v 'clojure.core.typed.init/cljs?)))

(defn reset-envs! 
  "Reset all environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  []
  (let [cljs? (load-cljs?)]
    (impl/impl-case
      :clojure
      (do (bse-clj/reset-clojure-envs!)
          (mmenv/reset-mm-dispatch-env!)
          (deps/reset-deps!)
          (ns-opts/reset-ns-opts!))
      :cljs
      (do
        (assert cljs? "No ClojureScript dependency")
        (when cljs?
          ((impl/v 'clojure.core.typed.base-env-cljs/reset-cljs-envs!))
          (deps/reset-deps!)
          (ns-opts/reset-ns-opts!))))
    nil))

(defn load-core-envs! 
  "Add core annotations to environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  []
  (let [cljs? (load-cljs?)]
    (impl/impl-case
      :clojure
      (do (bse-clj/refresh-core-clojure-envs!)
          ;(mmenv/reset-mm-dispatch-env!)
          ;(deps/reset-deps!)
          ;(ns-opts/reset-ns-opts!)
          )
      :cljs
      (do
        (assert nil "load-core-envs! TODO CLJS")
        (assert cljs? "No ClojureScript dependency")
        (when cljs?
          ((impl/v 'clojure.core.typed.base-env-cljs/reset-cljs-envs!))
          (deps/reset-deps!)
          (ns-opts/reset-ns-opts!))))
    nil))

