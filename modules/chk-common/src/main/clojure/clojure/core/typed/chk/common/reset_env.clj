(ns ^:skip-wiki clojure.core.typed.chk.common.reset-env
  (:require [clojure.core.typed.impl.jvm.base-env :as bse-clj]
            [clojure.core.typed.chk.common.ns-deps :as deps]
            [clojure.core.typed.chk.common.ns-options :as ns-opts]
            [clojure.core.typed.chk.common.current-impl :as impl]
            [clojure.core.typed.chk.common.mm-env :as mmenv]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn load-cljs? []
  ((impl/v 'clojure.core.typed.chk.common.init/cljs?)))

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
          ((impl/v 'clojure.core.typed.impl.js.base-env-cljs/reset-cljs-envs!))
          (deps/reset-deps!)
          (ns-opts/reset-ns-opts!))))
    nil))

