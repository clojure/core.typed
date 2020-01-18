;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.reset-env
  (:require [clojure.core.typed.checker.jvm.base-env :as bse-clj]
            [clojure.core.typed.checker.ns-options :as ns-opts]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.jvm.mm-env :as mmenv]))

(def ^:private reset-cljs-envs! (delay (impl/dynaload 'clojure.core.typed.base-env-cljs/reset-cljs-envs!)))

(defn reset-envs!
  "Reset all environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  ([] (reset-envs! false))
  ([cljs?]
  (let []
    (impl/impl-case
      :clojure
      (do (bse-clj/reset-clojure-envs!)
          (mmenv/reset-mm-dispatch-env!)
          (ns-opts/reset-ns-opts!))
      :cljs
      (do
        (assert cljs? "No ClojureScript dependency")
        (when cljs?
          (@reset-cljs-envs!)
          (ns-opts/reset-ns-opts!))))
    nil)))

(defn load-core-envs! 
  "Add core annotations to environments for all implementations. Cannot be called
  if a specific implementation is currently bound"
  ([] (load-core-envs! false))
  ([cljs?]
  (let []
    (impl/impl-case
      :clojure
      (do (bse-clj/refresh-core-clojure-envs!)
          ;(mmenv/reset-mm-dispatch-env!)
          ;(ns-opts/reset-ns-opts!)
          )
      :cljs
      (do
        (assert nil "load-core-envs! TODO CLJS")
        (assert cljs? "No ClojureScript dependency")
        (when cljs?
          (@reset-cljs-envs!)
          (ns-opts/reset-ns-opts!))))
    nil)))

