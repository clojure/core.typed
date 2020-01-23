;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.ctor-override-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor Override Env

(def add-constructor-override impl/add-constructor-override)

(defn reset-constructor-override-env! [m]
  (env/swap-checker! assoc impl/constructor-override-env-kw m)
  nil)

(defn merge-constructor-override-env! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/constructor-override-env-kw merge m)
  nil)

(defn constructor-override-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/constructor-override-env-kw {}))

(defn get-constructor-override [sym]
  {:post [((some-fn nil? r/Type?) %)]}
  (force (get (constructor-override-env) sym)))
