;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.method-override-env
  (:require [clojure.core.typed.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-rep :as r]))

; Should only override a method with a more specific type
; eg. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Override Env

(def add-method-override impl/add-method-override)

(defn reset-method-override-env! [m]
  (env/swap-checker! assoc impl/method-override-env-kw m)
  nil)

(defn merge-method-override-env! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/method-override-env-kw merge m)
  nil)

(defn method-override-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/method-override-env-kw {}))

(defn get-method-override [m]
  {:post [((some-fn r/Poly? r/FnIntersection? nil?) %)]}
  (force (get (method-override-env) m)))
