(ns clojure.core.typed.method-override-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.type-rep :as r]))

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
