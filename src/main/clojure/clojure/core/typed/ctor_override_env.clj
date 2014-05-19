(ns clojure.core.typed.ctor-override-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor Override Env

(defonce CONSTRUCTOR-OVERRIDE-ENV 
  (atom {}
        :validator (con/hash-c? symbol? r/Type?)))

(defn add-constructor-override [sym t]
  (swap! CONSTRUCTOR-OVERRIDE-ENV assoc sym t)
  nil)

(defn reset-constructor-override-env! [m]
  (reset! CONSTRUCTOR-OVERRIDE-ENV m)
  nil)
