(ns clojure.core.typed.chk.jvm.ctor-override-env
  (:require [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed.chk.common.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor Override Env

(defonce CONSTRUCTOR-OVERRIDE-ENV 
  (atom {}
        :validator (u/hash-c? symbol? r/Type?)))

(defn add-constructor-override [sym t]
  (swap! CONSTRUCTOR-OVERRIDE-ENV assoc sym t)
  nil)

(defn reset-constructor-override-env! [m]
  (reset! CONSTRUCTOR-OVERRIDE-ENV m)
  nil)
