(ns clojure.core.typed.chk.jvm.method-override-env
  (:require [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed.chk.common.type-rep :as r]))

; Should only override a method with a more specific type
; eg. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Override Env

(defonce METHOD-OVERRIDE-ENV (atom {}))
(set-validator! METHOD-OVERRIDE-ENV (u/hash-c? (every-pred namespace symbol?)
                                               (some-fn r/Poly? r/FnIntersection?)))

(defn add-method-override [sym t]
  (swap! METHOD-OVERRIDE-ENV assoc sym t)
  nil)

(defn reset-method-override-env! [m]
  (reset! METHOD-OVERRIDE-ENV m)
  nil)
