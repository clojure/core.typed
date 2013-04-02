(ns clojure.core.typed.protocol-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(defonce PROTOCOL-ENV (atom {}))
(set-validator! PROTOCOL-ENV (u/hash-c? (every-pred symbol? namespace) r/Type?))

(defn add-protocol [sym t]
  (swap! PROTOCOL-ENV assoc sym t)
  nil)

(defn resolve-protocol [sym]
  (let [p (@PROTOCOL-ENV sym)]
    (assert p (str "Could not resolve Protocol: " sym))
    (assert (not (r/Poly? p)) (str "Protocol " sym " takes mandatory arguments, none provided"))
    p))

