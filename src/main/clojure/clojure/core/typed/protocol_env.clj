(ns clojure.core.typed.protocol-env
  (:require (clojure.core.typed
             [utils :as u]
             [type-rep :as r])
            [clojure.core.typed :as t :refer [fn>]])
  (:import (clojure.lang IPersistentMap Symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(t/def-alias ProtocolEnv 
  "A map mapping protocol symbols their types."
  (IPersistentMap Symbol r/TCType))

(t/ann PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce PROTOCOL-ENV (atom {}))
(t/tc-ignore
(set-validator! PROTOCOL-ENV (u/hash-c? (every-pred symbol? namespace) r/Type?))
  )

(t/ann add-protocol [Symbol r/TCType -> nil])
(defn add-protocol [sym t]
  (swap! PROTOCOL-ENV (fn> [e :- ProtocolEnv] 
                        (assoc e sym t)))
  nil)

(t/ann get-protocol [Symbol -> (U nil r/TCType)])
(defn get-protocol 
  "Returns the protocol with var symbol sym.
  Returns nil if not found."
  [sym]
  (@PROTOCOL-ENV sym))

(t/ann resolve-protocol [Symbol -> r/TCType])
(defn resolve-protocol [sym]
  (let [p (@PROTOCOL-ENV sym)]
    (assert p (str "Could not resolve Protocol: " sym))
    (assert (not (r/Poly? p)) (str "Protocol " sym " takes mandatory arguments, none provided"))
    p))

