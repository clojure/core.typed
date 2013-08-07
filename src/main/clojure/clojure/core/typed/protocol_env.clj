(ns clojure.core.typed.protocol-env
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t :refer [fn> ann def-alias when-let-fail]])
  (:import (clojure.lang IPersistentMap Symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(def-alias ProtocolEnv 
  "A map mapping protocol symbols their types."
  (IPersistentMap Symbol r/TCType))

(ann *current-protocol-env* (U nil (t/Atom1 ProtocolEnv)))
(def ^:dynamic *current-protocol-env* nil)

(ann ^:no-check protocol-env? [Any -> Any])
(def protocol-env? (u/hash-c? (every-pred symbol? namespace) (some-fn r/Protocol? r/TypeFn?)))

(ann CLJ-PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce CLJ-PROTOCOL-ENV (atom {}))
(t/tc-ignore
(set-validator! CLJ-PROTOCOL-ENV protocol-env?)
  )

(ann CLJS-PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce CLJS-PROTOCOL-ENV (atom {}))
(t/tc-ignore
(set-validator! CLJS-PROTOCOL-ENV protocol-env?)
  )

(ann assert-protocol-env [-> Any])
(defn assert-protocol-env []
  (assert *current-protocol-env* "No current protocol env"))

(ann add-protocol [Symbol r/TCType -> nil])
(defn add-protocol [sym t]
  (assert-protocol-env)
  (when-let-fail [e *current-protocol-env*]
    (swap! e (fn> [e :- ProtocolEnv]
               (assoc e sym t))))
  nil)

(ann get-protocol [Symbol -> (U nil r/TCType)])
(defn get-protocol 
  "Returns the protocol with var symbol sym.
  Returns nil if not found."
  [sym]
  (assert-protocol-env)
  (when-let-fail [e *current-protocol-env*]
    (@e sym)))

(ann resolve-protocol [Symbol -> r/TCType])
(defn resolve-protocol [sym]
  (assert-protocol-env)
  (let [p (get-protocol sym)]
    (assert p (str "Could not resolve Protocol: " sym))
    p))

