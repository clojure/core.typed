(ns ^:skip-wiki clojure.core.typed.protocol-env
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

(ann protocol-env? [Any -> Any])
(def protocol-env? (u/hash-c? #(when (symbol? %)
                                 (namespace %)) 
                              (some-fn r/Protocol? r/TypeFn?)))

(ann CLJ-PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce CLJ-PROTOCOL-ENV (atom {} :validator protocol-env?))

(ann CLJS-PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce CLJS-PROTOCOL-ENV (atom {} :validator protocol-env?))

(ann assert-protocol-env [-> Any])
(defn assert-protocol-env []
  (assert *current-protocol-env* "No current protocol env"))

(ann reset-protocol-env! [ProtocolEnv -> nil])
(defn reset-protocol-env! [e]
  (assert-protocol-env)
  (when-let-fail [env *current-protocol-env*]
    (reset! env e))
  nil)

(ann add-protocol [Symbol r/TCType -> nil])
(defn add-protocol [sym t]
  (assert-protocol-env)
  (when-let-fail [e *current-protocol-env*]
    (let [swap!' (t/inst swap! ProtocolEnv ProtocolEnv Symbol r/TCType)
          assoc' (t/inst assoc Symbol r/TCType Any)]
      (swap!' e assoc' sym t)))
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

