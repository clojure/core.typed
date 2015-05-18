(ns ^:skip-wiki clojure.core.typed.protocol-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-ann]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.errors-ann]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(t/defalias ProtocolEnv 
  "A map mapping protocol symbols their types."
  (t/Map t/Sym r/Type))

(t/ann *current-protocol-env* (t/U nil (t/Atom1 ProtocolEnv)))
(defonce ^:dynamic *current-protocol-env* nil)

(t/ann protocol-env? [t/Any -> t/Any])
(def protocol-env? (con/hash-c? #(when (symbol? %)
                                   (namespace %)) 
                                (some-fn r/Protocol? r/TypeFn?)))

(t/ann CLJ-PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce CLJ-PROTOCOL-ENV (atom {} :validator protocol-env?))

(t/ann CLJS-PROTOCOL-ENV (t/Atom1 ProtocolEnv))
(defonce CLJS-PROTOCOL-ENV (atom {} :validator protocol-env?))

(t/ann assert-protocol-env [-> t/Any])
(defn assert-protocol-env []
  (assert *current-protocol-env* "No current protocol env"))

(t/ann reset-protocol-env! [ProtocolEnv -> nil])
(defn reset-protocol-env! [e]
  (assert-protocol-env)
  (t/when-let-fail [env *current-protocol-env*]
    (reset! env e))
  nil)

(t/ann add-protocol [t/Sym r/Type -> nil])
(defn add-protocol [sym t]
  ;(prn "add-protocol" sym t)
  (assert-protocol-env)
  (t/when-let-fail [e *current-protocol-env*]
    (let [swap!' (t/inst swap! ProtocolEnv ProtocolEnv t/Sym r/Type)
          assoc' (t/inst assoc t/Sym r/Type t/Any)]
      (swap!' e assoc' sym t)))
  nil)

(t/ann get-protocol [t/Sym -> (t/U nil r/Type)])
(defn get-protocol 
  "Returns the protocol with var symbol sym.
  Returns nil if not found."
  [sym]
  (assert-protocol-env)
  (t/when-let-fail [e *current-protocol-env*]
    (@e sym)))

(t/ann resolve-protocol [t/Sym -> r/Type])
(defn resolve-protocol [sym]
  (assert-protocol-env)
  (let [p (get-protocol sym)]
    (when-not p 
      (err/int-error (str "Could not resolve Protocol: " sym
                          "\n\nHint: Add protocol annotations with ann-protocol")))
    p))

