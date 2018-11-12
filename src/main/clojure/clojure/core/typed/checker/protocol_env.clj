;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.protocol-env
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-ann]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.errors-ann]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.env :as env]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(t/defalias ProtocolEnv 
  "A map mapping protocol symbols their types."
  (t/Map t/Sym r/Type))

(t/ann protocol-env? [t/Any -> t/Any])
(def protocol-env? (con/hash-c? (every-pred symbol? namespace)
                                (some-fn delay? r/Protocol? r/TypeFn?)))

(t/ann ^:no-check protocol-env [-> ProtocolEnv])
(defn protocol-env []
  {:post [(map? %)
          #_(protocol-env? %)]}
  (get (env/deref-checker) impl/current-protocol-env-kw {}))

(t/ann ^:no-check reset-protocol-env! [ProtocolEnv -> nil])
(defn reset-protocol-env! [e]
  {:pre [#_(protocol-env? e)]}
  (env/swap-checker! assoc impl/current-protocol-env-kw e)
  nil)

(defn merge-protocol-env! [e]
  {:pre [(map? e)]}
  (env/swap-checker! update impl/current-protocol-env-kw merge e)
  nil)

(t/ann ^:no-check add-protocol [t/Sym r/Type -> nil])
(def add-protocol impl/add-protocol)

(t/ann get-protocol [t/Sym -> (t/U nil r/Type)])
(defn get-protocol 
  "Returns the protocol with var symbol sym.
  Returns nil if not found."
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/Protocol? r/TypeFn?) %)]}
  (force (get (protocol-env) sym)))

(t/ann resolve-protocol [t/Sym -> r/Type])
(defn resolve-protocol [sym]
  {:post [(r/Type? %)]}
  (let [p (get-protocol sym)]
    (when-not p 
      (err/int-error (str "Could not resolve Protocol: " sym
                          "\n\nHint: Add protocol annotations with ann-protocol")))
    p))
