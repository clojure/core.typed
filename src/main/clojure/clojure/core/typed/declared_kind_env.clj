(ns clojure.core.typed.declared-kind-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declared kind Env

(defonce DECLARED-KIND-ENV (atom {}))
(set-validator! DECLARED-KIND-ENV (u/hash-c? (every-pred symbol? namespace) r/TypeFn?))

(defn add-declared-kind [sym tfn]
  (swap! DECLARED-KIND-ENV assoc sym tfn))

(defn get-declared-kind [sym]
  (if-let [tfn (@DECLARED-KIND-ENV sym)]
    tfn
    (throw (Exception. (u/error-msg "No declared kind for Name " sym)))))

(defn declare-alias-kind* [sym ty]
  (add-declared-kind sym ty))
