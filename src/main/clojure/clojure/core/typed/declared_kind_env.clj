(ns clojure.core.typed.declared-kind-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declared kind Env

(defonce DECLARED-KIND-ENV (atom {}))
(set-validator! DECLARED-KIND-ENV (u/hash-c? symbol? r/TypeFn?))

(defn add-declared-kind [sym tfn]
  {:pre [(symbol? sym)
         (r/TypeFn? tfn)]}
  (swap! DECLARED-KIND-ENV assoc sym tfn)
  nil)

(defn declared-kind-or-nil [sym]
  (@DECLARED-KIND-ENV sym))

(defn get-declared-kind [sym]
  (if-let [tfn (declared-kind-or-nil sym)]
    tfn
    (throw (Exception. (u/error-msg "No declared kind for Name " sym)))))

(defn has-declared-kind? [sym]
  (boolean (declared-kind-or-nil sym)))

(defn remove-declared-kind [sym]
  (swap! DECLARED-KIND-ENV dissoc sym)
  nil)

(defn declare-alias-kind* [sym ty]
  (add-declared-kind sym ty))
