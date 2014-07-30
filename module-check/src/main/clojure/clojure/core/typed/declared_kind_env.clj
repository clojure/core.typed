(ns clojure.core.typed.declared-kind-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declared kind Env

(defonce ^:dynamic *current-declared-kinds* nil)

(defn assert-declared-kinds []
  (assert *current-declared-kinds* "No declared kinds bound"))

(defonce CLJ-DECLARED-KIND-ENV (atom {}))
(set-validator! CLJ-DECLARED-KIND-ENV (con/hash-c? symbol? r/TypeFn?))

(defonce CLJS-DECLARED-KIND-ENV (atom {}))
(set-validator! CLJS-DECLARED-KIND-ENV (con/hash-c? symbol? r/TypeFn?))

(defn reset-declared-kinds! [m]
  (assert-declared-kinds)
  (reset! *current-declared-kinds* m))

(defn add-declared-kind [sym tfn]
  {:pre [(symbol? sym)
         (r/TypeFn? tfn)]}
  (assert-declared-kinds)
  (swap! *current-declared-kinds* assoc sym tfn)
  nil)

(defn declared-kind-or-nil [sym]
  (assert-declared-kinds)
  (@*current-declared-kinds* sym))

(defn get-declared-kind [sym]
  (assert-declared-kinds)
  (if-let [tfn (declared-kind-or-nil sym)]
    tfn
    (err/int-error (str "No declared kind for Name " sym))))

(defn has-declared-kind? [sym]
  (assert-declared-kinds)
  (boolean (declared-kind-or-nil sym)))

(defn remove-declared-kind [sym]
  (assert-declared-kinds)
  (swap! *current-declared-kinds* dissoc sym)
  nil)

(defn declare-alias-kind* [sym ty]
  (assert-declared-kinds)
  (add-declared-kind sym ty))
