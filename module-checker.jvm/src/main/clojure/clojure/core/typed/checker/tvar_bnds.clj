;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki 
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.checker.tvar-bnds
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r])
  (:import (clojure.core.typed.checker.type_rep Bounds)))

;; this implements an environment from (fresh) type variable names
;; to their bounds.
;;
;; Intended to be equivalent to a field on F or B, but without the bloat
;; of adding it to every instance

(t/defalias TVarBndsEnv
  "A map from (fresh) type variable names (symbols) to
  their bounds."
  (t/Map t/Symbol Bounds))

(t/ann ^:no-check tvar-bnds-env? (t/Pred TVarBndsEnv))
(def tvar-bnds-env? (con/hash-c? symbol? r/Bounds?))

(t/ann initial-tvar-bnds-env TVarBndsEnv)
(def initial-tvar-bnds-env {})

(t/ann ^:no-check *current-tvar-bnds* TVarBndsEnv)
(defonce ^:dynamic *current-tvar-bnds* initial-tvar-bnds-env)
(set-validator! #'*current-tvar-bnds* tvar-bnds-env?)

(defn lookup-tvar-bnds
  "Returns the bounds of tvar or nil"
  [var]
  (*current-tvar-bnds* var))

(defn tvar-bnds-fail
  "Returns the bounds of tvar. Throws an exception if not found"
  [var]
  {:post [%]}
  (lookup-tvar-bnds var))

(defn extend-one
  "Extend a tvar bounds environment."
  [env var bnds]
  (assoc env var bnds))

(defn extend-many
  "Extend env with pairwise mappings from vars to bndss"
  [env vars bndss]
  {:pre [(every? symbol? vars)
         (every? r/Bounds? bndss)
         (= (count vars) (count bndss))]
   :post [(tvar-bnds-env? %)]}
  (merge env (zipmap vars bndss)))

(defmacro with-extended-bnds
  "Takes a list of vars and bnds extends the current tvar environment.
  vars are the fresh names of the frees, rather than the scoped names."
  [vars bndss & body]
  `(binding [*current-tvar-bnds* (extend-many *current-tvar-bnds* ~vars ~bndss)]
     ~@body))
