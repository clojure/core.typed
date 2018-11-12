;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.checker.tvar-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed :as t])
  (:import (clojure.core.typed.checker.type_rep F)))

;; this implements the Delta environment from the TOPLAS paper
;; (as well as every other paper on System F)

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;;
;; The mapped-to type is used to distinguish type variables bound
;; at different scopes

(t/defalias TVarEnv
  "Map from scoped symbols to the actual free
  variables they represent"
  (t/Map t/Sym F))

(t/ann ^:no-check tvar-env? (t/Pred TVarEnv))
(def tvar-env? (con/hash-c? symbol? r/F?))

(t/ann initial-tvar-env TVarEnv)
(def initial-tvar-env {})

(t/ann ^:no-check *current-tvar* TVarEnv)
(defonce ^:dynamic *current-tvars* initial-tvar-env)
(t/tc-ignore
(set-validator! #'*current-tvars* tvar-env?)
  )

(defmacro with-extended-tvars
  "Takes a list of vars and extends the current tvar environment."
  [vars & body]
  `(binding [*current-tvars* (extend-many *current-tvars* ~vars)]
     ~@body))

(defmacro with-extended-new-tvars
  "Extends with new type variables (provided by (e.g., Poly-fresh))"
  [vars fresh-vars & body]
  `(binding [*current-tvars* (extend-many *current-tvars* ~vars ~fresh-vars)]
     ~@body))

(t/ann bound-tvar? [t/Sym -> Boolean])
(defn bound-tvar?
  "Returns true if the current type variable is bound"
  [var]
  (contains? *current-tvars* var))

(t/ann lookup-tvar [t/Sym -> (t/Nilable r/Type)])
(defn lookup-tvar
  "Returns the mapped-to type, or nil"
  [var]
  (*current-tvars* var))

(t/ann extend-one (t/IFn 
                    [TVarEnv t/Sym -> TVarEnv]
                    [TVarEnv t/Sym (t/Nilable t/Sym) -> TVarEnv]))
(defn extend-one
  "Extend a tvar environment. Adds an entry mapping var to itself,
  or if fresh-var is provided, mapping var to fresh-var"
  ([env var] (extend-one env var nil))
  ([env var fresh-var]
   {:pre [(tvar-env? env)
          (symbol? var)
          ((some-fn symbol? nil?) fresh-var)]
    :post [(tvar-env? %)]}
   (assoc env var (r/make-F (or fresh-var var)))))

(t/ann extend-many [TVarEnv (t/Coll t/Sym) (t/Nilable (t/Coll t/Sym)) -> TVarEnv])
(defn extend-many
  "Extends env with vars. If fresh-vars are provided, the vars will map to them
  pairwise in the resulting environment."
  ([env vars] (extend-many env vars nil))
  ([env vars fresh-vars]
   {:post [(tvar-env? %)]}
   (let [fresh-vars (or fresh-vars (repeat (count vars) nil))
         _ (assert (= (count vars) (count fresh-vars)))]
     (reduce (fn [env [var fresh-var]]
               {:pre [(symbol? var)
                      ((some-fn nil? symbol?) fresh-var)]}
               (extend-one env var fresh-var))
             env
             (map vector vars fresh-vars)))))
