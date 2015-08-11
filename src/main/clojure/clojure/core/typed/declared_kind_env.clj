(ns clojure.core.typed.declared-kind-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.env :as env]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declared kind Env

(def declared-kind-env? (con/hash-c? symbol? r/TypeFn?))

(def current-declared-kinds-kw ::current-declared-kinds)

(defn declared-kinds []
  {:post [(map? %)]}
  (get (env/deref-checker) current-declared-kinds-kw {}))

(defn reset-declared-kinds! [m]
  {:pre [(declared-kind-env? m)]
   :post [(nil? %)]}
  (env/swap-checker! assoc current-declared-kinds-kw m)
  nil)

(defn add-declared-kind [sym tfn]
  {:pre [(symbol? sym)
         (r/TypeFn? tfn)]
   :post [(nil? %)]}
  (env/swap-checker! assoc-in [current-declared-kinds-kw sym] tfn)
  nil)

(defn declared-kind-or-nil [sym]
  (get (declared-kinds) sym))

(defn get-declared-kind [sym]
  (if-let [tfn (declared-kind-or-nil sym)]
    tfn
    (err/int-error (str "No declared kind for Name " sym))))

(defn has-declared-kind? [sym]
  (boolean (declared-kind-or-nil sym)))

(defn remove-declared-kind [sym]
  (env/swap-checker! update current-declared-kinds-kw dissoc sym)
  nil)

(defn declare-alias-kind* [sym ty]
  (add-declared-kind sym ty))
