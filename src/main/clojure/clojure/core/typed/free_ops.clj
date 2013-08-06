(ns clojure.core.typed.free-ops
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r])
  (:import (clojure.core.typed.type_rep Bounds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

;(Map Symbol F)
(def ^:dynamic *free-scope* {})
(set-validator! #'*free-scope* 
                (fn [a]
                  (when-not ((u/hash-c? symbol? (u/hmap-c? :F r/F? :bnds r/Bounds?))
                             a)
                    (throw (IllegalStateException. (str "Validator violation clojure.core.typed.free-ops/*free-ops*."
                                                        " Given: " a))))
                  true))

(defn free-with-name 
  "Find the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (some (fn [[_ {{fname :name :as f} :F}]]
          (when (= name fname)
            f))
        *free-scope*))

(defn ^Bounds
  free-with-name-bnds 
  "Find the bounds for the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (some (fn [[_ {{fname :name} :F :keys [bnds]}]]
          (when (= name fname)
            bnds))
        *free-scope*))

(defn free-in-scope 
  "Find the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (:F (*free-scope* name)))

(defn free-in-scope-bnds 
  "Find the bounds for the free scoped as name"
  ^Bounds
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (:bnds (*free-scope* name)))

(defmacro with-free-mappings 
  [frees-map & body]
  `(binding [*free-scope* (merge *free-scope* ~frees-map)]
     ~@body))

(defmacro with-bounded-frees 
  "Scopes bfrees, a map of instances of F to their bounds, inside body."
  [bfrees & body]
  `(with-free-mappings (into {} (for [[f# bnds#] ~bfrees]
                                  [(:name f#) {:F f# :bnds bnds#}]))
     ~@body))

(defmacro with-frees 
  "Scopes frees, which are instances of F, inside body, with
  default bounds."
  [frees & body]
  `(with-free-mappings (into {} (for [f# ~frees]
                                  [(:name f#) {:F f# :bnds r/no-bounds}]))
     ~@body))

(defmacro with-free-symbols 
  "Scopes sfrees, a sequence of symbols, inside body as free variables, with default bounds."
  [sfrees & body]
  `(with-free-mappings (into {} (for [f# ~sfrees]
                                  [f# {:F (r/F-maker f#) :bnds r/no-bounds}]))
     ~@body))
