(ns clojure.core.typed.free-ops
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]])
  (:import (clojure.core.typed.type_rep Bounds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

;(Map Symbol F)
(def ^:dynamic *free-scope* {})
(set-validator! #'*free-scope* (u/hash-c? symbol? (u/hmap-c? :F r/F? :bnds r/Bounds?)))

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

(defmacro with-free-mappings [frees-map & body]
  `(binding [*free-scope* (merge *free-scope* ~frees-map)]
     ~@body))

(defmacro with-bounded-frees [bfrees & body]
  `(with-free-mappings (into {} (for [[f# bnds#] ~bfrees]
                                  [(:name f#) {:F f# :bnds bnds#}]))
     ~@body))

(defmacro with-frees [frees & body]
  `(with-free-mappings (into {} (for [f# ~frees]
                                  [(:name f#) {:F f# :bnds r/no-bounds}]))
     ~@body))
