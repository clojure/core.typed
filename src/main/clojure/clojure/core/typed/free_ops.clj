(ns ^:skip-wiki 
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.free-ops
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed :as t :refer [fn>]])
  (:import (clojure.core.typed.type_rep F Bounds)
           (clojure.lang Symbol)))

(alter-meta! *ns* assoc :skip-wiki true
             :core.typed {:collect-only true})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

(t/def-alias FreeEntry
  "The right hand side of the free-scope map.
  Includes the F and the bound."
  '{:F F :bnds Bounds})

;(Map Symbol F)
(t/ann *free-scope* (t/Map Symbol FreeEntry))
(defonce ^:dynamic *free-scope* {})
(set-validator! #'*free-scope* (u/hash-c? symbol? (u/hmap-c? :F r/F? :bnds r/Bounds?)))

(t/ann free-with-name [Symbol -> (U nil F)])
(defn free-with-name 
  "Find the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (some (fn> [[_ {{fname :name :as f} :F}] :- '[Symbol FreeEntry]]
          (t/ann-form fname Symbol)
          (when (= name fname)
            f))
        *free-scope*))

(t/ann free-with-name-bnds [Symbol -> (U nil Bounds)])
(defn ^Bounds
  free-with-name-bnds 
  "Find the bounds for the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (some (fn> [[_ {{fname :name} :F :keys [bnds]}] :- '[Symbol FreeEntry]]
          (when (= name fname)
            bnds))
        *free-scope*))

(t/ann free-in-scope [Symbol -> (U nil F)])
(defn free-in-scope 
  "Find the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (when-let [entry (*free-scope* name)]
    (:F entry)))

(t/ann free-in-scope-bnds [Symbol -> (U nil Bounds)])
(defn free-in-scope-bnds 
  "Find the bounds for the free scoped as name"
  ^Bounds
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (when-let [entry (*free-scope* name)]
    (:bnds entry)))

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
