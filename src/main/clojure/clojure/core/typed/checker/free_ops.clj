;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.checker.free-ops
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed :as t]
            [clojure.core.typed.checker.tvar-env :as tvar]
            [clojure.core.typed.checker.tvar-bnds :as bnds])
  (:import (clojure.core.typed.checker.type_rep F Bounds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

;(t/ann free-with-name [t/Sym -> (t/U nil F)])
;(defn free-with-name
;  "Find the free with the actual name name, as opposed to
;  the alias used for scoping"
;  [name]
;  {:pre [(symbol? name)]
;   :post [((some-fn nil? r/F?) %)]}
;  (some (fn> [[_ {{fname :name :as f} :F}] :- '[t/Sym FreeEntry]]
;          (t/ann-form fname t/Sym)
;          (when (= name fname)
;            f))
;        *free-scope*))

(t/ann free-with-name-bnds [t/Sym -> (t/U nil Bounds)])
(defn ^Bounds
  free-with-name-bnds
  "Find the bounds for the free with the actual name name, as opposed to
  the alias used for scoping"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (bnds/lookup-tvar-bnds name))

(t/ann free-in-scope [t/Sym -> (t/U nil F)])
(defn free-in-scope
  "Find the free scoped as name"
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/F?) %)]}
  (tvar/*current-tvars* name))

(t/ann free-in-scope-bnds [t/Sym -> (t/U nil Bounds)])
(defn free-in-scope-bnds
  "Find the bounds for the free scoped as name"
  ^Bounds
  [name]
  {:pre [(symbol? name)]
   :post [((some-fn nil? r/Bounds?) %)]}
  (when-let [f (free-in-scope name)]
    (bnds/lookup-tvar-bnds (:name f))))

(def frees-map? (con/hash-c? symbol? (con/hmap-c? :F r/F? :bnds r/Bounds?)))

; we used to have scopes and bounds in the same map. To avoid changing the interface,
; with-free-mappings now handles frees-map to do scoping and bounds in separate bindings.
;
; Once this works we should use a more consistent interface
;
;frees-map :- '{t/Sym '{:F F :bnds Bounds}}
(defmacro with-free-mappings
  [frees-map & body]
  `(let [frees-map# ~frees-map
         _# (assert (frees-map? frees-map#)
                    frees-map#)
         scoped-names# (keys frees-map#)
         fresh-names# (map (comp :name :F) (vals frees-map#))
         bndss# (map :bnds (vals frees-map#))]
     (tvar/with-extended-new-tvars scoped-names# fresh-names#
       (bnds/with-extended-bnds fresh-names# bndss#
         ~@body))))

(def bounded-frees? (con/hash-c? r/F? r/Bounds?))

(defn with-bounded-frees* [bfrees bfn]
  (let [_ (assert (bounded-frees? bfrees) bfrees)]
    (with-free-mappings (into {} (for [[f bnds] bfrees]
                                   [(:name f) {:F f :bnds bnds}]))
      (bfn))))

(defmacro with-bounded-frees
  "Scopes bfrees, a map of instances of F to their bounds, inside body."
  [bfrees & body]
  `(with-bounded-frees* ~bfrees (fn [] (do ~@body))))

(defn with-frees* [frees bfn]
  (with-free-mappings (into {} (for [f frees]
                                 [(:name f) {:F f :bnds r/no-bounds}]))
    (bfn)))

(defmacro with-frees
  "Scopes frees, which are instances of F, inside body, with
  default bounds."
  [frees & body]
  `(with-frees* ~frees
     (fn [] (do ~@body))))

(defmacro with-free-symbols
  "Scopes sfrees, a sequence of symbols, inside body as free variables, with default bounds."
  [sfrees & body]
  `(with-free-mappings (into {} (for [f# ~sfrees]
                                  [f# {:F (r/F-maker f#) :bnds r/no-bounds}]))
     ~@body))
