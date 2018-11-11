;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.ns-deps
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.nilsafe-utils :as nilsafe]
            [clojure.core.typed.env :as env]))

(t/defalias DepMap
  "A map declaring possibly-circular namespace dependencies."
  (t/Map t/Sym (t/Set t/Sym)))

(t/ann ^:no-check dep-map? [t/Any -> t/Any])
(def dep-map? (con/hash-c? symbol? (con/set-c? symbol?)))

(t/ann init-deps [-> DepMap])
(defn init-deps [] 
  {})

(t/ann ^:no-check deps [-> DepMap])
(defn deps []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/current-deps-kw {}))

(t/ann ^:no-check add-ns-deps [t/Sym (t/Set t/Sym) -> DepMap])
(def add-ns-deps impl/add-ns-deps)

(t/ann ^:no-check remove-ns-deps [t/Sym (t/Set t/Sym) -> DepMap])
(defn remove-ns-deps [nsym deps]
  {:pre [(symbol? nsym)
         ((con/set-c? symbol?) deps)]
   :post [(nil? %)]}
  (env/swap-checker! update-in [impl/current-deps-kw nsym] nilsafe/set-difference deps)
  nil)

(t/ann ^:no-check immediate-deps [t/Sym -> (t/Set t/Sym)])
(defn immediate-deps [target-ns]
  {:pre [(symbol? target-ns)]
   :post [((con/set-c? symbol?) %)]}
  (get (deps) target-ns #{}))

(t/ann ^:no-check reset-deps! [-> DepMap])
(defn reset-deps! []
  (env/swap-checker! assoc impl/current-deps-kw (init-deps)))

(t/ann typed-deps [t/Sym -> (t/Set t/Sym)])
(defn typed-deps [nsym]
  (let [deps (immediate-deps nsym)]
    (set (t/for [d :- t/Sym deps
                 :when (get (immediate-deps d) (impl/impl-case
                                                 :clojure 'clojure.core.typed
                                                 :cljs 'cljs.core.typed))]
           :- t/Sym
           d))))
