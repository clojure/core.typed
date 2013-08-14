(ns ^:skip-wiki clojure.core.typed.ns-deps
  (:require [clojure.core.typed :as t :refer [fn>]]
            [clojure.core.typed.utils :as u]
            [clojure.set :as set])
  (:import (clojure.lang IPersistentMap Symbol IPersistentSet)))

(t/def-alias DepMap
  "A map declaring possibly-circular namespace dependencies."
  (IPersistentMap Symbol (IPersistentSet Symbol)))

(t/ann init-deps [-> DepMap])
(defn init-deps [] 
  {})

(t/ann *current-deps* (U nil (t/Atom1 DepMap)))
(def ^:dynamic *current-deps* nil)

(t/ann assert-dep-map [-> Any])
(defn assert-dep-map []
  (assert *current-deps* "No current namespace dependencies"))

(t/ann current-deps [-> (t/Atom1 DepMap)])
(defn current-deps []
  {:post [%]}
  (assert-dep-map)
  *current-deps*)

(t/ann ^:no-check dep-map? [Any -> Any])
(def dep-map? (u/hash-c? symbol? (u/set-c? symbol?)))

(t/ann ^:no-check CLJ-TYPED-DEPS (t/Atom1 DepMap))
(defonce CLJ-TYPED-DEPS (atom (init-deps) :validator dep-map?))

(t/ann ^:no-check CLJS-TYPED-DEPS (t/Atom1 DepMap))
(defonce CLJS-TYPED-DEPS (atom (init-deps) :validator dep-map?))

(t/ann ^:no-check add-ns-deps [Symbol (IPersistentSet Symbol) -> DepMap])
(defn add-ns-deps [nsym deps]
  (assert-dep-map)
  (swap! (current-deps) update-in [nsym] u/set-union deps))

(t/ann ^:no-check remove-ns-deps [Symbol (IPersistentSet Symbol) -> DepMap])
(defn remove-ns-deps [nsym deps]
  (assert-dep-map)
  (swap! (current-deps) update-in [nsym] u/set-difference deps))

(t/ann ^:no-check immediate-deps [Symbol -> (IPersistentSet Symbol)])
(defn immediate-deps [target-ns]
  {:pre [(symbol? target-ns)]
   :post [((u/set-c? symbol?) %)]}
  (assert-dep-map)
  (or (@(current-deps) target-ns)
      #{}))

(t/ann reset-deps! [-> DepMap])
(defn reset-deps! []
  (assert-dep-map)
  (reset! (current-deps) (init-deps)))
