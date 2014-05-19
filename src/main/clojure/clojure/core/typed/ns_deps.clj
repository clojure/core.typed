(ns ^:skip-wiki clojure.core.typed.ns-deps
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.nilsafe-utils :as nilsafe]
            [clojure.set :as set]))

(alter-meta! *ns* assoc :skip-wiki true)

(t/defalias DepMap
  "A map declaring possibly-circular namespace dependencies."
  (t/Map t/Sym (t/Set t/Sym)))

(t/ann init-deps [-> DepMap])
(defn init-deps [] 
  {})

(t/ann *current-deps* (U nil (t/Atom1 DepMap)))
(defonce ^:dynamic *current-deps* nil)

(t/ann assert-dep-map [-> Any])
(defn assert-dep-map []
  (assert *current-deps* "No current namespace dependencies"))

(t/ann current-deps [-> (t/Atom1 DepMap)])
(defn current-deps []
  {:post [%]}
  (assert-dep-map)
  *current-deps*)

(t/ann ^:no-check dep-map? [Any -> Any])
(def dep-map? (con/hash-c? symbol? (con/set-c? symbol?)))

(t/ann ^:no-check CLJ-TYPED-DEPS (t/Atom1 DepMap))
(defonce CLJ-TYPED-DEPS (atom (init-deps) :validator dep-map?))

(t/ann ^:no-check CLJS-TYPED-DEPS (t/Atom1 DepMap))
(defonce CLJS-TYPED-DEPS (atom (init-deps) :validator dep-map?))

(t/ann ^:no-check add-ns-deps [t/Sym (t/Set t/Sym) -> DepMap])
(defn add-ns-deps [nsym deps]
  (assert-dep-map)
  (swap! (current-deps) update-in [nsym] nilsafe/set-union deps))

(t/ann ^:no-check remove-ns-deps [t/Sym (t/Set t/Sym) -> DepMap])
(defn remove-ns-deps [nsym deps]
  (assert-dep-map)
  (swap! (current-deps) update-in [nsym] nilsafe/set-difference deps))

(t/ann ^:no-check immediate-deps [t/Sym -> (t/Set t/Sym)])
(defn immediate-deps [target-ns]
  {:pre [(symbol? target-ns)]
   :post [((con/set-c? symbol?) %)]}
  (assert-dep-map)
  (or (@(current-deps) target-ns)
      #{}))

(t/ann reset-deps! [-> DepMap])
(defn reset-deps! []
  (assert-dep-map)
  (reset! (current-deps) (init-deps)))
