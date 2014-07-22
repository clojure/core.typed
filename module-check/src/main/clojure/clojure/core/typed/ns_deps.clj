(ns ^:skip-wiki clojure.core.typed.ns-deps
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.nilsafe-utils :as nilsafe]
            [clojure.set :as set]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
)

(t/defalias DepMap
  "A map declaring possibly-circular namespace dependencies."
  (t/Map t/Sym (t/Set t/Sym)))

(t/ann init-deps [-> DepMap])
(defn init-deps [] 
  {})

(t/ann *current-deps* (t/U nil (t/Atom1 DepMap)))
(defonce ^:dynamic *current-deps* nil)

(t/ann assert-dep-map [-> (t/Atom1 DepMap)])
(defn assert-dep-map []
  (let [d *current-deps*]
    (assert d "No current namespace dependencies")
    d))

(t/ann current-deps [-> (t/Atom1 DepMap)])
(defn current-deps []
  {:post [%]}
  (let [d (assert-dep-map)]
    d))

(t/ann ^:no-check dep-map? [t/Any -> t/Any])
(def dep-map? (con/hash-c? symbol? (con/set-c? symbol?)))

(t/ann CLJ-TYPED-DEPS (t/Atom1 DepMap))
(defonce CLJ-TYPED-DEPS (atom (init-deps) :validator dep-map?))

(t/ann CLJS-TYPED-DEPS (t/Atom1 DepMap))
(defonce CLJS-TYPED-DEPS (atom (init-deps) :validator dep-map?))

(t/ann ^:no-check add-ns-deps [t/Sym (t/Set t/Sym) -> DepMap])
(defn add-ns-deps [nsym deps]
  {:pre [(symbol? nsym)
         ((con/set-c? symbol?) deps)]
   :post [(dep-map? %)]}
  (swap! (current-deps) update-in [nsym] nilsafe/set-union deps))

(t/ann ^:no-check remove-ns-deps [t/Sym (t/Set t/Sym) -> DepMap])
(defn remove-ns-deps [nsym deps]
  {:pre [(symbol? nsym)
         ((con/set-c? symbol?) deps)]
   :post [(dep-map? %)]}
  (swap! (current-deps) update-in [nsym] nilsafe/set-difference deps))

(t/ann immediate-deps [t/Sym -> (t/Set t/Sym)])
(defn immediate-deps [target-ns]
  {:pre [(symbol? target-ns)]
   :post [(t/tc-ignore
            ((con/set-c? symbol?) %))]}
  (or (@(current-deps) target-ns)
      #{}))

(t/ann reset-deps! [-> DepMap])
(defn reset-deps! []
  (reset! (current-deps) (init-deps)))

(t/ann typed-deps [t/Sym -> (t/Set t/Sym)])
(defn typed-deps [nsym]
  (let [deps (immediate-deps nsym)]
    (set (t/for [d :- t/Sym deps
                 :when (get (immediate-deps d) (impl/impl-case
                                                 :clojure 'clojure.core.typed
                                                 :cljs 'cljs.core.typed))]
           :- t/Sym
           d))))
