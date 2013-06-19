(ns clojure.core.typed.ns-deps
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

(t/ann ^:nocheck TYPED-DEPS (t/Atom1 DepMap))
(defonce TYPED-DEPS (atom (init-deps)
                          :validator (u/hash-c? symbol? (u/set-c? symbol?))))

(t/ann ^:nocheck add-ns-deps [Symbol (IPersistentSet Symbol) -> DepMap])
(defn add-ns-deps [nsym deps]
  (swap! TYPED-DEPS update-in [nsym] u/set-union deps))

(t/ann ^:nocheck remove-ns-deps [Symbol (IPersistentSet Symbol) -> DepMap])
(defn remove-ns-deps [nsym deps]
  (swap! TYPED-DEPS update-in [nsym] u/set-difference deps))

(t/ann ^:nocheck immediate-deps [Symbol -> (IPersistentSet Symbol)])
(defn immediate-deps [target-ns]
  {:pre [(symbol? target-ns)]
   :post [(u/set-c? Symbol)]}
  (or (@TYPED-DEPS target-ns)
      #{}))

(t/ann reset-deps! [-> DepMap])
(defn reset-deps! []
  (reset! TYPED-DEPS (init-deps)))
