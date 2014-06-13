(ns ^:skip-wiki clojure.core.typed.reset-caches
  (:require [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.cs-gen :as cgen]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn reset-caches 
  "Reset internal type caches."
  []
  (sub/reset-subtype-cache)
  (c/reset-Un-cache)
  (c/reset-In-cache)
  (c/reset-supers-cache!)
  (c/reset-RClass-of-cache!)
  (cgen/reset-dotted-var-store!)
  nil)
