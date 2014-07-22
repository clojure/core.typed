(ns ^:skip-wiki clojure.core.typed.filter-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t]))

(alter-meta! *ns* assoc :skip-wiki true)
