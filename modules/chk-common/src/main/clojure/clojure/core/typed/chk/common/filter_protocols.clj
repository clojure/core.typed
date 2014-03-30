(ns ^:skip-wiki clojure.core.typed.chk.common.filter-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed :as t]))

(alter-meta! *ns* assoc :skip-wiki true)
