(ns ^:skip-wiki clojure.core.typed.object-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t]))

(t/ann-protocol IRObject)
(u/defprotocol IRObject)
