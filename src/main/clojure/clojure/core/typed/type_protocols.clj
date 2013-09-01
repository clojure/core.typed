(ns ^:skip-wiki 
  clojure.core.typed.type-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t]))

(alter-meta! *ns* assoc :skip-wiki true)

(t/ann-protocol TCType)
(u/defprotocol TCType)

(t/ann-protocol TCAnyType)
(u/defprotocol TCAnyType)

(t/ann-protocol IScope
                scope-body
                [IScope -> (U TCType IScope)])
(u/defprotocol IScope
  (scope-body [this]))
