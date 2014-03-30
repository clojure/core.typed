(ns ^:skip-wiki 
  clojure.core.typed.chk.common.impl-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed :as t]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

;; Implementation Protocols and protocol predicates go here.
;;

;; Type protocols

(t/ann-protocol TCType)
(u/defprotocol TCType)

(t/ann-protocol TCAnyType)
(u/defprotocol TCAnyType)

(t/ann-protocol IScope
                scope-body
                [IScope -> (U TCType IScope)])
(u/defprotocol IScope
  (scope-body [this]))

(t/ann ^:no-check IScope? (predicate IScope))
(defn IScope? [a]
  (instance? clojure.core.typed.chk.common.impl_protocols.IScope a))

(t/ann-protocol IMu
                mu-scope
                [IMu -> IScope])
(u/defprotocol IMu
  (mu-scope [this]))

;; Filter protocols

(t/ann-protocol IFilter)
(u/defprotocol IFilter)

(t/ann ^:no-check IFilter? (predicate IFilter))
(defn IFilter? [a]
  (instance? clojure.core.typed.chk.common.impl_protocols.IFilter a))

(t/ann-protocol IFilterSet
                then-filter
                [IFilterSet -> IFilter]
                else-filter
                [IFilterSet -> IFilter])
(u/defprotocol IFilterSet
  (then-filter [this])
  (else-filter [this]))

(t/ann ^:no-check IFilterSet? (predicate IFilterSet))
(defn IFilterSet? [a]
  (instance? clojure.core.typed.chk.common.impl_protocols.IFilterSet a))

;; Object protocols

(t/ann-protocol IRObject)
(u/defprotocol IRObject)

(t/ann ^:no-check IRObject? (predicate IRObject))
(defn IRObject? [a]
  (instance? clojure.core.typed.chk.common.impl_protocols.IRObject a))

