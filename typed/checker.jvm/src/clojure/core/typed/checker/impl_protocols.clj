;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki 
  clojure.core.typed.checker.impl-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.checker.def-utils :as u]
            [clojure.core.typed :as t]))

;; Implementation Protocols and protocol predicates go here.
;;

;; Type protocols

(t/ann-protocol TCType)
(u/defprotocol TCType)

(t/ann-protocol TCAnyType)
(u/defprotocol TCAnyType)

(t/ann-protocol IScope
                scope-body
                [IScope -> (t/U TCType IScope)])
(u/defprotocol IScope
  (scope-body [this]))

(t/ann ^:no-check IScope? (t/Pred IScope))
(defn IScope? [a]
  (instance? clojure.core.typed.checker.impl_protocols.IScope a))

(t/ann-protocol IMu
                mu-scope
                [IMu -> IScope])
(u/defprotocol IMu
  (mu-scope [this]))

;; Filter protocols

(t/ann-protocol IFilter)
(u/defprotocol IFilter)

(t/ann ^:no-check IFilter? (t/Pred IFilter))
(defn IFilter? [a]
  (instance? clojure.core.typed.checker.impl_protocols.IFilter a))

(t/ann-protocol IFilterSet
                then-filter
                [IFilterSet -> IFilter]
                else-filter
                [IFilterSet -> IFilter])
(u/defprotocol IFilterSet
  (then-filter [this])
  (else-filter [this]))

(t/ann ^:no-check IFilterSet? (t/Pred IFilterSet))
(defn IFilterSet? [a]
  (instance? clojure.core.typed.checker.impl_protocols.IFilterSet a))

;; Object protocols

(t/ann-protocol IRObject)
(u/defprotocol IRObject)

(t/ann ^:no-check IRObject? (t/Pred IRObject))
(defn IRObject? [a]
  (instance? clojure.core.typed.checker.impl_protocols.IRObject a))
