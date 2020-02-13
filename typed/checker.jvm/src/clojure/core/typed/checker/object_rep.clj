;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.object-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.impl-protocols :as p]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.path-rep :as pr]
            [clojure.core.typed.checker.filter-rep :as fr]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.indirect-utils :as ind-u]
            [clojure.core.typed.checker.indirect-ops :as ind]))

(t/defalias RObject
  "An object with a path."
  p/IRObject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(t/ann ^:no-check RObject? (t/Pred p/IRObject))
(defn RObject? [a]
  (instance? clojure.core.typed.checker.impl_protocols.IRObject a))

(t/ann-record EmptyObject [])
(u/def-object EmptyObject []
  "No interesting information about this path/object"
  []
  :methods
  [p/IRObject])

(t/ann ^:no-check -empty EmptyObject)
(def -empty (EmptyObject-maker))

(defn -empty-fn []
  -empty)

(t/tc-ignore
(ind-u/add-indirection ind/-empty-fn -empty-fn)
)

(t/ann-record Path [path :- (t/Seqable p/IRObject)
                    id :- fr/NameRef])
(u/def-object Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first (think of -> threading operator)."
  [(pr/path-elems? path)
   (fr/name-ref? id)]
  :methods
  [p/IRObject])

(t/ann ^:no-check -path [(t/Seqable p/IRObject) fr/NameRef -> Path])
(defn -path [path id]
  {:pre [(pr/path-elems? path)
         (fr/name-ref? id)]
   :post [(Path? %)]}
  (Path-maker path id))

(defn last-path-elem [o]
  {:pre [(Path? o)]
   :post [((some-fn nil? pr/PathElem?) %)]}
  (last (:path o)))

(defn without-final-elem [o]
  {:pre [(Path? o)]
   :post [(Path? %)]}
  (update-in o [:path] (comp seq butlast)))

(t/ann-record NoObject [])
(u/def-object NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  []
  :methods
  [p/IRObject])

(def -no-object (NoObject-maker))

(t/ann -id-path [fr/NameRef -> RObject])
(defn -id-path [sym]
  {:pre [(fr/name-ref? sym)]
   :post [(RObject? %)]}
  (-path nil sym))
