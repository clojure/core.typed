(ns ^:skip-wiki clojure.core.typed.object-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.object-protocols :refer [IRObject]]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t])
  (:import (clojure.lang Seqable)))

(t/def-alias RObject
  "An object with a path."
  IRObject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(t/ann ^:no-check RObject? (predicate IRObject))
(defn RObject? [a]
  (satisfies? IRObject a))

(t/ann ^:no-check declare-robject [Class -> Any])
(defn declare-robject [c]
  (extend c IRObject {}))

(t/ann-record EmptyObject [])
(u/defrecord EmptyObject []
  "?"
  [])

(t/ann -empty EmptyObject)
(def -empty (->EmptyObject))

(t/ann-record Path [path :- (Seqable IRObject)
                    id :- fr/NameRef])
(u/defrecord Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first (think of -> threading operator)."
  [(or (and (seq path)
            (sequential? path))
       (nil? path))
   (every? pr/PathElem? path)
   (fr/name-ref? id)])

(t/ann-record NoObject [])
(u/defrecord NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  [])

;Objects

(declare-robject EmptyObject)
(declare-robject Path)
(declare-robject NoObject)
