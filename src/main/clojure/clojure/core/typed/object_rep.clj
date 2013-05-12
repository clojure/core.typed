(ns clojure.core.typed.object-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed
             [type-rep :as r]
             [path-rep :as pr]
             [filter-rep :as fr]
             [utils :as u]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(def RObject ::r-object)
(defprotocol IRObject)

(defn RObject? [a]
  (satisfies? IRObject a))

(defn declare-robject [c]
  (extend c IRObject {}))

(u/defrecord EmptyObject []
  "?"
  [])

(def -empty (->EmptyObject))

(u/defrecord Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first (think of -> threading operator)."
  [(or (and (seq path)
            (sequential? path))
       (nil? path))
   (every? pr/PathElem? path)
   (fr/name-ref? id)])

(u/defrecord NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  [])

;Objects

(declare-robject EmptyObject)
(declare-robject Path)
(declare-robject NoObject)
