(ns ^:skip-wiki clojure.core.typed.object-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t])
  (:import (clojure.lang Seqable)))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

(t/defalias RObject
  "An object with a path."
  p/IRObject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(t/ann ^:no-check RObject? (t/Pred p/IRObject))
(defn RObject? [a]
  (instance? clojure.core.typed.impl_protocols.IRObject a))

(t/ann-record EmptyObject [])
(u/defrecord EmptyObject []
  "No interesting information about this path/object"
  []
  p/IRObject)

(t/ann -empty EmptyObject)
(def -empty (->EmptyObject))

(t/ann-record Path [path :- (Seqable p/IRObject)
                    id :- fr/NameRef])
(u/defrecord Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first (think of -> threading operator)."
  [(or (and (seq path)
            (sequential? path))
       (nil? path))
   (every? pr/PathElem? path)
   (fr/name-ref? id)]
  p/IRObject)

(t/ann-record NoObject [])
(u/defrecord NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  []
  p/IRObject)
