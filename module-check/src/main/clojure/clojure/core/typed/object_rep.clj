(ns ^:skip-wiki clojure.core.typed.object-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed.impl-protocols :as p]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.indirect-utils :as ind-u]
            [clojure.core.typed.indirect-ops :as ind]
            [clojure.core.typed.contract-utils :as con]
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

(t/ann-record Path [path :- (Seqable p/IRObject)
                    id :- fr/NameRef])
(u/def-object Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first (think of -> threading operator)."
  [(pr/path-elems? path)
   (fr/name-ref? id)]
  :methods
  [p/IRObject])

(t/ann ^:no-check -path [(Seqable p/IRObject) fr/NameRef -> Path])
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
