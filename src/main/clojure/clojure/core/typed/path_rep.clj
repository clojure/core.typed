(ns clojure.core.typed.path-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed
             [utils :as u]]
            [clojure.core.typed :as t])
  (:import (clojure.lang Keyword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(t/ann-protocol IPathElem)
(u/defprotocol IPathElem)

(t/ann ^:no-check PathElem? (predicate IPathElem))
(defn PathElem? [a]
  (satisfies? IPathElem a))

(t/ann ^:no-check declare-path-elem [Class -> Any])
(defn declare-path-elem [c]
  (extend c IPathElem {}))

(t/ann-record FirstPE [])
(u/defrecord FirstPE []
  "A path calling clojure.core/first"
  [])

(t/ann-record NextPE [])
(u/defrecord NextPE []
  "A path calling clojure.core/next"
  [])

(t/ann-record ClassPE [])
(u/defrecord ClassPE []
  "A path calling clojure.core/class"
  [])

(t/ann-record CountPE [])
(u/defrecord CountPE []
  "A path calling clojure.core/count"
  [])

(t/ann-record KeyPE [val :- Keyword])
(u/defrecord KeyPE [val]
  "A key in a hash-map"
  [(keyword? val)])

(t/ann -kpe [Keyword -> KeyPE])
(def -kpe ->KeyPE)

(t/ann-record KeysPE [])
(u/defrecord KeysPE []
  "Calling clojure.core/keys"
  [])

(t/ann-record ValsPE [])
(u/defrecord ValsPE []
  "Calling clojure.core/vals"
  [])

(declare-path-elem FirstPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)
(declare-path-elem KeysPE)
(declare-path-elem ValsPE)

