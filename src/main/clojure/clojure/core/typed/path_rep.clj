(ns clojure.core.typed.path-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed
             [utils :as u]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(defprotocol IPathElem)

(defn PathElem? [a]
  (satisfies? IPathElem a))

(defn declare-path-elem [c]
  (extend c IPathElem {}))

(u/defrecord FirstPE []
  "A path calling clojure.core/first"
  [])
(u/defrecord NextPE []
  "A path calling clojure.core/next"
  [])

(u/defrecord ClassPE []
  "A path calling clojure.core/class"
  [])

(u/defrecord CountPE []
  "A path calling clojure.core/count"
  [])

(u/defrecord KeyPE [val]
  "A key in a hash-map"
  [(keyword? val)])

(def -kpe ->KeyPE)

(declare-path-elem FirstPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)

