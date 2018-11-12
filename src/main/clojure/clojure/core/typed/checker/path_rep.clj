;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.path-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed :as t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(t/ann-protocol IPathElem)
(u/defprotocol IPathElem)

(t/ann ^:no-check PathElem? (t/Pred IPathElem))
(defn PathElem? [a]
  (satisfies? IPathElem a))

(t/ann ^:no-check declare-path-elem [Class -> t/Any])
(defn declare-path-elem [c]
  (extend c IPathElem {}))

(t/ann-record NthPE [idx :- Number]) ;; More specific?
(u/def-object NthPE [idx]
  "A path accessing an indexed member, as by clojure.core/first, second, nth"
  [(integer? idx)
   (not (neg? idx))])

(t/ann-record NextPE [])
(u/def-object NextPE []
  "A path calling clojure.core/next"
  [])

(t/ann-record ClassPE [])
(u/def-object ClassPE []
  "A path calling clojure.core/class"
  [])

(t/ann-record CountPE [])
(u/def-object CountPE []
  "A path calling clojure.core/count"
  [])

(t/ann-record KeyPE [val :- t/Kw])
(u/def-object KeyPE [val]
  "A key in a hash-map"
  [(keyword? val)])

(t/ann ^:no-check -kpe [t/Kw -> KeyPE])
(def -kpe KeyPE-maker)

(t/ann-record KeysPE [])
(u/def-object KeysPE []
  "Calling clojure.core/keys"
  [])

(t/ann-record ValsPE [])
(u/def-object ValsPE []
  "Calling clojure.core/vals"
  [])

(t/ann-record KeywordPE [])
(u/def-object KeywordPE []
  "Calling clojure.core/keyword with a single argument."
  [])

(declare-path-elem NthPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)
(declare-path-elem KeysPE)
(declare-path-elem ValsPE)
(declare-path-elem KeywordPE)

(def path-elems? (every-pred (some-fn nil? seq)
                             (con/every-c? PathElem?)))
