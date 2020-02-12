;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.mm-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-rep :as r]))

;; Environment for storing multimethod types and inferred filters

(def initial-mm-dispatch-env {})

; (Atom (Seqable (IPersistentMap Symbol '{:fn-type Type, :dispatch-result (U nil Type)})))
(defonce MULTIMETHOD-DISPATCH-ENV (atom initial-mm-dispatch-env
                                        :validator (con/hash-c?
                                                     (every-pred symbol? namespace)
                                                     r/Type?)))

(defn reset-mm-dispatch-env! []
  (reset! MULTIMETHOD-DISPATCH-ENV initial-mm-dispatch-env)
  nil)

; [Symbol Filter -> nil]
(defn add-multimethod-dispatch-type
  "Add the type of the dispatch function of the multimethod named by mmsym
  to the environment. If already exists, must be identical."
  [mmsym dtype]
  {:pre [(symbol? mmsym)
         (r/Type? dtype)]}
  (impl/assert-clojure)
  (when-let [old (@MULTIMETHOD-DISPATCH-ENV mmsym)]
    (when-not (= old dtype)
      (err/int-error 
        (str "Inconsistent dispatch type inferred for multimethod: " mmsym
             ".  JVM process restart probably necessary."))))
  (swap! MULTIMETHOD-DISPATCH-ENV assoc mmsym dtype)
  nil)

(defn multimethod-dispatch-type 
  "Can return nil"
  [mmsym]
  {:pre [(symbol? mmsym)]
   :post [((some-fn nil? r/Type?) %)]}
  (impl/assert-clojure)
  (@MULTIMETHOD-DISPATCH-ENV mmsym))

(defn get-multimethod-dispatch-type [mmsym]
  {:pre [(symbol? mmsym)]
   :post [(r/Type? %)]}
  (impl/assert-clojure)
  (let [t (@MULTIMETHOD-DISPATCH-ENV mmsym)]
    (when-not t 
      (err/int-error (str "Multimethod requires dispatch type: " mmsym)))
    t))
