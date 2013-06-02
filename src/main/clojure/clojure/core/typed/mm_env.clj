(ns clojure.core.typed.mm-env
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]
             [parse-unparse :as prs]]))

;; Environment for storing multimethod types and inferred filters

; (Atom (Seqable (IPersistentMap Symbol '{:fn-type Type, :dispatch-result (U nil Type)})))
(defonce MULTIMETHOD-DISPATCH-ENV (atom {}
                                        :validator (u/hash-c?
                                                     (every-pred symbol? namespace)
                                                     r/Type?)))

; [Symbol Filter -> nil]
(defn add-multimethod-dispatch-type
  "Add the type of the dispatch function of the multimethod named by mmsym
  to the environment. If already exists, must be identical."
  [mmsym dtype]
  {:pre [(symbol? mmsym)
         (r/Type? dtype)]}
  (when-let [old (@MULTIMETHOD-DISPATCH-ENV mmsym)]
    (assert (= old dtype)
            (str "Cannot assign multimethod a different dispatch result: "
                 " Old: " (prs/unparse-type old)
                 " New: " (prs/unparse-type dtype))))
  (swap! MULTIMETHOD-DISPATCH-ENV assoc mmsym dtype)
  nil)

(defn multimethod-dispatch-type 
  "Can return nil"
  [mmsym]
  {:pre [(symbol? mmsym)]
   :post [((some-fn nil? r/Type?) %)]}
  (@MULTIMETHOD-DISPATCH-ENV mmsym))

(defn get-multimethod-dispatch-type [mmsym]
  {:pre [(symbol? mmsym)]
   :post [(r/Type? %)]}
  (let [t (@MULTIMETHOD-DISPATCH-ENV mmsym)]
    (assert t (str "Multimethod requires dispatch type: " mmsym))
    t))
