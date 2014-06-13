(ns ^:skip-wiki clojure.core.typed.rclass-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.type-rep :as r]))

(alter-meta! *ns* assoc :skip-wiki true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

;Class -> RClass
(defonce RESTRICTED-CLASS (atom {}))
(set-validator! RESTRICTED-CLASS (con/hash-c? symbol? r/Type?))

(defn get-rclass 
  "Returns the RClass with class symbol csym.
  Returns nil if not found."
  [csym]
  (@RESTRICTED-CLASS csym))

(defn alter-class* [csym type]
  (assert (r/Type? type)
          (str "alter-class* " csym " not a type: " type))
  (swap! RESTRICTED-CLASS assoc csym type))

(defn reset-rclass-env! [m]
  (reset! RESTRICTED-CLASS m)
  nil)
