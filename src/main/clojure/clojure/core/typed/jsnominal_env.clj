(ns clojure.core.typed.jsnominal-env
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSNominal

(defonce JSNOMINAL-ENV (atom {}))
(set-validator! JSNOMINAL-ENV (u/hash-c? symbol? r/Type?))

(defn get-jsnominal
  "Returns the nomainal JS type with class symbol csym.
  Returns nil if not found."
  [csym]
  (@JSNOMINAL-ENV csym))

(defn add-jsnominal [csym type]
  (assert (r/Type? type)
          (str "JS nominal" csym " not a type: " type))
  (swap! JSNOMINAL-ENV assoc csym type))

(defn reset-jsnominal! [m]
  (reset! JSNOMINAL-ENV m)
  nil)
