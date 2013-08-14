(ns ^:skip-wiki clojure.core.typed.jsnominal-env
  (:refer-clojure :exclude [get-method])
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSNominal

(defonce JSNOMINAL-ENV 
  (atom {} 
        :validator
        (u/hash-c? symbol? 
                   (u/hmap-c? :jsnominal  r/Type?
                              :fields (u/hash-c? symbol? r/Type?)
                              :methods (u/hash-c? symbol? r/Type?)))))

(defn get-jsnominal
  "Returns the nomainal JS type with class symbol csym.
  Returns nil if not found."
  [csym]
  (-> (@JSNOMINAL-ENV csym) :jsnominal))

(defn add-jsnominal [csym type]
  (assert (r/Type? type)
          (str "JS nominal" csym " not a type: " type))
  (swap! JSNOMINAL-ENV assoc-in [csym :jsnominal] type))

(defn add-method [csym method-sym type]
  (swap! JSNOMINAL-ENV update-in [csym :fields method-sym] (constantly type)))

(defn add-field [csym field-sym type]
  (swap! JSNOMINAL-ENV update-in [csym :fields field-sym] (constantly type)))

(defn get-method [csym method-sym]
  (get-in @JSNOMINAL-ENV [csym :methods method-sym]))

(defn get-field [csym field-sym]
  (get-in @JSNOMINAL-ENV [csym :fields field-sym]))

(defn reset-jsnominal! [m]
  (reset! JSNOMINAL-ENV m)
  nil)
