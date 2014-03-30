(ns ^:skip-wiki clojure.core.typed.impl.js.jsnominal-env
  (:refer-clojure :exclude [get-method])
  (:require [clojure.core.typed.chk.common.type-rep :as r]
            [clojure.core.typed.chk.common.type-ctors :as c]
            [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed :as t :refer [ann def-alias]])
  (:import [clojure.core.typed.chk.common.type_rep Scope]
           [clojure.lang Symbol]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSNominal

(def-alias JSNominalEntry
  "A map entry for a JSNominal type."
  '{:jsnominal r/Type
    :fields (t/Map Symbol (U Scope r/Type))
    :methods (t/Map Symbol (U Scope r/Type))
    :ctors (U Scope r/Type nil)
    :ancestors (t/Set (U Scope r/Type))})

(def-alias JSNominalEnv
  "A map of symbols of JSNomainalEntry's"
  (t/Map Symbol JSNominalEntry))

(ann JSNOMINAL-ENV (t/Atom1 JSNominalEnv))
(defonce JSNOMINAL-ENV 
  (atom {} 
        :validator
        (u/hash-c? symbol? 
                   (u/hmap-c? :jsnominal  r/Type?
                              :fields (u/hash-c? symbol? (some-fn r/Scope? r/Type?))
                              :methods (u/hash-c? symbol? (some-fn r/Scope? r/Type?))
                              :ctor (some-fn nil? r/Scope? r/Type?)
                              :ancestors (u/set-c? (some-fn r/Scope? r/Type?))))))

(ann init-jsnominal-entry [r/Type -> JSNominalEntry])
(defn init-jsnominal-entry [nom]
  {:jsnominal nom
   :fields {}
   :methods {}
   :ctors nil
   :ancestors #{}})

(ann get-jsnominal [Any -> (t/Nilable r/Type)])
(defn get-jsnominal
  "Returns the nomainal JS type with class symbol csym.
  Returns nil if not found."
  [csym]
  (-> (@JSNOMINAL-ENV csym) :jsnominal))

(ann contains-jsnominal? [Any -> boolean])
(defn contains-jsnominal?
  [csym]
  (boolean (get-jsnominal csym)))

;(ann add-jsnominal [Symbol r/Type -> nil])
;(defn add-jsnominal [csym type]
;  (assert (r/Type? type)
;          (str "JS nominal" csym " not a type: " type))
;  ; remove old fields etc.
;  (swap! JSNOMINAL-ENV assoc-in [csym] (init-jsnominal-entry type))
;  nil)
;
;(ann add-method [Symbol JSNominal (U Scope r/Type) -> nil])
;(defn add-method 
;  "Add a new method to the JS nominal type csym. Assumes
;  the method type is properly Scope'd"
;  [csym method-sym type]
;  (swap! JSNOMINAL-ENV assoc-in [csym :methods method-sym] type)
;  nil)
;
;(ann add-field [Symbol JSNominal (U Scope r/Type) -> nil])
;(defn add-field 
;  "Add a new field to the JS nominal type csym. Assumes
;  the field type is properly Scope'd"
;  [csym field-sym type]
;  (swap! JSNOMINAL-ENV update-in [csym :fields field-sym] (constantly type)))

(ann ^:no-check get-method [Symbol (U nil (t/Seqable r/Type)) Symbol -> (U nil r/Type)])
(defn get-method 
  "Returns the instantiated method type named method-sym on nominal csym."
  [csym args method-sym]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? method-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in @JSNOMINAL-ENV [csym :methods method-sym])]
    (c/inst-and-subst tscope args)))

(ann ^:no-check get-field [Symbol (U nil (t/Seqable r/Type)) Symbol -> (U nil r/Type)])
(defn get-field 
  "Returns the instantiated field type named field-sym on nominal csym."
  [csym args field-sym]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? field-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in @JSNOMINAL-ENV [csym :fields field-sym])]
    (c/inst-and-subst tscope args)))

(ann ^:no-check get-ctor [Symbol (U nil (t/Seqable r/Type)) -> (U nil r/Type)])
(defn get-ctor
  "Returns the instantiated constructor type on nominal csym."
  [csym args]
  {:pre [(symbol? csym)
         (every? r/Type? args)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in @JSNOMINAL-ENV [csym :ctor])]
    (c/inst-and-subst tscope args)))

(ann reset-jsnominal! [JSNominalEnv -> nil])
(defn reset-jsnominal! [m]
  (reset! JSNOMINAL-ENV m)
  nil)
