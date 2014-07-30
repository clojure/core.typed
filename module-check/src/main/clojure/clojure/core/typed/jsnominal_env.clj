(ns ^:skip-wiki clojure.core.typed.jsnominal-env
  (:refer-clojure :exclude [get-method])
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed :as t])
  (:import (clojure.core.typed.type_rep Scope)))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSNominal

(t/defalias JSNominalEntry
  "A map entry for a JSNominal type."
  '{:jsnominal r/Type
    :fields (t/Map t/Sym (t/U Scope r/Type))
    :methods (t/Map t/Sym (t/U Scope r/Type))
    :ctors (t/U Scope r/Type nil)
    :ancestors (t/Set (t/U Scope r/Type))})

(t/defalias JSNominalEnv
  "A map of symbols of JSNomainalEntry's"
  (t/Map t/Sym JSNominalEntry))

(t/ann JSNOMINAL-ENV (t/Atom1 JSNominalEnv))
(defonce JSNOMINAL-ENV 
  (atom {} 
        :validator
        (con/hash-c? symbol? 
                     (con/hmap-c? :jsnominal  r/Type?
                                  :fields (con/hash-c? symbol? (some-fn r/Scope? r/Type?))
                                  :methods (con/hash-c? symbol? (some-fn r/Scope? r/Type?))
                                  :ctor (some-fn nil? r/Scope? r/Type?)
                                  :ancestors (con/set-c? (some-fn r/Scope? r/Type?))))))

(t/ann init-jsnominal-entry [r/Type -> JSNominalEntry])
(defn init-jsnominal-entry [nom]
  {:jsnominal nom
   :fields {}
   :methods {}
   :ctors nil
   :ancestors #{}})

(t/ann get-jsnominal [t/Any -> (t/Nilable r/Type)])
(defn get-jsnominal
  "Returns the nomainal JS type with class symbol csym.
  Returns nil if not found."
  [csym]
  (-> (@JSNOMINAL-ENV csym) :jsnominal))

(t/ann contains-jsnominal? [t/Any -> boolean])
(defn contains-jsnominal?
  [csym]
  (boolean (get-jsnominal csym)))

;(t/ann add-jsnominal [t/Sym r/Type -> nil])
;(defn add-jsnominal [csym type]
;  (assert (r/Type? type)
;          (str "JS nominal" csym " not a type: " type))
;  ; remove old fields etc.
;  (swap! JSNOMINAL-ENV assoc-in [csym] (init-jsnominal-entry type))
;  nil)
;
;(t/ann add-method [t/Sym JSNominal (t/U Scope r/Type) -> nil])
;(defn add-method 
;  "Add a new method to the JS nominal type csym. Assumes
;  the method type is properly Scope'd"
;  [csym method-sym type]
;  (swap! JSNOMINAL-ENV assoc-in [csym :methods method-sym] type)
;  nil)
;
;(t/ann add-field [t/Sym JSNominal (t/U Scope r/Type) -> nil])
;(defn add-field 
;  "Add a new field to the JS nominal type csym. Assumes
;  the field type is properly Scope'd"
;  [csym field-sym type]
;  (swap! JSNOMINAL-ENV update-in [csym :fields field-sym] (constantly type)))

(t/ann ^:no-check get-method [t/Sym (t/U nil (t/Seqable r/Type)) t/Sym -> (t/U nil r/Type)])
(defn get-method 
  "Returns the instantiated method type named method-sym on nominal csym."
  [csym args method-sym]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? method-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in @JSNOMINAL-ENV [csym :methods method-sym])]
    (c/inst-and-subst tscope args)))

(t/ann ^:no-check get-field [t/Sym (t/U nil (t/Seqable r/Type)) t/Sym -> (t/U nil r/Type)])
(defn get-field 
  "Returns the instantiated field type named field-sym on nominal csym."
  [csym args field-sym]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? field-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in @JSNOMINAL-ENV [csym :fields field-sym])]
    (c/inst-and-subst tscope args)))

(t/ann ^:no-check get-ctor [t/Sym (t/U nil (t/Seqable r/Type)) -> (t/U nil r/Type)])
(defn get-ctor
  "Returns the instantiated constructor type on nominal csym."
  [csym args]
  {:pre [(symbol? csym)
         (every? r/Type? args)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in @JSNOMINAL-ENV [csym :ctor])]
    (c/inst-and-subst tscope args)))

(t/ann reset-jsnominal! [JSNominalEnv -> nil])
(defn reset-jsnominal! [m]
  (reset! JSNOMINAL-ENV m)
  nil)
