;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.jsnominal-env
  (:refer-clojure :exclude [get-method])
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.env :as env])
  (:import (clojure.core.typed.checker.type_rep Scope)))

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

(defn jsnominal-env []
  {:post [(map? %)]}
  (impl/jsnominal-env))

(def jsnominal-env?
  (con/hash-c? symbol?
               (con/hmap-c? :jsnominal  r/Type?
                            :fields (con/hash-c? symbol? (some-fn r/Scope? r/Type?))
                            :methods (con/hash-c? symbol? (some-fn r/Scope? r/Type?))
                            :ctor (some-fn nil? r/Scope? r/Type?)
                            :ancestors (con/set-c? (some-fn r/Scope? r/Type?)))))

(t/ann init-jsnominal-entry [r/Type -> JSNominalEntry])
(defn init-jsnominal-entry [nom]
  {:post [(jsnominal-env? %)]}
  {:jsnominal nom
   :fields {}
   :methods {}
   :ctors nil
   :ancestors #{}})

(t/ann ^:no-check get-jsnominal [t/Any -> (t/Nilable r/Type)])
(defn get-jsnominal
  "Returns the nomainal JS type with class symbol csym.
  Returns nil if not found."
  [csym]
  {:post [((some-fn nil? r/Type?) %)]}
  (-> (get (impl/jsnominal-env) csym) :jsnominal))

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

(declare get-method get-field get-inherited-property)

(t/ann ^:no-check get-method [t/Sym (t/U nil (t/Seqable r/Type)) t/Sym -> (t/U nil r/Type)])
(defn get-method 
  "Returns the instantiated method type named method-sym on nominal csym."
  [csym args method-sym]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? method-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (println (str "Searching " csym "#" method-sym))
  (if-let [tscope (get-in (impl/jsnominal-env) [csym :methods method-sym])]
    (c/inst-and-subst tscope args)
    (get-inherited-property get-method csym args method-sym)))

(t/ann ^:no-check get-inherited-property [[t/Sym (t/Option (t/Seqable r/Type)) t/Sym -> (t/Option r/Type)] 
                                          t/Sym (t/Option (t/Seqable r/Type)) t/Sym -> (t/Option r/Type)])
(defn get-inherited-property
  "search for the property in the interfaces ancestors
   method: (get-inherited-property get-method csym args method-sym)
   field:  (get-inherited-property get-field csym args field-sym)"
  [f csym args method-sym]
  ;(println (->> (get-in (impl/jsnominal-env) [csym :ancestors]) (map :id)))
  (->> (get-in (impl/jsnominal-env) [csym :ancestors])
       (map #(f (:id %) args method-sym))
       (filter identity)
       first))

(t/ann ^:no-check get-field [t/Sym (t/U nil (t/Seqable r/Type)) t/Sym -> (t/U nil r/Type)])
(defn get-field 
  "Returns the instantiated field type named field-sym on nominal csym."
  [csym args field-sym]
  {:pre [(symbol? csym)
         (every? r/Type? args)
         (symbol? field-sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (if-let [tscope (get-in (impl/jsnominal-env) [csym :fields field-sym])]
    (c/inst-and-subst tscope args)
    (get-inherited-property get-method csym args field-sym)))

(t/ann ^:no-check get-ctor [t/Sym (t/U nil (t/Seqable r/Type)) -> (t/U nil r/Type)])
(defn get-ctor
  "Returns the instantiated constructor type on nominal csym."
  [csym args]
  {:pre [(symbol? csym)
         (every? r/Type? args)]
   :post [((some-fn nil? r/Type?) %)]}
  (when-let [tscope (get-in (impl/jsnominal-env) [csym :ctor])]
    (c/inst-and-subst tscope args)))

(t/ann ^:no-check reset-jsnominal! [JSNominalEnv -> nil])
(defn reset-jsnominal! [m]
  {:pre [(jsnominal-env? m)]
   :post [(nil? %)]}
  (impl/reset-jsnominal-env! m)
  nil)
