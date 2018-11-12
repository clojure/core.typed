;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.promote-demote
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.frees :as frees]
            [clojure.core.typed :as t]
            [clojure.core.typed.checker.hset-utils :as hset]
            [clojure.set :as set]
            [clojure.core.typed.checker.impl-protocols :as p]
            clojure.core.typed.checker.filter-rep)
  (:import (clojure.core.typed.checker.type_rep NotType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Unchecked TopFunction B F Result AnyValue
                                        TCError Extends JSNominal
                                        JSString JSBoolean JSNumber CLJSInteger JSObject
                                        ArrayCLJS FunctionCLJS KwArgsSeq HSequential HSet LTRange
                                        AnyValue TopFunction Scope DissocType AssocType
                                        GetType GTRange JSUndefined JSNull JSSymbol JSObj TypeOf)
           (clojure.core.typed.checker.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)))

;FIXME use fold!
;TODO automatically check for completeness

;; Note: paths and path elements are currently ignored because they cannot contain variables.
;; FIXME this should probably consider filters too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elimination

(t/defalias ElimVars
  "A set of variables to be eliminated via promotion
  or demotion."
  (t/Set clojure.lang.Symbol))

(declare promote demote)

(t/ann promote-var [r/AnyType ElimVars -> r/AnyType])
(defn promote-var [T V]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/AnyType? %)]}
  (promote T V))

(t/ann demote-var [r/AnyType ElimVars -> r/AnyType])
(defn demote-var [T V]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/AnyType? %)]}
  (demote T V))

;no-check because of update-in
(t/ann ^:no-check promote [r/AnyType ElimVars -> r/AnyType])
(defmulti promote 
  "Eliminate all variables V in t by promotion"
  (fn [T V] 
    {:pre [(r/AnyType? T)
           (set? V)
           (every? symbol? V)]}
    (class T)))

;no-check because of update-in
(t/ann ^:no-check demote [r/AnyType ElimVars -> r/AnyType])
(defmulti demote 
  "Eliminate all variables V in T by demotion"
  (fn [T V]
    {:pre [(r/AnyType? T)
           (set? V)
           (every? symbol? V)]}
    (class T)))

(defn completeness-check []
  (let [vs (set (keys (methods promote)))
        expecteds (set (map resolve @u/all-types))
        missing (set/difference expecteds vs)]
    (when (seq missing)
      {:missing missing
       :mm 'promote})))

(defmacro promote-demote [cls & fbody]
  `(do (defmethod promote ~cls [T# V#] 
         (let [~'promote promote
               ~'demote demote
               f# (fn ~@fbody)]
           (f# T# V#)))
       (defmethod demote ~cls [T# V#] 
         (let [~'promote demote
               ~'demote promote
               f# (fn ~@fbody)]
           (f# T# V#)))))

(promote-demote ArrayCLJS 
  [T V]
  (-> T
    (update-in [:input-type] #(demote % V))
    (update-in [:output-type] #(promote % V))))

(promote-demote PrimitiveArray
  [T V]
  (-> T
    (update-in [:input-type] #(demote % V))
    (update-in [:output-type] #(promote % V))))

(defmethod promote F
  [{:keys [name] :as T} V]
  (if (V name)
    r/-any
    T))

(defmethod demote F
  [{:keys [name] :as T} V]
  (if (V name)
    (r/Bottom)
    T))

(defn handle-kw-map [m p-or-d-fn V]
  (into {}
        (for [[k v] m]
          [k (p-or-d-fn v V)])))

(promote-demote KwArgsSeq
  [T V]
  (-> T
    (update-in [:mandatory] handle-kw-map promote V)
    (update-in [:optional] handle-kw-map promote V)))

(promote-demote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] handle-kw-map promote V)
    (update-in [:optional] handle-kw-map promote V)))

(promote-demote JSObj
  [T V]
  (-> T
    (update-in [:types] handle-kw-map promote V)))

(promote-demote HSequential
  [T V]
  (let [pmt #(promote % V)
        latent-filter-vs (set/intersection (set (mapcat frees/fv (:fs T)))
                                           (set (mapcat frees/fi (:fs T))))]
    (cond
      ;if filter contains V, give up
      (seq (set/intersection V latent-filter-vs)) (case (:kind T)
                                                    :vector (c/RClass-of clojure.lang.IPersistentVector [r/-any])
                                                    :seq (c/RClass-of clojure.lang.ISeq [r/-any])
                                                    :list (c/RClass-of clojure.lang.IPersistentList [r/-any])
                                                    :sequential
                                                    (c/In (c/RClass-of clojure.lang.Sequential)
                                                          (c/RClass-of clojure.lang.IPersistentCollection [r/-any])))

      ;if dotted bound is in V, transfer to rest args
      (and (:drest T) (V (-> T :drest :name)))
      (r/-hsequential (mapv pmt (:types T))
                      :filters (:fs T)
                      :objects (:objects T)
                      :rest (pmt (-> T :drest :pre-type))
                      :kind (:kind T))

      :else
      (r/-hsequential (mapv pmt (:types T))
                      ; we know no filters contain V
                      :filters (:fs T)
                      :objects (:objects T)
                      :rest (when-let [rest (:rest T)]
                              (pmt rest))
                      :drest (when-let [drest (:drest T)]
                               (update-in drest [:pre-type] pmt))
                      :repeat (:repeat T)
                      :kind (:kind T)))))

(promote-demote HSet
  [T V]
  (let [fixed (set (mapv promote (:fixed T) (repeat V)))
        h (r/-hset fixed :complete? (:complete? T))]
    (if (every? (fn [a] 
                  (and (r/Value? a)
                       (hset/valid-fixed? (:val a))))
                fixed)
      h
      (c/upcast-hset h))))

(promote-demote Value [T V] T)

(promote-demote JSNominal [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv promote % (repeat V))))))

(promote-demote DataType [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv promote % (repeat V))))
    #_(update-in [:fields] #(apply array-map
                                 (apply concat
                                        (for [[k v] %]
                                          [k (promote v V)]))))))

(defmacro promote-demote-id [& cs]
  `(do ~@(map (fn [c]
                `(promote-demote ~c [T# V#] T#))
              cs)))

(promote-demote-id B Name Top Unchecked TCError CountRange JSString
                   JSBoolean JSNumber JSObject CLJSInteger
                   FunctionCLJS LTRange GTRange AnyValue TopFunction JSUndefined
                   JSNull JSSymbol TypeOf)

(promote-demote GetType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update-in [:target] pmt)
        (update-in [:key] pmt)
        (update-in [:not-found] pmt)
        (update-in [:target-fs] pmt)
        (update-in [:target-object] pmt))))

(promote-demote AssocType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update-in [:target] pmt)
        (update-in [:entries] (fn [entries] (mapv #(mapv pmt %) entries)))
        (update-in [:dentries] #(some-> % (update-in [:pre-type] pmt))))))

(promote-demote DissocType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update-in [:target] pmt)
        (update-in [:keys] (fn [keys] (mapv pmt keys)))
        (update-in [:dkeys] #(some-> % (update-in [:pre-type] pmt))))))

(promote-demote Scope
  [T V]
  (-> T
    (update-in [:body] #(promote % V))))

(promote-demote TApp
  [T V]
  (-> T
    (update-in [:rator] #(promote % V))
    (update-in [:rands] (fn [rands] (mapv #(promote % V) rands)))))

(promote-demote App
  [T V]
  (-> T
    (update-in [:rator] #(promote % V))
    (update-in [:rands] (fn [rands] (mapv #(promote % V) rands)))))

(promote-demote Union 
  [T V]
  (apply c/Un (map promote (:types T) (repeat V))))

; FIXME is this correct? Promoting NotType should make the inner type smaller,
; and demoting should make inner type bigger?
(defmethod promote NotType
  [T V] 
  (-> T
    (update-in [:type] #(demote % V))))

(defmethod demote NotType
  [T V] 
  (-> T
    (update-in [:type] #(promote % V))))

(promote-demote Extends
  [T V] 
  (c/-extends
    (map promote (:extends T) (repeat V))
    :without (map demote (:without T) (repeat V))))

(promote-demote Intersection
  [T V] 
  (apply c/In (map promote (:types T) (repeat V))))

(promote-demote FnIntersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(promote-demote Protocol
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update-in [:poly?] #(when %
                               (mapv pmt %)))
        (update-in [:methods] (fn [ms]
                                (into {}
                                      (for [[k v] ms]
                                        [k (pmt v)])))))))

(promote-demote RClass
  [T V]
  (let [pmt #(promote % V)]
    (-> T
      (update-in [:poly?] #(when %
                             (mapv pmt %)))
      #_(update-in [:replacements] #(into {} (for [[k v] %]
                                             [k (pmt v)]))))))

(promote-demote TypeFn
  [{:keys [variances] :as T} V]
  (let [names (c/TypeFn-fresh-symbols* T)
        pmt-body (promote (c/TypeFn-body* names T) V)]
    (c/TypeFn* names 
               variances
               (c/TypeFn-bbnds* names T)
               pmt-body)))

(promote-demote Poly [T V]
  (let [names (c/Poly-fresh-symbols* T)
        bbnds (c/Poly-bbnds* names T)
        pmt-body (promote (c/Poly-body* names T) V)]
    (c/Poly* names 
             bbnds
             pmt-body)))

(promote-demote Mu 
  [T V]
  (let [name (c/Mu-fresh-symbol* T)
        body (c/Mu-body* name T)]
    (c/Mu* name (promote body V))))

;; Note: ignores path elements
(defmethod promote Function
  [{:keys [dom rng rest drest kws] :as T} V]
  (let [pmt #(promote % V)
        dmt #(demote % V)
        dmt-kw #(into {} (for [[k v] %]
                           [k (dmt v)]))
        latent-filter-vs (let [f (r/Result-filter* rng)]
                           (set/intersection (frees/fv f)
                                             (frees/fi f)))]
    (cond 
      ;if filter contains V, give up
      (seq (set/intersection V latent-filter-vs)) (r/TopFunction-maker)

      ;if dotted bound is in V, transfer to rest args
      (and drest (V (:name drest)))
      (-> T
        (update-in [:dom] #(mapv dmt %))
        (update-in [:rng] pmt)
        (assoc :rest (dmt (:pre-type drest)))
        (assoc :drest nil)
        (assoc :kws (when kws
                      (-> kws
                        (update-in [:mandatory] dmt-kw)
                        (update-in [:optional] dmt-kw)))))

      :else
      (-> T
        (update-in [:dom] #(mapv dmt %))
        ;we know no filters contain V
        (update-in [:rng] #(-> %
                             (update-in [:t] pmt)))
        (update-in [:rest] #(when %
                              (dmt %)))
        (update-in [:drest] #(when %
                               (-> %
                                 (update-in [:pre-type] dmt))))
        (update-in [:kws] #(when %
                             (-> %
                               (update-in [:mandatory] dmt-kw)
                               (update-in [:optional] dmt-kw))))))))

(defmethod demote Function
  [{:keys [dom rng rest drest kws] :as T} V]
  (let [pmt #(promote % V)
        dmt #(demote % V)
        pmt-kw #(into {} (for [[k v] %]
                           [k (pmt v)]))
        latent-filter-vs (let [f (r/Result-filter* rng)]
                           (set/intersection (frees/fv f)
                                             (frees/fi f)))]
    (cond 
      ;if filter contains V, give up
      (seq (set/intersection V latent-filter-vs)) (r/TopFunction-maker)

      ;if dotted bound is in V, transfer to rest args
      (and drest (V (:name drest)))
      (-> T
        (update-in [:dom] #(mapv pmt %))
        (update-in [:rng] dmt)
        (assoc :rest (pmt (:pre-type drest)))
        (assoc :drest nil)
        (assoc :kws (when kws
                      (-> kws
                        (update-in [:mandatory] pmt-kw)
                        (update-in [:optional] pmt-kw)))))

      :else
      (-> T
        (update-in [:dom] #(mapv pmt %))
        ;we know no filters contain V
        (update-in [:rng] #(-> %
                             (update-in [:t] pmt)))
        (update-in [:rest] #(when %
                              (pmt %)))
        (update-in [:drest] #(when %
                               (-> %
                                 (update-in [:pre-type] pmt))))
        (update-in [:kws] #(when %
                             (-> %
                               (update-in [:mandatory] pmt-kw)
                               (update-in [:optional] pmt-kw))))))))
