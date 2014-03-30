(ns ^:skip-wiki clojure.core.typed.chk.common.promote-demote
  (:require [clojure.core.typed.chk.common.utils :as u]
            [clojure.core.typed.chk.common.type-rep :as r]
            [clojure.core.typed.chk.common.type-ctors :as c]
            [clojure.core.typed.chk.common.filter-rep]
            [clojure.core.typed.chk.common.object-rep]
            [clojure.core.typed.chk.common.path-rep]
            [clojure.core.typed.chk.common.frees :as frees]
            [clojure.core.typed :as t]
            [clojure.set :as set])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result AnyValue
                                        HeterogeneousSeq TCError Extends JSNominal
                                        StringCLJS BooleanCLJS NumberCLJS IntegerCLJS ObjectCLJS
                                        ArrayCLJS FunctionCLJS KwArgsSeq)
           (clojure.core.typed.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE CountPE ClassPE)
           (clojure.lang Cons IPersistentList Symbol IPersistentVector)))

(alter-meta! *ns* assoc :skip-wiki true)

;FIXME use fold!
;TODO automatically check for completeness

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elimination

(t/def-alias ElimVars
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

(defmethod promote ArrayCLJS
  [T V]
  (-> T
    (update-in [:input-type] #(demote % V))
    (update-in [:output-type] #(promote % V))))

(defmethod demote ArrayCLJS
  [T V]
  (-> T
    (update-in [:input-type] #(promote % V))
    (update-in [:output-type] #(demote % V))))

(defmethod promote PrimitiveArray
  [T V]
  (-> T
    (update-in [:input-type] #(demote % V))
    (update-in [:output-type] #(promote % V))))

(defmethod demote PrimitiveArray
  [T V]
  (-> T
    (update-in [:input-type] #(promote % V))
    (update-in [:output-type] #(demote % V))))

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

(defmethod promote KwArgsSeq
  [T V]
  (-> T
    (update-in [:mandatory] handle-kw-map promote V)
    (update-in [:optional] handle-kw-map promote V)))

(defmethod demote KwArgsSeq
  [T V]
  (-> T
    (update-in [:mandatory] handle-kw-map demote V)
    (update-in [:optional] handle-kw-map demote V)))

(defmethod promote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] handle-kw-map promote V)))

(defmethod demote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] handle-kw-map demote V)))

(defmethod promote HeterogeneousVector
  [T V]
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(defmethod demote HeterogeneousVector
  [T V]
  (-> T
    (update-in [:types] #(mapv demote % (repeat V)))))

(defmethod promote HeterogeneousList
  [T V]
  (-> T
    (update-in [:types] #(apply list (mapv promote % (repeat V))))))

(defmethod demote HeterogeneousList
  [T V]
  (-> T
    (update-in [:types] #(apply list (mapv demote % (repeat V))))))

(defmethod promote Value [T V] T)
(defmethod demote Value [T V] T)

(defmethod promote JSNominal [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv promote % (repeat V))))))

(defmethod demote JSNominal [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv demote % (repeat V))))))

(defmethod promote DataType [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv promote % (repeat V))))
    #_(update-in [:fields] #(apply array-map
                                 (apply concat
                                        (for [[k v] %]
                                          [k (promote v V)]))))))
(defmethod demote DataType [T V]
  (-> T
    (update-in [:poly?] #(when %
                           (mapv demote % (repeat V))))
    #_(update-in [:fields] #(apply array-map
                                 (apply concat
                                        (for [[k v] %]
                                          [k (demote v V)]))))))

(defmethod promote B [T V] T)
(defmethod demote B [T V] T)

(defmethod promote Name [T V] T)
(defmethod demote Name [T V] T)

(defmethod promote Top [T V] T)
(defmethod demote Top [T V] T)

(defmethod promote TCError [T V] T)
(defmethod demote TCError [T V] T)

(defmethod promote CountRange [T V] T)
(defmethod demote CountRange [T V] T)

(defmethod promote StringCLJS [T V] T)
(defmethod demote StringCLJS [T V] T)

(defmethod promote BooleanCLJS [T V] T)
(defmethod demote BooleanCLJS [T V] T)

(defmethod promote NumberCLJS [T V] T)
(defmethod demote NumberCLJS [T V] T)

(defmethod promote ObjectCLJS [T V] T)
(defmethod demote ObjectCLJS [T V] T)

(defmethod promote IntegerCLJS [T V] T)
(defmethod demote IntegerCLJS [T V] T)

(defmethod promote FunctionCLJS [T V] T)
(defmethod demote FunctionCLJS [T V] T)

(defmethod promote TApp
  [T V]
  (-> T
    (update-in [:rator] #(promote % V))
    (update-in [:rands] (fn [rands] (mapv #(promote % V) rands)))))

(defmethod demote TApp
  [T V]
  (-> T
    (update-in [:rator] #(demote % V))
    (update-in [:rands] (fn [rands] (mapv #(demote % V) rands)))))

(defmethod promote App
  [T V]
  (-> T
    (update-in [:rator] #(promote % V))
    (update-in [:rands] (fn [rands] (mapv #(promote % V) rands)))))

(defmethod demote App
  [T V]
  (-> T
    (update-in [:rator] #(demote % V))
    (update-in [:rands] (fn [rands] (mapv #(demote % V) rands)))))

(defmethod promote Union 
  [T V]
  (-> T
    (update-in [:types] #(set (mapv promote % (repeat V))))))

(defmethod demote Union 
  [T V] 
  (-> T
    (update-in [:types] #(set (mapv demote % (repeat V))))))

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

(defmethod promote Extends
  [T V] 
  (-> T
    (update-in [:extends] #(mapv promote % (repeat V)))
    (update-in [:without] #(mapv demote % (repeat V)))))

(defmethod demote Extends
  [T V] 
  (-> T
    (update-in [:extends] #(mapv demote % (repeat V)))
    (update-in [:without] #(mapv promote % (repeat V)))))


(defmethod promote Intersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(defmethod demote Intersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv demote % (repeat V)))))

(defmethod promote FnIntersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(defmethod demote FnIntersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv demote % (repeat V)))))

(defmethod promote Protocol
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update-in [:poly?] #(when %
                               (mapv pmt %)))
        (update-in [:methods] (fn [ms]
                                (into {}
                                      (for [[k v] ms]
                                        [k (pmt v)])))))))

(defmethod demote Protocol
  [T V]
  (let [dmt #(demote % V)]
    (-> T
        (update-in [:poly?] #(when %
                               (mapv dmt %)))
        (update-in [:methods] (fn [ms]
                                (into {}
                                      (for [[k v] ms]
                                        [k (dmt v)])))))))

(defmethod promote RClass
  [T V]
  (let [pmt #(promote % V)]
    (-> T
      (update-in [:poly?] #(when %
                             (mapv pmt %)))
      #_(update-in [:replacements] #(into {} (for [[k v] %]
                                             [k (pmt v)]))))))

(defmethod demote RClass
  [T V]
  (let [dmt #(demote % V)]
    (-> T
      (update-in [:poly?] #(when %
                             (mapv dmt %)))
      #_(update-in [:replacements] #(into {} (for [[k v] %]
                                             [k (dmt v)]))))))

(defmethod promote TypeFn
  [{:keys [variances] :as T} V]
  (let [names (c/TypeFn-fresh-symbols* T)
        pmt-body (promote (c/TypeFn-body* names T) V)]
    (c/TypeFn* names 
               variances
               (c/TypeFn-bbnds* names T)
               pmt-body)))

(defmethod demote TypeFn
  [{:keys [variances] :as T} V]
  (let [names (c/TypeFn-fresh-symbols* T)
        dem-body (demote (c/TypeFn-body* names T) V)]
    (c/TypeFn* names 
               variances
               (c/TypeFn-bbnds* names T)
               dem-body)))

(defmethod promote Poly
  [{:keys [] :as T} V]
  (let [names (c/Poly-fresh-symbols* T)
        bbnds (c/Poly-bbnds* names T)
        pmt-body (promote (c/Poly-body* names T) V)]
    (c/Poly* names 
             bbnds
             pmt-body)))

(defmethod demote Poly
  [{:keys [nbound] :as T} V]
  (let [names (c/Poly-fresh-symbols* T)
        bbnds (c/Poly-bbnds* names T)
        dem-body (demote (c/Poly-body* names T) V)]
    (c/Poly* names 
             bbnds
             dem-body)))

(defmethod promote Mu
  [T V]
  (let [name (c/Mu-fresh-symbol* T)
        body (c/Mu-body* name T)]
    (c/Mu* name (promote body V))))

(defmethod demote Mu
  [T V]
  (let [name (c/Mu-fresh-symbol* T)
        body (c/Mu-body* name T)]
    (c/Mu* name (demote body V))))

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
