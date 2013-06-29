(ns clojure.core.typed.promote-demote
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]
             [type-ctors :as c]
             [filter-rep]
             [object-rep]
             [path-rep]
             [frees :as frees]]
            [clojure.set :as set])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        Projection DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result AnyValue
                                        HeterogeneousSeq TCError)
           (clojure.core.typed.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE CountPE ClassPE)
           (clojure.lang Cons IPersistentList Symbol IPersistentVector)))

;FIXME use fold!
;TODO automatically check for completeness

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elimination

(declare promote demote)

(defn promote-var [T V]
  {:pre [(r/Type? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/Type? %)]}
  (promote T V))

(defn demote-var [T V]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/Type? %)]}
  (demote T V))

(defmulti promote 
  "Eliminate all variables V in t by promotion"
  (fn [T V] 
    {:pre [(r/AnyType? T)
           (set? V)
           (every? symbol? V)]}
    (class T)))

(defmulti demote 
  "Eliminate all variables V in T by demotion"
  (fn [T V]
    {:pre [(r/AnyType? T)
           (set? V)
           (every? symbol? V)]}
    (class T)))

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

(defmethod promote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] #(into {}
                               (for [[k v] %]
                                 [k (promote v V)])))))

(defmethod demote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] #(into {}
                               (for [[k v] %]
                                 [k (demote v V)])))))

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

(defmethod promote Poly
  [{:keys [nbound] :as T} V]
  (let [free-names (c/Poly-free-names* T)
        names (repeatedly nbound gensym)
        pmt-body (promote (c/Poly-body* names T) V)]
    (c/Poly* names 
           (c/Poly-bbnds* names T)
           pmt-body
           free-names)))

(defmethod demote Poly
  [{:keys [nbound] :as T} V]
  (let [free-names (c/Poly-free-names* T)
        names (repeatedly nbound gensym)
        dem-body (demote (c/Poly-body* names T) V)]
    (c/Poly* names 
           (c/Poly-bbnds* names T)
           dem-body
           free-names)))

(defmethod promote Mu
  [T V]
  (let [name (gensym)
        body (c/Mu-body* name T)]
    (c/Mu* name (promote body V))))

(defmethod demote Mu
  [T V]
  (let [name (gensym)
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
