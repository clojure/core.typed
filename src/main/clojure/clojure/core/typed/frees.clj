(ns clojure.core.typed.frees
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.name-env :as nmenv]
            [clojure.core.typed.declared-kind-env :as kinds])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result AnyValue
                                        HeterogeneousSeq Scope TCError Extends)
           (clojure.core.typed.filter_rep FilterSet TypeFilter NotTypeFilter ImpFilter
                                          AndFilter OrFilter TopFilter BotFilter)
           (clojure.core.typed.object_rep Path EmptyObject NoObject)
           (clojure.core.typed.path_rep KeyPE)))

(def ^:dynamic *frees-mode* nil)
(set-validator! #'*frees-mode* (some-fn #{::frees ::idxs} nil?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(def variance-map? (u/hash-c? symbol? r/variance?))

(declare frees-in)

(defn fv-variances 
  "Map of frees to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::frees]
    (frees-in t)))

(defn idx-variances 
  "Map of indexes to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::idxs]
    (frees-in t)))

(defn fv 
  "All frees in type"
  [t]
  {:post [((u/set-c? symbol?) %)]}
  (set (keys (fv-variances t))))

(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t]
  {:post [((u/set-c? symbol?) %)]}
  (set (keys (idx-variances t))))

(defn flip-variances [vs]
  {:pre [(variance-map? vs)]}
  (into {} (for [[k vari] vs]
             [k (case vari
                  :covariant :contravariant
                  :contravariant :covariant
                  vari)])))

(defn combine-frees [& frees]
  {:pre [(every? variance-map? frees)]
   :post [(variance-map? %)]}
  (into {}
        (apply merge-with (fn [old-vari new-vari]
                            (cond 
                              (= old-vari new-vari) old-vari
                              (= old-vari :dotted) new-vari
                              (= new-vari :dotted) old-vari
                              (= old-vari :constant) new-vari
                              (= new-vari :constant) old-vari
                              :else :invariant))
               frees)))

(derive ::frees ::any-var)
(derive ::idxs ::any-var)

(declare frees)

(defn frees-in [t]
  {:post [(variance-map? %)]}
  (frees t))

(defmulti frees (fn [t] [*frees-mode* (class t)]))

(defmethod frees [::any-var Result]
  [{:keys [t fl o]}]
  (combine-frees (frees t)
                 (frees fl)
                 (frees o)))

;; Filters

(defmethod frees [::any-var FilterSet]
  [{:keys [then else]}]
  (combine-frees (frees then)
                 (frees else)))

(defmethod frees [::any-var TypeFilter]
  [{:keys [type]}]
  (frees type))

(defmethod frees [::any-var NotTypeFilter]
  [{:keys [type]}] 
  (flip-variances (frees type)))

(defmethod frees [::any-var ImpFilter]
  [{:keys [a c]}] 
  (combine-frees (frees a)
                 (frees c)))

(defmethod frees [::any-var AndFilter]
  [{:keys [fs]}] 
  (apply combine-frees (mapv frees fs)))

(defmethod frees [::any-var OrFilter]
  [{:keys [fs]}]
  (apply combine-frees (mapv frees fs)))

(defmethod frees [::any-var TopFilter] [t] {})
(defmethod frees [::any-var BotFilter] [t] {})

;; Objects

(defmethod frees [::any-var Path]
  [{:keys [path]}]
  (apply combine-frees (mapv frees path)))

(defmethod frees [::any-var EmptyObject] [t] {})
(defmethod frees [::any-var NoObject] [t] {})
(defmethod frees [::any-var KeyPE] [t] {})


(defmethod frees [::frees F]
  [{:keys [name] :as t}]
  {name :covariant})

(defmethod frees [::idxs F] [t] {})

(defmethod frees [::any-var TCError] [t] {})
(defmethod frees [::any-var B] [t] {})
(defmethod frees [::any-var CountRange] [t] {})
(defmethod frees [::any-var Value] [t] {})
(defmethod frees [::any-var AnyValue] [t] {})
(defmethod frees [::any-var Top] [t] {})
(defmethod frees [::any-var Name] [t] {})

(defmethod frees [::any-var DataType]
  [{:keys [fields poly?]}]
  (apply combine-frees 
         (mapv frees (concat (vals fields) poly?))))

(defmethod frees [::any-var HeterogeneousList]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var App]
  [{:keys [rator rands]}]
  (apply combine-frees (mapv frees (cons rator rands))))

(defmethod frees [::any-var TApp]
  [{:keys [rator rands]}]
  (apply combine-frees
         (let [^TypeFn
               tfn (loop [rator rator]
                     (cond
                       (r/F? rator) (when-let [bnds (free-ops/free-with-name-bnds (.name ^F rator))]
                                      ;assume upper/lower bound variance agree
                                      (c/fully-resolve-type (:upper-bound bnds)))
                       (r/Name? rator) (let [{:keys [id]} rator]
                                         (cond
                                           (nmenv/declared-name? id)
                                           (kinds/get-declared-kind id)

                                           ; alter class introduces temporary declared kinds for
                                           ; computing variance when referencing an RClass inside
                                           ; its own definition.
                                           (and (class? (resolve id))
                                                (kinds/has-declared-kind? id))
                                           (kinds/get-declared-kind id)

                                           :else
                                           (recur (c/resolve-Name rator))))
                       (r/TypeFn? rator) rator
                       :else (throw (Exception. (u/error-msg "NYI case " (class rator))))))
               _ (assert (r/TypeFn? tfn) "First argument to TApp must be TypeFn")]
           (mapv (fn [[v arg-vs]]
                   (case v
                     :covariant arg-vs
                     :contravariant (flip-variances arg-vs)
                     :invariant (into {} (for [[k _] arg-vs]
                                           [k :invariant]))))
                 (map vector (.variances tfn) (map frees rands))))))

(defmethod frees [::any-var PrimitiveArray]
  [{:keys [input-type output-type]}] 
  (combine-frees (flip-variances (frees input-type))
                 (frees output-type)))

(defmethod frees [::any-var HeterogeneousSeq]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var HeterogeneousMap]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees (concat (keys types) (vals types)))))

(defmethod frees [::any-var HeterogeneousVector]
  [{:keys [types fs objects]}] 
  (apply combine-frees (mapv frees (concat types fs objects))))

(defmethod frees [::any-var Extends]
  [{:keys [extends without]}] 
  (apply combine-frees (mapv frees (concat extends without))))

(defmethod frees [::any-var NotType]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var FnIntersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::frees Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               [(vals kws)])))
                               [(frees rng)]
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees flip-variances)
                                          (:name drest))]))))

(defmethod frees [::idxs Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               (vals kws))))
                               [(frees rng)]
                               (when drest
                                 (let [{:keys [name pre-type]} drest]
                                   [{name :contravariant}
                                    (-> pre-type
                                      frees flip-variances)])))))

(defmethod frees [::any-var RClass]
  [t]
  (let [varis (:variances t)
        args (:poly? t)]
    (assert (= (count args) (count varis)))
    (apply combine-frees (for [[arg va] (map vector args varis)]
                           (case va
                             :covariant (frees arg)
                             :contravariant (flip-variances (frees arg))
                             :invariant (let [fvs (frees arg)]
                                          (into {}
                                                (for [[k _] fvs]
                                                  [k :invariant]))))))))

(defmethod frees [::any-var Scope]
  [{:keys [body]}]
  (frees body))

;FIXME Type variable bounds should probably be checked for frees
(defmethod frees [::any-var TypeFn]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var Poly]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var Mu]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var PolyDots]
  [{:keys [nbound scope]}]
  (frees scope))

