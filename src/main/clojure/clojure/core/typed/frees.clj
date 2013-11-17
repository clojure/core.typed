(ns ^:skip-wiki clojure.core.typed.frees
  (:require [clojure.core.typed :as t :refer [for> fn>]]
            [clojure.core.typed.type-rep :as r]
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
                                        HeterogeneousSeq Scope TCError Extends AssocType)
           (clojure.core.typed.filter_rep FilterSet TypeFilter NotTypeFilter ImpFilter
                                          AndFilter OrFilter TopFilter BotFilter)
           (clojure.core.typed.object_rep Path EmptyObject NoObject)
           (clojure.core.typed.path_rep KeyPE)
           (clojure.lang Keyword Symbol)))

(alter-meta! *ns* assoc :skip-wiki true)

;(t/typed-deps clojure.core.typed.type-rep)

;TODO make this an argument
(t/ann *frees-mode* (U nil Keyword))
(defonce ^:dynamic *frees-mode* nil)
(t/tc-ignore
(set-validator! #'*frees-mode* (some-fn #{::frees ::idxs} nil?))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(t/def-alias VarianceEntry
  "A map entry of a VarianceMap."
  '[Symbol r/Variance])

(t/def-alias VarianceMap
  "A map of free names (symbols) to their variances"
  (t/Map Symbol r/Variance))

(t/ann ^:no-check variance-map? (predicate VarianceMap))
(def variance-map? (u/hash-c? symbol? r/variance?))

(declare frees-in)

(t/ann fv-variances [r/AnyType -> VarianceMap])
(defn fv-variances 
  "Map of frees to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::frees]
    (frees-in t)))

(t/ann idx-variances [r/AnyType -> VarianceMap])
(defn idx-variances 
  "Map of indexes to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::idxs]
    (frees-in t)))

(t/ann fv [r/AnyType -> (t/Set Symbol)])
(defn fv 
  "All frees in type"
  [t]
  {:post [((u/set-c? symbol?) %)]}
  (set (keys (fv-variances t))))

(t/ann fi [r/AnyType -> (t/Set Symbol)])
(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t]
  {:post [((u/set-c? symbol?) %)]}
  (set (keys (idx-variances t))))

(t/ann flip-variances [VarianceMap -> VarianceMap])
(defn flip-variances [vs]
  {:pre [(variance-map? vs)]}
  (zipmap (keys vs) 
          (map (fn> [vari :- r/Variance]
                 (case vari
                   :covariant :contravariant
                   :contravariant :covariant
                   vari))
               (vals vs))))

(t/ann combine-frees [VarianceMap * -> VarianceMap])
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

(t/ann frees-in [r/AnyType -> VarianceMap])
(defn frees-in [t]
  {:post [(variance-map? %)]}
  (u/p :frees/frees-in
  (frees t)))

(t/ann frees [Any -> VarianceMap])
(defmulti ^:private frees (fn [t] [*frees-mode* (class t)]))

(u/add-defmethod-generator frees)

(add-frees-method [::any-var Result]
  [t]
  (t/ann-form t Result)
  (let [{:keys [t fl o]} t]
    (combine-frees (frees t)
                   (frees fl)
                   (frees o))))

;; Filters

(add-frees-method [::any-var FilterSet]
  [{:keys [then else]}]
  (combine-frees (frees then)
                 (frees else)))

(add-frees-method [::any-var TypeFilter]
  [{:keys [type]}]
  (frees type))

(add-frees-method [::any-var NotTypeFilter]
  [{:keys [type]}] 
  (flip-variances (frees type)))

(add-frees-method [::any-var ImpFilter]
  [{:keys [a c]}] 
  (combine-frees (frees a)
                 (frees c)))

(add-frees-method [::any-var AndFilter]
  [{:keys [fs]}] 
  (apply combine-frees (mapv frees fs)))

(add-frees-method [::any-var OrFilter]
  [{:keys [fs]}]
  (apply combine-frees (mapv frees fs)))

(add-frees-method [::any-var TopFilter] [t] {})
(add-frees-method [::any-var BotFilter] [t] {})

;; Objects

(add-frees-method [::any-var Path]
  [{:keys [path]}]
  (apply combine-frees (mapv frees path)))

(add-frees-method [::any-var EmptyObject] [t] {})
(add-frees-method [::any-var NoObject] [t] {})
(add-frees-method [::any-var KeyPE] [t] {})


(add-frees-method [::frees F]
  [{:keys [name] :as t}]
  {name :covariant})

(add-frees-method [::idxs F] [t] {})

(add-frees-method [::any-var TCError] [t] {})
(add-frees-method [::any-var B] [t] {})
(add-frees-method [::any-var CountRange] [t] {})
(add-frees-method [::any-var Value] [t] {})
(add-frees-method [::any-var AnyValue] [t] {})
(add-frees-method [::any-var Top] [t] {})
(add-frees-method [::any-var Name] [t] {})

(add-frees-method [::any-var DataType]
  [{:keys [fields poly?]}]
  (apply combine-frees 
         (mapv frees (concat (vals fields) poly?))))

(add-frees-method [::any-var HeterogeneousList]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(add-frees-method [::any-var App]
  [{:keys [rator rands]}]
  (apply combine-frees (mapv frees (cons rator rands))))

;FIXME flow error during checking
(t/tc-ignore
(add-frees-method [::any-var TApp]
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
  )

(add-frees-method [::any-var PrimitiveArray]
  [{:keys [input-type output-type]}] 
  (combine-frees (flip-variances (frees input-type))
                 (frees output-type)))

(add-frees-method [::any-var HeterogeneousSeq]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(add-frees-method [::any-var HeterogeneousMap]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees (concat (keys types) (vals types)))))

(add-frees-method [::any-var HeterogeneousVector]
  [{:keys [types fs objects rest drest]}] 
  (apply combine-frees (concat (mapv frees (concat types fs objects))
                               (when rest [(frees rest)])
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees)
                                          (:name drest))]))))

(add-frees-method [::any-var Extends]
  [{:keys [extends without]}] 
  (apply combine-frees (mapv frees (concat extends without))))

(add-frees-method [::any-var AssocType]
  [{:keys [target entries]}] 
  (apply combine-frees (frees target)
         (mapv frees (apply concat entries))))

(add-frees-method [::any-var NotType]
  [{:keys [type]}] 
  (frees type))

(add-frees-method [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(add-frees-method [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(add-frees-method [::any-var FnIntersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(add-frees-method [::frees Function]
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

(add-frees-method [::idxs Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv #(-> % frees flip-variances)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               (vals kws))))
                               [(frees rng)]
                               (when drest
                                 (let [{:keys [name pre-type]} drest]
                                   (assert (symbol? name))
                                   [(t/ann-form {name :contravariant} VarianceMap)
                                    (-> pre-type
                                      frees flip-variances)])))))


(t/tc-ignore
(add-frees-method [::any-var RClass]
  [t]
  (let [varis (:variances t)
        args (:poly? t)]
    (assert (= (count args) (count varis)))
    (apply combine-frees (for> :- VarianceMap
                           [[arg va] :- '[Any r/Variance], (map (-> vector 
                                                                    (t/inst Any r/Variance Any Any Any Any))
                                                                args varis)]
                           (case va
                             :covariant (frees arg)
                             :contravariant (flip-variances (frees arg))
                             :invariant (let [fvs (frees arg)]
                                          (zipmap (keys fvs) (repeat :invariant))))))))
  )

(t/tc-ignore
(add-frees-method [::any-var Protocol]
  [t]
  (let [varis (:variances t)
        args (:poly? t)]
    (assert (= (count args) (count varis)))
    (apply combine-frees (for> :- VarianceMap
                           [[arg va] :- '[Any r/Variance], (map (-> vector 
                                                                    (t/inst Any r/Variance Any Any Any Any))
                                                                args varis)]
                           (case va
                             :covariant (frees arg)
                             :contravariant (flip-variances (frees arg))
                             :invariant (let [fvs (frees arg)]
                                          (zipmap (keys fvs) (repeat :invariant))))))))
  )

(add-frees-method [::any-var Scope]
  [{:keys [body]}]
  (frees body))

(add-frees-method [::any-var Bounds]
  [{:keys [upper-bound lower-bound]}]
  (combine-frees (frees upper-bound)
                 (frees lower-bound)))

;FIXME Type variable bounds should probably be checked for frees
(add-frees-method [::any-var TypeFn]
  [{:keys [scope bbnds]}]
  (let [_ (assert (every? empty? (map frees bbnds))
                  "NYI Handle frees in bounds")]
    (frees scope)))

(add-frees-method [::any-var Poly]
  [{:keys [scope bbnds]}]
  (let [_ (assert (every? empty? (map frees bbnds))
                  "NYI Handle frees in bounds")]
    (frees scope)))

(add-frees-method [::any-var Mu]
  [{:keys [scope]}]
  (frees scope))

(add-frees-method [::any-var PolyDots]
  [{:keys [scope bbnds]}]
  (let [_ (assert (every? empty? (map frees bbnds))
                  "NYI Handle frees in bounds")]
    (frees scope)))


(comment
(u/profile :info :foo (t/check-ns))
  )
