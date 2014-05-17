(ns ^:skip-wiki 
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.frees
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.filter-rep :as fr]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.name-env :as nmenv]
            [clojure.core.typed.declared-kind-env :as kinds])
  (:import (clojure.core.typed.type_rep NotType DifferenceType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top Unchecked TopFunction B F Result AnyValue
                                        HeterogeneousSeq Scope TCError Extends AssocType HSequential HSet)
           (clojure.core.typed.filter_rep FilterSet TypeFilter NotTypeFilter ImpFilter
                                          AndFilter OrFilter TopFilter BotFilter)
           (clojure.core.typed.object_rep Path EmptyObject NoObject)
           (clojure.core.typed.path_rep NthPE NextPE ClassPE CountPE KeyPE KeysPE ValsPE KeywordPE)))

(alter-meta! *ns* assoc :skip-wiki true
             :core.typed {:collect-only true})

;(t/typed-deps clojure.core.typed.type-rep)

;TODO make this an argument
(t/ann *frees-mode* (t/U nil t/Kw))
(defonce ^:dynamic *frees-mode* nil)
(t/tc-ignore
(set-validator! #'*frees-mode* (some-fn #{::frees ::idxs} nil?))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(t/defalias VarianceEntry
  "A map entry of a VarianceMap."
  '[t/Sym r/Variance])

(t/defalias VarianceMap
  "A map of free names (symbols) to their variances"
  (t/Map t/Sym r/Variance))

(t/ann ^:no-check variance-map? (t/Pred VarianceMap))
(def variance-map? (con/hash-c? symbol? r/variance?))

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

(t/ann fv [r/AnyType -> (t/Set t/Sym)])
(defn fv 
  "All frees in type"
  [t]
  {:post [((con/set-c? symbol?) %)]}
  (set (keys (fv-variances t))))

(t/ann fi [r/AnyType -> (t/Set t/Sym)])
(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t]
  {:post [((con/set-c? symbol?) %)]}
  (set (keys (idx-variances t))))

(t/ann flip-variances [VarianceMap -> VarianceMap])
(defn flip-variances [vs]
  {:pre [(variance-map? vs)]}
  (zipmap (keys vs) 
          (map (t/fn [vari :- r/Variance]
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

(t/ann frees [t/Any -> VarianceMap])
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

(add-frees-method [::any-var NthPE] [t] {})
(add-frees-method [::any-var NextPE] [t] {})
(add-frees-method [::any-var ClassPE] [t] {})
(add-frees-method [::any-var CountPE] [t] {})
(add-frees-method [::any-var KeyPE] [t] {})
(add-frees-method [::any-var KeysPE] [t] {})
(add-frees-method [::any-var ValsPE] [t] {})
(add-frees-method [::any-var KeywordPE] [t] {})


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
(add-frees-method [::any-var Unchecked] [t] {})
(add-frees-method [::any-var Name] [t] {})

(add-frees-method [::any-var DataType]
  [{varis :variances args :poly? :as t}]
  (assert (= (count args) (count varis)))
  (apply combine-frees (t/for [[arg va] :- '[t/Any r/Variance], (map (-> vector 
                                                                         (t/inst t/Any r/Variance t/Any t/Any t/Any t/Any))
                                                                     args varis)]
                         :- VarianceMap
                         (case va
                           :covariant (frees arg)
                           :contravariant (flip-variances (frees arg))
                           :invariant (let [fvs (frees arg)]
                                        (zipmap (keys fvs) (repeat :invariant)))))))

(add-frees-method [::any-var HeterogeneousList]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(add-frees-method [::any-var App]
  [{:keys [rator rands]}]
  (apply combine-frees (mapv frees (cons rator rands))))

;FIXME flow error during checking
(t/tc-ignore
(add-frees-method [::any-var TApp]
  [{:keys [rator rands] :as tapp}]
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
                       :else (err/int-error (str "Invalid operator to type application: "
                                               ((impl/v 'clojure.core.typed.parse-unparse/unparse-type)
                                                tapp)))))
               _ (when-not (r/TypeFn? tfn) 
                   (err/int-error (str "First argument to TApp must be TypeFn")))]
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

(add-frees-method [::any-var HeterogeneousMap]
  [{:keys [types optional]}]
  (apply combine-frees (mapv frees (concat (keys types) (vals types)
                                           (keys optional) (vals optional)))))

(defn heterogeneous*-frees-any-var
  [{:keys [types fs objects rest drest]}]
  (apply combine-frees (concat (mapv frees (concat types fs objects))
                               (when rest [(frees rest)])
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees)
                                          (:name drest))]))))

(add-frees-method [::any-var HeterogeneousSeq]
  [t]
  (heterogeneous*-frees-any-var t))

(add-frees-method [::any-var HeterogeneousVector]
  [t]
  (heterogeneous*-frees-any-var t))

(add-frees-method [::any-var HSequential]
  [t]
  (heterogeneous*-frees-any-var t))

(add-frees-method [::any-var HSet]
  [{:keys [fixed]}]
  (mapv combine-frees fixed))

(add-frees-method [::any-var Extends]
  [{:keys [extends without]}] 
  (apply combine-frees (mapv frees (concat extends without))))

(add-frees-method [::frees AssocType]
  [{:keys [target entries dentries]}]
  (apply combine-frees (concat [(frees target)]
                               (mapv frees (apply concat entries))
                               (when dentries
                                 [(dissoc (-> (:pre-type dentries) frees)
                                          (:name dentries))]))))

(add-frees-method [::idxs AssocType]
  [{:keys [target entries dentries]}]
  (apply combine-frees (concat [(frees target)]
                               (mapv frees (apply concat entries))
                               (when-let [{:keys [name pre-type]} dentries]
                                   (assert (symbol? name))
                                   [(t/ann-form {name :covariant} VarianceMap)
                                    (frees pre-type)]))))

; are negative types covariant?
(add-frees-method [::any-var NotType]
  [{:keys [type]}] 
  (frees type))

(add-frees-method [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

; are negative types covariant?
(add-frees-method [::any-var DifferenceType]
  [{:keys [type without]}] 
  (apply combine-frees (frees type) (mapv frees without)))

(add-frees-method [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(add-frees-method [::any-var FnIntersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(add-frees-method [::frees Function]
  [{:keys [dom rng rest drest kws prest pdot]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               [(vals kws)])
                                             (when prest
                                               [prest])))
                               [(frees rng)]
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees flip-variances)
                                          (:name drest))])
                               (when pdot
                                 [(dissoc (-> (:pre-type pdot) frees flip-variances)
                                          (:name pdot))]))))

(add-frees-method [::idxs Function]
  [{:keys [dom rng rest drest kws prest pdot]}]
  (apply combine-frees (concat (mapv #(-> % frees flip-variances)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               (vals kws))
                                             (when prest
                                               [prest])))
                               [(frees rng)]
                               (when drest
                                 (let [{:keys [name pre-type]} drest]
                                   (assert (symbol? name))
                                   [(t/ann-form {name :contravariant} VarianceMap)
                                    (-> pre-type
                                      frees flip-variances)]))
                               (when pdot
                                 (let [{:keys [name pre-type]} pdot]
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
    (apply combine-frees (t/for
                           [[arg va] :- '[t/Any r/Variance], (map (-> vector 
                                                                    (t/inst t/Any r/Variance t/Any t/Any t/Any t/Any))
                                                                args varis)]
                           :- VarianceMap
                           (case va
                             :covariant (frees arg)
                             :contravariant (flip-variances (frees arg))
                             :invariant (let [fvs (frees arg)]
                                          (zipmap (keys fvs) (repeat :invariant))))))))
  )

(t/tc-ignore
(add-frees-method [::any-var Protocol]
  [{varis :variances, args :poly?, :as t}]
  (assert (= (count args) (count varis)))
  (apply combine-frees (t/for
                         [[arg va] :- '[t/Any r/Variance], (map (-> vector 
                                                                    (t/inst t/Any r/Variance t/Any t/Any t/Any t/Any))
                                                                args varis)]
                         :- VarianceMap
                         (case va
                           :covariant (frees arg)
                           :contravariant (flip-variances (frees arg))
                           :invariant (let [fvs (frees arg)]
                                        (zipmap (keys fvs) (repeat :invariant)))))))
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
  (let [_ (when-not (every? empty? (map frees bbnds))
            (err/nyi-error "NYI Handle frees in bounds"))]
    (frees scope)))

(add-frees-method [::any-var Mu]
  [{:keys [scope]}]
  (frees scope))

(add-frees-method [::any-var PolyDots]
  [{:keys [scope bbnds]}]
  (let [_ (when-not (every? empty? (map frees bbnds))
            (err/nyi-error "NYI Handle frees in bounds"))]
    (frees scope)))

;;js types
(add-frees-method [::any-var clojure.core.typed.type_rep.BooleanCLJS] [t] {})
(add-frees-method [::any-var clojure.core.typed.type_rep.ObjectCLJS] [t] {})
(add-frees-method [::any-var clojure.core.typed.type_rep.StringCLJS] [t] {})
(add-frees-method [::any-var clojure.core.typed.type_rep.NumberCLJS] [t] {})
(add-frees-method [::any-var clojure.core.typed.type_rep.IntegerCLJS] [t] {})
(add-frees-method [::any-var clojure.core.typed.type_rep.ArrayCLJS] [t] {})
(add-frees-method [::any-var clojure.core.typed.type_rep.FunctionCLJS] [t] {})

(comment
(u/profile :info :foo (t/check-ns))
  )
