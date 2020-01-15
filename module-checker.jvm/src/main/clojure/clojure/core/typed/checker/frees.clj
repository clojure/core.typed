;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki 
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.checker.frees
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.object-rep]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.filter-rep :as fr]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.name-env :as nmenv]
            [clojure.core.typed.checker.declared-kind-env :as kinds])
  (:import (clojure.core.typed.checker.type_rep NotType DifferenceType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Unchecked TopFunction B F Result AnyValue
                                        Scope TCError Extends AssocType HSequential HSet
                                        JSObj TypeOf)
           (clojure.core.typed.checker.filter_rep FilterSet TypeFilter NotTypeFilter ImpFilter
                                          AndFilter OrFilter TopFilter BotFilter)
           (clojure.core.typed.checker.object_rep Path EmptyObject NoObject)
           (clojure.core.typed.checker.path_rep NthPE NextPE ClassPE CountPE KeyPE KeysPE ValsPE KeywordPE)))

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
  (frees t))

(t/ann frees [t/Any -> VarianceMap])
(defmulti ^:private frees (fn [t] [*frees-mode* (class t)]))

(defmethod frees [::any-var Result]
  [t]
  (t/ann-form t Result)
  (let [{:keys [t fl o]} t]
    (combine-frees (frees t)
                   (frees fl)
                   (frees o))))

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

(defmethod frees [::any-var NthPE] [t] {})
(defmethod frees [::any-var NextPE] [t] {})
(defmethod frees [::any-var ClassPE] [t] {})
(defmethod frees [::any-var CountPE] [t] {})
(defmethod frees [::any-var KeyPE] [t] {})
(defmethod frees [::any-var KeysPE] [t] {})
(defmethod frees [::any-var ValsPE] [t] {})
(defmethod frees [::any-var KeywordPE] [t] {})


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
(defmethod frees [::any-var Unchecked] [t] {})
(defmethod frees [::any-var Name] [t] {})
(defmethod frees [::any-var TypeOf] [t] {})

(defmethod frees [::any-var DataType]
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

(defmethod frees [::any-var App]
  [{:keys [rator rands]}]
  (apply combine-frees (mapv frees (cons rator rands))))

(def ^:private unparse-type (delay (impl/dynaload 'clojure.core.typed.checker.jvm.parse-unparse/unparse-type)))

;FIXME flow error during checking
(t/tc-ignore
(defmethod frees [::any-var TApp]
  [{:keys [rator rands] :as tapp}]
  (apply combine-frees
         (let [tfn (loop [rator rator]
                     (cond
                       (r/F? rator) (when-let [bnds (free-ops/free-with-name-bnds (:name rator))]
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
                                               (@unparse-type tapp)))))
               _ (when-not (r/TypeFn? tfn) 
                   (err/int-error (str "First argument to TApp must be TypeFn")))]
           (mapv (fn [[v arg-vs]]
                   (case v
                     :covariant arg-vs
                     :contravariant (flip-variances arg-vs)
                     :invariant (into {} (for [[k _] arg-vs]
                                           [k :invariant]))))
                 (map vector (:variances tfn) (map frees rands))))))
  )

(defmethod frees [::any-var PrimitiveArray]
  [{:keys [input-type output-type]}] 
  (combine-frees (flip-variances (frees input-type))
                 (frees output-type)))

(defmethod frees [::any-var HeterogeneousMap]
  [{:keys [types optional]}]
  (apply combine-frees (mapv frees (concat (keys types) (vals types)
                                           (keys optional) (vals optional)))))

(defmethod frees [::any-var JSObj]
  [{:keys [types]}]
  (apply combine-frees (mapv frees (vals types))))

(defmethod frees [::any-var HSequential]
  [{:keys [types fs objects rest drest]}]
  (apply combine-frees (concat (mapv frees (concat types fs objects))
                               (when rest [(frees rest)])
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees)
                                          (:name drest))]))))

(defmethod frees [::any-var HSet]
  [{:keys [fixed]}]
  (mapv combine-frees fixed))

(defmethod frees [::any-var Extends]
  [{:keys [extends without]}] 
  (apply combine-frees (mapv frees (concat extends without))))

(defmethod frees [::frees AssocType]
  [{:keys [target entries dentries]}]
  (apply combine-frees (concat [(frees target)]
                               (mapv frees (apply concat entries))
                               (when dentries
                                 [(dissoc (-> (:pre-type dentries) frees)
                                          (:name dentries))]))))

(defmethod frees [::idxs AssocType]
  [{:keys [target entries dentries]}]
  (apply combine-frees (concat [(frees target)]
                               (mapv frees (apply concat entries))
                               (when-let [{:keys [name pre-type]} dentries]
                                   (assert (symbol? name))
                                   [(t/ann-form {name :covariant} VarianceMap)
                                    (frees pre-type)]))))

; are negative types covariant?
(defmethod frees [::any-var NotType]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

; are negative types covariant?
(defmethod frees [::any-var DifferenceType]
  [{:keys [type without]}] 
  (apply combine-frees (frees type) (mapv frees without)))

(defmethod frees [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var FnIntersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::frees Function]
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

(defmethod frees [::idxs Function]
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
(defmethod frees [::any-var RClass]
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
(defmethod frees [::any-var Protocol]
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

(defmethod frees [::any-var Scope]
  [{:keys [body]}]
  (frees body))

(defmethod frees [::any-var Bounds]
  [{:keys [upper-bound lower-bound]}]
  (combine-frees (frees upper-bound)
                 (frees lower-bound)))

;FIXME Type variable bounds should probably be checked for frees
(defmethod frees [::any-var TypeFn]
  [{:keys [scope bbnds]}]
  (let [_ (assert (every? empty? (map frees bbnds))
                  "NYI Handle frees in bounds")]
    (frees scope)))

(defmethod frees [::any-var Poly]
  [{:keys [scope bbnds]}]
  (let [_ (when-not (every? empty? (map frees bbnds))
            (err/nyi-error "NYI Handle frees in bounds"))]
    (frees scope)))

(defmethod frees [::any-var Mu]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var PolyDots]
  [{:keys [scope bbnds]}]
  (let [_ (when-not (every? empty? (map frees bbnds))
            (err/nyi-error "NYI Handle frees in bounds"))]
    (frees scope)))

;;js types
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSBoolean] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSObject] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSString] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSSymbol] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSNumber] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.CLJSInteger] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.ArrayCLJS] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.FunctionCLJS] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSUndefined] [t] {})
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSNull] [t] {})
