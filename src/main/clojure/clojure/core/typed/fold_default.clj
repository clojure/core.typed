(ns clojure.core.typed.fold-default
  (:require [clojure.core.typed.fold-rep :refer [add-default-fold-case]]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.filter-rep]
            [clojure.core.typed.filter-ops :as fops]
            [clojure.core.typed.object-rep]
            [clojure.core.typed.path-rep])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass JSNominal App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result
                                        HeterogeneousSeq TCResult TCError FlowSet Extends
                                        NumberCLJS IntegerCLJS ObjectCLJS StringCLJS ArrayCLJS
                                        BooleanCLJS)
           (clojure.core.typed.filter_rep NoFilter TopFilter BotFilter TypeFilter NotTypeFilter
                                          ImpFilter AndFilter OrFilter FilterSet)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE KeysPE ValsPE ClassPE)))

(add-default-fold-case NotType
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec))))

(add-default-fold-case Intersection
                       (fn [ty _]
                         ;(prn "fold-default Intersection" ty)
                         (let [ts (mapv type-rec (:types ty))]
                           ;(prn ts)
                           (apply c/In ts))))

(add-default-fold-case Union 
                       (fn [ty _]
                         ;(prn "union default" (clojure.core.typed.parse-unparse/unparse-type ty))
                         (apply c/Un (mapv type-rec (:types ty)))))

(add-default-fold-case FnIntersection
                       (fn [ty _]
                         (-> ty
                           (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case Bounds
                       (fn [ty _]
                         (r/visit-bounds ty type-rec)))

(add-default-fold-case DottedPretype
                       (fn [ty _]
                         (-> ty
                           (update-in [:pre-type] type-rec))))

(add-default-fold-case Function
                       (fn [ty _]
                         ;(prn "fold Function" ty)
                         (-> ty
                           (update-in [:dom] #(mapv type-rec %))
                           (update-in [:rng] type-rec)
                           (update-in [:rest] #(when %
                                                 (type-rec %)))
                           (update-in [:drest] #(when %
                                                  (-> %
                                                    (update-in [:pre-type] type-rec)))))))

(add-default-fold-case JSNominal
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %))))))

(add-default-fold-case RClass 
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           #_(update-in [:replacements] #(into {} (for [[k v] %]
                                                                  [k (type-rec v)])))
                           #_(update-in [:unchecked-ancestors] #(->> %
                                                                (map type-rec)
                                                                set)))))

(add-default-fold-case App
                       (fn [ty _]
                         (-> ty
                           (update-in [:rator] type-rec)
                           (update-in [:rands] #(mapv type-rec %)))))

(add-default-fold-case TApp
                       (fn [ty _]
                         (-> ty
                           (update-in [:rator] type-rec)
                           (update-in [:rands] #(mapv type-rec %)))))

(add-default-fold-case PrimitiveArray
                       (fn [ty _]
                         (-> ty
                           (update-in [:input-type] type-rec)
                           (update-in [:output-type] type-rec))))

(add-default-fold-case DataType
                       (fn [ty _]
                         ;(prn "datatype default" (clojure.core.typed.parse-unparse/unparse-type ty))
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:fields] (fn [fs]
                                                  (apply array-map
                                                         (apply concat
                                                                (for [[k v] fs]
                                                                  [k (type-rec v)]))))))))

(add-default-fold-case Protocol
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                             ;FIXME this should probably be left alone in fold
                             ; same in promote/demote
                           (update-in [:methods] (fn [ms]
                                                   (into {}
                                                         (for [[k v] ms]
                                                           [k (type-rec v)])))))))

(add-default-fold-case TypeFn
                       (fn [^TypeFn ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (c/TypeFn-body* names ty)
                               bbnds (c/TypeFn-bbnds* names ty)]
                           (c/TypeFn* names 
                                    (.variances ty)
                                    (mapv #(r/visit-bounds % type-rec) bbnds)
                                    (type-rec body)))))


(add-default-fold-case Poly
                       (fn [^Poly ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (c/Poly-body* names ty)
                               bbnds (c/Poly-bbnds* names ty)]
                           (c/Poly* names 
                                  (mapv #(r/visit-bounds % type-rec) bbnds)
                                  (type-rec body)
                                  (c/Poly-free-names* ty)))))

(add-default-fold-case PolyDots
                       (fn [^PolyDots ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (c/PolyDots-body* names ty)
                               bbnds (c/PolyDots-bbnds* names ty)]
                           (c/PolyDots* names 
                                        (mapv #(r/visit-bounds % type-rec) bbnds)
                                        (type-rec body)
                                        (.actual-frees ty)))))

(add-default-fold-case Mu
                       (fn [ty _]
                         (let [name (gensym)
                               body (c/Mu-body* name ty)]
                           (c/Mu* name (type-rec body)))))

(add-default-fold-case HeterogeneousVector
                       (fn [{:keys [types fs objects rest drest] :as ty} _]
                         (r/-hvec
                           (mapv type-rec (:types ty))
                           :filters (mapv filter-rec (:fs ty))
                           :objects (mapv object-rec (:objects ty))
                           :rest (when rest (type-rec rest))
                           :drest (when drest (update-in drest [:pre-type] type-rec)))))

(add-default-fold-case HeterogeneousList 
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousSeq
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousMap
                       (fn [ty _]
                         (-> ty 
                           (update-in [:types] #(into {} (for [[k v] %]
                                                           [(type-rec k) (type-rec v)]))))))

(add-default-fold-case Extends
                       (fn [{:keys [extends without] :as ty} _]
                         (c/-extends
                           (doall (map type-rec extends))
                           :without (doall (mapv type-rec without)))))


(def ret-first (fn [a & rest] a))

(add-default-fold-case CountRange ret-first)
(add-default-fold-case Name ret-first)
(add-default-fold-case Value ret-first)
(add-default-fold-case Top ret-first)
(add-default-fold-case TCError ret-first)
(add-default-fold-case TopFunction ret-first)
(add-default-fold-case B ret-first)
(add-default-fold-case F ret-first)

(add-default-fold-case Result 
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec)
                           (update-in [:flow] filter-rec))))

(defmacro ret-first-many [& cls]
  `(do ~@(map #(list `add-default-fold-case % `ret-first) cls)))

; CLJS types

(ret-first-many NumberCLJS IntegerCLJS ObjectCLJS StringCLJS BooleanCLJS)

(add-default-fold-case ArrayCLJS
                       (fn [ty _]
                         (-> ty
                           (update-in [:input-type] type-rec)
                           (update-in [:output-type] type-rec))))

;filters

(add-default-fold-case NoFilter ret-first)
(add-default-fold-case TopFilter ret-first)
(add-default-fold-case BotFilter ret-first)

(add-default-fold-case TypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (doall (map pathelem-rec %)))))))

(add-default-fold-case NotTypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (doall (map pathelem-rec %)))))))

(add-default-fold-case ImpFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:a] filter-rec)
                           (update-in [:c] filter-rec))))

(add-default-fold-case AndFilter
                       (fn [^AndFilter ty _]
                         (apply fops/-and
                                (map filter-rec (.fs ty)))))

(add-default-fold-case OrFilter
                       (fn [^OrFilter ty _]
                         (apply fops/-or
                                (map filter-rec (.fs ty)))))

(add-default-fold-case FilterSet
                       (fn [^FilterSet ty _]
                         (fops/-FS
                           (filter-rec (.then ty))
                           (filter-rec (.else ty)))))

(add-default-fold-case FlowSet
                       (fn [^FlowSet ty _]
                         (r/-flow (filter-rec (.normal ty)))))


;objects
(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty _]
                         (-> ty
                           (update-in [:path] #(when %
                                                 (mapv pathelem-rec %))))))
(add-default-fold-case NoObject ret-first)

;path-elems

(add-default-fold-case KeyPE ret-first)
(add-default-fold-case KeysPE ret-first)
(add-default-fold-case ValsPE ret-first)
(add-default-fold-case ClassPE ret-first)

;TCResult

(add-default-fold-case TCResult
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec)
                           (update-in [:flow] filter-rec))))
