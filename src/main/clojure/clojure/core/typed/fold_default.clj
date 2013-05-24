(ns clojure.core.typed.fold-default
  (:require [clojure.core.typed
             [fold-rep :refer [add-default-fold-case]]
             [type-rep :as r]
             [type-ctors :as c]
             [filter-rep]
             [object-rep]
             [path-rep]])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        Projection DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result
                                        HeterogeneousSeq TCResult TCError)
           (clojure.core.typed.filter_rep NoFilter TopFilter BotFilter TypeFilter NotTypeFilter
                                          ImpFilter AndFilter OrFilter FilterSet)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE)))

(add-default-fold-case NotType
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec))))

(add-default-fold-case Intersection
                       (fn [ty _]
                         (apply c/In (mapv type-rec (:types ty)))))

(add-default-fold-case Union 
                       (fn [ty _]
                         (apply c/Un (mapv type-rec (:types ty)))))

(add-default-fold-case FnIntersection
                       (fn [ty _]
                         (-> ty
                           (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case Bounds
                       (fn [ty _]
                         (r/visit-bounds ty type-rec)))

(add-default-fold-case Projection
                       (fn [ty _]
                         (-> ty
                           (update-in [:ts] #(mapv type-rec %)))))

(add-default-fold-case DottedPretype
                       (fn [ty _]
                         (-> ty
                           (update-in [:pre-type] type-rec))))

(add-default-fold-case Function
                       (fn [ty _]
                         (-> ty
                           (update-in [:dom] #(mapv type-rec %))
                           (update-in [:rng] type-rec)
                           (update-in [:rest] #(when %
                                                 (type-rec %)))
                           (update-in [:drest] #(when %
                                                  (-> %
                                                    (update-in [:pre-type] type-rec)))))))

(add-default-fold-case RClass 
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:replacements] #(into {} (for [[k v] %]
                                                                  [k (type-rec v)])))
                           (update-in [:unchecked-ancestors] #(->> %
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
                         #_(prn (clojure.core.typed.parse-unparse/unparse-type ty))
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
                                      (type-rec body)))))

(add-default-fold-case Mu
                       (fn [ty _]
                         (let [name (gensym)
                               body (c/Mu-body* name ty)]
                           (c/Mu* name (type-rec body)))))

(add-default-fold-case HeterogeneousVector
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

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
                           (update-in [:o] object-rec))))


;filters

(add-default-fold-case NoFilter ret-first)
(add-default-fold-case TopFilter ret-first)
(add-default-fold-case BotFilter ret-first)

(add-default-fold-case TypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (map pathelem-rec %))))))

(add-default-fold-case NotTypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (map pathelem-rec %))))))

(add-default-fold-case ImpFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:a] filter-rec)
                           (update-in [:c] filter-rec))))

(add-default-fold-case AndFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:fs] #(set (map filter-rec %))))))

(add-default-fold-case OrFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:fs] #(set (map filter-rec %))))))

(add-default-fold-case FilterSet
                       (fn [ty _]
                         (-> ty
                           (update-in [:then] filter-rec)
                           (update-in [:else] filter-rec))))

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

;TCResult

(add-default-fold-case TCResult
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec)
                           (update-in [:flow :normal] filter-rec))))
