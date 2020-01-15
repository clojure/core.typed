;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.fold-default
  (:require [clojure.core.typed.checker.fold-rep :refer [add-default-fold-case]]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.filter-rep]
            [clojure.core.typed.checker.filter-ops :as fops]
            [clojure.core.typed.checker.object-rep]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.jvm.assoc-utils :as assoc-u]
            [clojure.core.typed.checker.path-rep])
  (:import (clojure.core.typed.checker.type_rep NotType DifferenceType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass JSNominal App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Unchecked TopFunction B F Result
                                        TCResult TCError FlowSet Extends
                                        JSNumber CLJSInteger JSObject JSString ArrayCLJS
                                        JSBoolean AssocType GetType KwArgsSeq HSequential HSet
                                        JSUndefined JSNull JSSymbol JSObj TypeOf SymbolicClosure)
           (clojure.core.typed.checker.filter_rep NoFilter TopFilter BotFilter TypeFilter NotTypeFilter
                                          ImpFilter AndFilter OrFilter FilterSet)
           (clojure.core.typed.checker.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.checker.path_rep KeyPE KeysPE ValsPE ClassPE NthPE CountPE KeywordPE)))

(add-default-fold-case NotType
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec))))

(add-default-fold-case DifferenceType
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:without] #(mapv type-rec %)))))

(add-default-fold-case Intersection
                       (fn [ty _]
                         ;(prn "fold-default Intersection" ty)
                         (let [ts (mapv type-rec (:types ty))]
                           ; don't simplify types in case some types aren't defined yet
                           (c/make-Intersection ts))))

(add-default-fold-case Union 
                       (fn [ty _]
                         ;(prn "union default" (clojure.core.typed.checker.jvm.parse-unparse/unparse-type ty))
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
                                                    (update-in [:pre-type] type-rec))))
                           (update-in [:prest] #(when %
                                                  (let [t (type-rec %)]
                                                    ;; if we fully flatten out the prest, we're left
                                                    ;; with no prest
                                                    (if (= r/-nothing t)
                                                      nil
                                                      t)))))))

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
                         ;(prn "datatype default" (clojure.core.typed.checker.jvm.parse-unparse/unparse-type ty))
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
                       (fn [ty _]
                         (let [names (c/TypeFn-fresh-symbols* ty)
                               body (c/TypeFn-body* names ty)
                               bbnds (c/TypeFn-bbnds* names ty)
                               bmap (zipmap (map r/make-F names) bbnds)]
                           (c/TypeFn* names 
                                      (:variances ty)
                                      (free-ops/with-bounded-frees bmap
                                        (mapv #(r/visit-bounds % type-rec) bbnds))
                                      (free-ops/with-bounded-frees bmap
                                        (type-rec body))))))


(add-default-fold-case Poly
                       (fn [ty _]
                         (let [names (c/Poly-fresh-symbols* ty)
                               body (c/Poly-body* names ty)
                               bbnds (c/Poly-bbnds* names ty)
                               bmap (zipmap (map r/make-F names) bbnds)]
                           (c/Poly* names 
                                    (free-ops/with-bounded-frees bmap
                                      (mapv #(r/visit-bounds % type-rec) bbnds))
                                    (free-ops/with-bounded-frees bmap
                                      (type-rec body))
                                    :named (:named ty)))))

(add-default-fold-case PolyDots
                       (fn [ty _]
                         (let [names (c/PolyDots-fresh-symbols* ty)
                               body (c/PolyDots-body* names ty)
                               bbnds (c/PolyDots-bbnds* names ty)
                               ; don't scope the dotted bound
                               bmap (zipmap (map r/make-F (rest names)) (rest bbnds))]
                           (c/PolyDots* names 
                                        (free-ops/with-bounded-frees bmap
                                          (mapv #(r/visit-bounds % type-rec) bbnds))
                                        (free-ops/with-bounded-frees bmap
                                          (type-rec body))
                                        :named (:named ty)))))

(add-default-fold-case Mu
                       (fn [ty _]
                         (let [name (c/Mu-fresh-symbol* ty)
                               body (c/Mu-body* name ty)]
                           (c/Mu* name (type-rec body)))))

(add-default-fold-case HSequential 
                       (fn [{:keys [types rest drest repeat kind] :as ty} _]
                         (r/-hsequential
                           (mapv type-rec (:types ty))
                           :filters (mapv filter-rec (:fs ty))
                           :objects (mapv object-rec (:objects ty))
                           :rest (when rest (type-rec rest))
                           :drest (when drest (update-in drest [:pre-type] type-rec))
                           :repeat repeat
                           :kind kind)))

(add-default-fold-case HSet
                       (fn [{:keys [fixed] :as ty} _]
                         (r/-hset (set (map type-rec fixed)))))

(defn visit-type-map [m f]
  (into {} (for [[k v] m]
             [(f k) (f v)])))

(add-default-fold-case HeterogeneousMap
                       (fn [ty _]
                         (-> ty 
                           (update-in [:types] visit-type-map type-rec)
                           (update-in [:optional] visit-type-map type-rec))))

(add-default-fold-case JSObj
                       (fn [ty _]
                         (-> ty 
                           (update-in [:types] #(zipmap (keys %) (map type-rec (vals %)))))))

(add-default-fold-case KwArgsSeq
                       (fn [ty _]
                         (-> ty 
                           (update-in [:mandatory] visit-type-map type-rec)
                           (update-in [:optional] visit-type-map type-rec))))

(add-default-fold-case Extends
                       (fn [{:keys [extends without] :as ty} _]
                         (c/-extends
                           (doall (map type-rec extends))
                           :without (doall (mapv type-rec without)))))

(add-default-fold-case GetType
                       (fn [ty _]
                         (-> ty
                           (update-in [:target] type-rec)
                           (update-in [:key] type-rec)
                           (update-in [:not-found] type-rec)
                           (update-in [:target-fs] filter-rec)
                           (update-in [:target-object] object-rec))))

(add-default-fold-case AssocType
                       (fn [{:keys [target entries dentries] :as ty} _]
                         (let [s-target (type-rec target)
                               s-entries (doall
                                           (for [[k v] entries]
                                             [(type-rec k) (type-rec v)]))
                               s-dentries (when dentries (type-rec dentries))
                               fallback-r (-> ty
                                            (assoc-in [:target] s-target)
                                            (assoc-in [:entries] s-entries)
                                            (assoc-in [:dentries] s-dentries))]
                           (if dentries
                             fallback-r
                             (if-let [assoced (apply assoc-u/assoc-pairs-noret s-target s-entries)]
                               assoced
                               fallback-r)))))

(def ret-first (fn [a & rest] a))

(add-default-fold-case CountRange ret-first)
(add-default-fold-case Name ret-first)
(add-default-fold-case Value ret-first)
(add-default-fold-case Top ret-first)
(add-default-fold-case Unchecked ret-first)
(add-default-fold-case TCError ret-first)
(add-default-fold-case TopFunction ret-first)
(add-default-fold-case B ret-first)
(add-default-fold-case F ret-first)
(add-default-fold-case TypeOf ret-first)
(add-default-fold-case SymbolicClosure ret-first)

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

(ret-first-many JSNumber CLJSInteger JSObject JSString JSBoolean JSUndefined
                JSNull JSSymbol)

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
(add-default-fold-case NthPE ret-first)
(add-default-fold-case CountPE ret-first)
(add-default-fold-case KeywordPE ret-first)

;TCResult

(add-default-fold-case TCResult
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec)
                           (update-in [:flow] filter-rec))))
