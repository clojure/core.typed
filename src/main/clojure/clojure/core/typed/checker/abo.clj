;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.abo
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.fold-rep :as fold]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo])
  (:import (clojure.core.typed.checker.filter_rep NotTypeFilter TypeFilter FilterSet)))

(declare abstract-object abstract-type abo)

;[Type (Seqable t/Sym) -> Type]
(defn abstract-type [ids keys t]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (r/AnyType? t)]
   :post [(r/AnyType? %)]}
  ;(prn "abstract type" ids keys t)
  (letfn [(sb-t [t] (abstract-type ids keys t))
          (sb-f [f] (abo ids keys f))
          (sb-o [o] (abstract-object ids keys o))]
    (fold/fold-rhs ::abo
       {:type-rec sb-t
        :filter-rec sb-f
        :object-rec sb-o}
      t)))

;[(Seqable t/Sym) (Seqable AnyInteger) RObject -> RObject]
(defn abstract-object [ids keys o]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (obj/RObject? o)]
   :post [(obj/RObject? %)]}
  ;(prn "abstract-object" ids keys o)
  (letfn [ ; Difference from Typed Racket:
            ;   because abstract-result calls abstract-type, we could have
            ;   already-abstracted filters at this point. We relax the contract
            ;   to allow naturals.
            ;
            ; eg. (ann-form (fn [] (fn [b] b)) [-> [Any -> Any]])
            ;
            ;    In this type the (fn [b] b) is already abstracted as 
            ;      [Any -> Any :filters {:then (! (U nil false) 0), :else (is (U nil false) 0)} :object {:id 0}]
            ;    by the time we call abstract-result.
          (lookup [y]
            {:pre [((some-fn symbol? nat-int?) y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector ids keys)))]
    (cond
      (and (obj/Path? o)
           (lookup (:id o))) (update-in o [:id] lookup)
      :else obj/-empty)))

;[(Seqable t/Sym) (Seqable AnyInteger) (U NoFilter FilterSet) 
;  -> (U NoFilter FilterSet)]
(defn abstract-filter [ids keys fs]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         ((some-fn fl/NoFilter? fl/FilterSet?) fs)]
   :post [((some-fn fl/NoFilter? fl/FilterSet?) %)]}
  ;(prn "abstract filter" ids keys fs)
  (cond
    (fl/FilterSet? fs)
    (let [{fs+ :then fs- :else} fs]
      (fo/-FS (abo ids keys fs+)
              (abo ids keys fs-)))
    (fl/NoFilter? fs) (fo/-FS fl/-top fl/-top)))

;[(Seqable t/Sym) (Seqable AnyInteger) FlowSet
;  -> (U NoFilter FilterSet)]
(defn abstract-flow [ids keys fs]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (r/FlowSet? fs)]
   :post [(r/FlowSet? %)]}
  (r/FlowSet-maker (abo ids keys (:normal fs))))

(fold/derive-default ::abo)

(fold/add-fold-case ::abo
                    TypeFilter
                    (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                      ;if variable goes out of scope, replace filter with fl/-top
                      (if-let [scoped (lookup id)]
                        (fo/-filter type scoped path)
                        fl/-top)))

(fold/add-fold-case ::abo
                    NotTypeFilter
                    (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                      ;if variable goes out of scope, replace filter with fl/-top
                      (if-let [scoped (lookup id)]
                        (fo/-not-filter type scoped path)
                        fl/-top)))

;[(Seqable t/Sym) (Seqable AnyInteger) Filter -> Filter]
(defn abo [xs idxs f]
  {:pre [(every? symbol? xs)
         (every? integer? idxs)
         (fl/Filter? f)]
   :post [(fl/Filter? %)]}
  ;(prn "abo" xs idxs f)
  (letfn [(lookup [y]
            ; Difference from Typed Racket:
            ;   because abstract-result calls abstract-type, we could have
            ;   already-abstracted filters at this point. We relax the contract
            ;   to allow naturals.
            ;
            ; eg. (ann-form (fn [] (fn [b] b)) [-> [Any -> Any]])
            ;
            ;    In this type the (fn [b] b) is already abstracted as 
            ;      [Any -> Any :filters {:then (! (U nil false) 0), :else (is (U nil false) 0)} :object {:id 0}]
            ;    by the time we call abstract-result.
            {:pre [((some-fn symbol? nat-int?) y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector xs idxs)))
          (rec [f] (abo xs idxs f))
          (sb-t [t] (abstract-type xs idxs t))]
    (fold/fold-rhs ::abo
      {:type-rec sb-t 
       :filter-rec rec
       :locals {:lookup lookup}}
      f)))

; Difference from Typed Racket
;
; Here we also abstract types with abstract-type. We have types
; like HSequential that contains Result's, but can also
; appear in arbitrary positions. The combination of these means
; we need to abstract and instantiate all types at function boundaries.

;[TCResult (Seqable t/Sym) -> Result]
(defn abstract-result [result arg-names]
  {:pre [(r/TCResult? result)
         (every? symbol? arg-names)]
   :post [(r/Result? %)]}
  ;(prn "abstract result" result arg-names)
  (let [keys (range (count arg-names))]
    (r/make-Result
      (abstract-type   arg-names keys (r/ret-t result))
      (abstract-filter arg-names keys (r/ret-f result))
      (abstract-object arg-names keys (r/ret-o result))
      (abstract-flow   arg-names keys (r/ret-flow result)))))
