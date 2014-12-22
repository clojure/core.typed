(ns ^:skip-wiki clojure.core.typed.check.map
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.type-ctors :as c])
  (:import (clojure.lang APersistentMap)))

(alter-meta! *ns* assoc :skip-wiki true)

;(ann expected-vals [(Coll Type) (Nilable TCResult) -> (Coll (Nilable TCResult))])
(defn expected-vals
  "Returns a sequence of (Nilable TCResults) to use as expected types for type
  checking the values of a literal map expression"
  [key-types expected]
  {:pre [(every? r/Type? key-types)
         ((some-fn r/TCResult? nil?) expected)]
   :post [(every? (some-fn nil? r/TCResult?) %)
          (= (count %)
             (count key-types))]}
  (let [expected (when expected 
                   (c/fully-resolve-type (r/ret-t expected)))
        flat-expecteds (when expected
                         (mapv c/fully-resolve-type
                               (if (r/Union? expected)
                                 (:types expected)
                                 [expected])))
        no-expecteds (repeat (count key-types) nil)]
    (cond 
      ; If every key in this map expression is a Value, let's try and
      ; make use of our expected type for each individual entry. This is
      ; most useful for `extend`, where we have HMaps of functions, and
      ; we want to forward the expected types to each val, a function.
      ;
      ; The expected type also needs to be an expected shape:
      ; - a HMap or union of just HMaps
      ; - each key must have exactly one possible type if it is
      ;   present
      ; - if a key is absent in a HMap type, it must be explicitly
      ;   absent
      (and expected 
           (every? r/Value? key-types)
           (every? r/HeterogeneousMap? flat-expecteds))
        (let [hmaps flat-expecteds]
          (reduce (fn [val-expecteds ktype]
                    ; also includes this contract wrapped in a Reduced
                    ; The post-condition for `expected-vals` catches this anyway
                    ;{:post [(every? (some-fn nil? r/TCResult?) %)]}

                    (let [; find the ktype key in each hmap.
                          ; - If ktype is present in mandatory or optional then we either use the entry's val type 
                          ; - otherwise if ktype is explicitly forbidden via :absent-keys or completeness
                          ;   we skip the entry.
                          ; - otherwise we give up and don't check this as a hmap, return nil
                          ;   that gets propagated up
                          corresponding-vals 
                          (reduce (fn [corresponding-vals {:keys [types absent-keys optional] :as hmap}]
                                    (if-let [v (some #(get % ktype) [types optional])]
                                      (conj corresponding-vals v)
                                      (cond
                                        (or (contains? absent-keys ktype)
                                            (c/complete-hmap? hmap))
                                          corresponding-vals
                                        :else 
                                          (reduced nil))))
                                  #{} hmaps)
                          val-expect (when (= 1 (count corresponding-vals))
                                       (r/ret (first corresponding-vals)))]
                      (if val-expect
                        (conj val-expecteds val-expect)
                        (reduced no-expecteds))))
                  [] key-types))

      ; If we expect an (IPersistentMap k v), just use the v as the expected val types
      (and (r/RClass? expected)
           (= (:the-class expected) 'clojure.lang.IPersistentMap))
        (let [{[_ vt] :poly?} expected]
          (map r/ret (repeat (count key-types) vt)))
      ; otherwise we don't give expected types
      :else no-expecteds)))

(defn check-map [check {keyexprs :keys valexprs :vals :as expr} expected]
  (let [ckeyexprs (mapv check keyexprs)
        key-types (map (comp r/ret-t u/expr-type) ckeyexprs)

        val-rets
        (expected-vals key-types expected)

        cvalexprs (mapv check valexprs val-rets)
        val-types (map (comp r/ret-t u/expr-type) cvalexprs)

        ts (zipmap key-types val-types)
        actual-t (if (every? c/keyword-value? (keys ts))
                 (c/-complete-hmap ts)
                 (impl/impl-case
                   :clojure (c/RClass-of APersistentMap [(apply c/Un (keys ts))
                                                         (apply c/Un (vals ts))])
                   :cljs (c/Protocol-of 'cljs.core/IMap
                                        [(apply c/Un (keys ts))
                                         (apply c/Un (vals ts))]) ))
        actual-ret (r/ret actual-t (fo/-true-filter))]
    (assoc expr
           :keys ckeyexprs
           :vals cvalexprs
           u/expr-type (below/maybe-check-below actual-ret expected))))
