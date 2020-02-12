;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; support for assoc/merge/conj
(ns clojure.core.typed.checker.jvm.assoc-utils
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.core.typed.checker.indirect-utils :as ind-u]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.tvar-bnds :as bnds]
            [clojure.set :as set]
            [clojure.core.typed.current-impl :as impl])
  (:import (clojure.core.typed.checker.type_rep HeterogeneousMap Value Intersection F RClass DataType HSequential)
           (clojure.lang IPersistentMap IPersistentVector)))

;supporting assoc functionality

(declare assoc-type-pairs)

(defprotocol AssocableType
  (-assoc-pair [left kv]))

(extend-protocol AssocableType
  Intersection
  (-assoc-pair
    [old-i assoc-entry]
    ; attempt to simplify the intersection before recursing. Parsing an
    ; intersection type does not simplify it.
    (let [new-i (apply c/In (:types old-i))]
      (if (r/Intersection? new-i)
        (apply c/In (keep #(assoc-type-pairs % assoc-entry) (:types new-i)))
        (assoc-type-pairs new-i assoc-entry))))

  ; use the upper bound if bounds below (Map t/Any t/Any)
  F
  (-assoc-pair
    [{:keys [name] :as f} assoc-entry]
    (let [bnd (free-ops/free-with-name-bnds name)
          _ (when-not bnd
              (err/int-error (str "No bounds for type variable: " name bnds/*current-tvar-bnds*)))]
      (when (ind/subtype? (:upper-bound bnd)
                          (impl/impl-case
                            :clojure (c/RClass-of IPersistentMap [r/-any r/-any])
                            :cljs (c/Protocol-of 'cljs.core/IMap [r/-any r/-any])))
        (r/AssocType-maker f [(mapv r/ret-t assoc-entry)] nil))))

  Value
  (-assoc-pair
   [v [kt vt]]
   (when (ind/subtype? v r/-nil)
     (let [rkt (-> kt :t c/fully-resolve-type)]
       (if (c/keyword-value? rkt)
         (c/-complete-hmap {rkt (:t vt)})
         (impl/impl-case
           :clojure (c/RClass-of IPersistentMap [rkt (:t vt)])
           :cljs (c/Protocol-of 'cljs.core/IMap [rkt (:t vt)]))))))
  
  RClass
  (-assoc-pair
   [rc [kt vt]]
   (let [_ (impl/assert-clojure)
         rkt (-> kt :t c/fully-resolve-type)]
     (cond
       (= (:the-class rc) 'clojure.lang.IPersistentMap)
       (c/RClass-of IPersistentMap [(c/Un (:t kt) (nth (:poly? rc) 0))
                                    (c/Un (:t vt) (nth (:poly? rc) 1))])

       (and (= (:the-class rc) 'clojure.lang.IPersistentVector)
            (r/Value? rkt))
       (let [kt ^Value rkt]
         (when (integer? (.val kt))
           (c/RClass-of IPersistentVector [(c/Un (:t vt) (nth (:poly? rc) 0))])))

       (and (= (:the-class rc) 'clojure.lang.IPersistentVector)
            (ind/subtype? rkt (r/Name-maker 'clojure.core.typed/Int)))
       (c/RClass-of IPersistentVector [(c/Un (:t vt) (nth (:poly? rc) 0))]))))
  
  HeterogeneousMap
  (-assoc-pair
   [hmap [kt vt]]
   (let [rkt (-> kt :t c/fully-resolve-type)]
     (if (c/keyword-value? rkt)
       (c/make-HMap
         :mandatory (assoc-in (:types hmap) [rkt] (:t vt))
         :optional (:optional hmap)
         :absent-keys (-> (:absent-keys hmap) 
                          (disj rkt))
         :complete? (c/complete-hmap? hmap))
       ; devolve the map
       ;; todo: probably some machinery I can reuse here?
       (let [ks (apply c/Un (concat [rkt] (mapcat keys [(:types hmap) (:optional hmap)])))
             vs (apply c/Un (concat [(:t vt)] (mapcat vals [(:types hmap) (:optional hmap)])))]
         (impl/impl-case
           :clojure (c/RClass-of IPersistentMap [ks vs])
           :cljs (c/Protocol-of 'cljs.core/IMap [ks vs]))))))
  
  HSequential
  (-assoc-pair
   [v [kt vt]]
   (when (r/HeterogeneousVector? v)
     (let [rkt (-> kt :t c/fully-resolve-type)]
       (when (r/Value? rkt)
         (let [kt rkt
               k (:val kt)] 
           (when (and (integer? k) (<= k (count (:types v))))
             (r/-hvec (assoc (:types v) k (:t vt))
                      :filters (assoc (:fs v) k (:fl vt))
                      :objects (assoc (:objects v) k (:o vt)))))))))
  
  DataType
  (-assoc-pair
   [dt [kt vt]]
   (let [rkt (-> kt :t c/fully-resolve-type)]
     (when (and (r/Record? dt) (c/keyword-value? rkt))
       (let [^Value kt rkt
             field-type (when (c/keyword-value? kt)
                          (get (.fields dt) (symbol (name (.val kt)))))]
         (when (and field-type (ind/subtype? (:t vt) field-type))
           dt))))))

(defn assoc-type-pairs [t & pairs]
  {:pre [(r/Type? t)
         (every? (fn [[k v :as kv]]
                   (and (= 2 (count kv))
                        (r/TCResult? k)
                        (r/TCResult? v)))
                 pairs)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform -assoc-pair t pairs
                           :when #(satisfies? AssocableType %)))

(defn assoc-pairs-noret [t & pairs]
  {:pre [(r/Type? t)
         (every? (fn [[k v :as kv]]
                   (and (= 2 (count kv))
                        (r/Type? k)
                        (r/Type? v)))
                 pairs)]
   :post [((some-fn nil? r/Type?) %)]}
  (apply assoc-type-pairs t (map (fn [[k v]] [(r/ret k) (r/ret v)]) pairs)))

(ind-u/add-indirection ind/assoc-pairs-noret assoc-pairs-noret)

; dissoc support functions
(defn- -dissoc-key [t k]
  {:pre [(r/Type? t)
         (r/TCResult? k)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/union-or-nil
    (for [rtype (c/resolved-type-vector k)]
      (cond
        (ind/subtype? t r/-nil)
        t

        (and (r/HeterogeneousMap? t) (c/keyword-value? rtype))
        (c/make-HMap
          :mandatory
          (dissoc (:types t) rtype)
          :optional
          (dissoc (:optional t) rtype)
          :absent-keys
          (conj (:absent-keys t) rtype)
          :complete? (c/complete-hmap? t))

        (ind/subtype? t (impl/impl-case
                          :clojure (c/RClass-of IPersistentMap [r/-any r/-any])
                          :cljs (c/Protocol-of 'cljs.core/IMap [r/-any r/-any])))
        t))))

(defn dissoc-keys [t ks]
  {:post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform -dissoc-key t ks))

; merge support functions
(defn- merge-hmaps
  "Merges two HMaps into one, right into left.
  
  Preserves all key information where possible, missing keys in a right hand incomplete
  map will erase type information for those keys in the left.
  
  This strategy allows a merge of HMaps to always stay an HMap, without having to drop
  down to an IPersistentMap.
  
  For example:
  (merge '{:a 4 :b 6} '{:b 5}) -> '{:a t/Any :b 5}"
  [left right]
  {:pre [(r/HeterogeneousMap? left)
         (r/HeterogeneousMap? right)]}
  ;; want to know how often complete HMap's help with merging.
  (u/trace-when (c/complete-hmap? right)
    "Merge: complete used on the right")
  (c/make-HMap
    :mandatory
      (let [m (:types left)
            ; optional keys on the right may or may not overwrite mandatory
            ; entries, so we union the common mandatory and optional val types together.
            ;
            ; eg. (merge (HMap :mandatory {:a Number}) (HMap :optional {:a t/Sym}))
            ;     => (HMap :mandatory {:a (t/U Number t/Sym)})
            m (merge-with c/Un 
                          m 
                          (select-keys (:optional right) (keys (:types left))))
            ;_ (prn "after first mandatory pass" m)

            ; combine left+right mandatory entries. 
            ; If right is partial, we can only update the entries common to both
            ; and give any entries type Any.
            ;
            ; eg. (merge (HMap :mandatory {:a Number}) (HMap :mandatory {:b Number}))
            ;     ;=> (HMap :mandatory {:a t/Any :b Number})
            ;
            ; If right is complete, it's safe to merge both mandatory maps.
            ; right-most wins on duplicates.
            m (merge m 
                     (cond
                       (c/partial-hmap? right)
                         (merge (:types right)
                                (zipmap (set/difference 
                                          (set (keys (:types left)))
                                          (set (keys (:types right)))
                                          (set (keys (:optional right)))
                                          (:absent-keys right))
                                        (repeat r/-any)))
                       :else
                        (:types right)))]
        ;(prn "after final mandatory pass" m)
        m)
    :optional
      (let [o (:optional left)
            ;_ (prn "before first optional pass" o)
            ; dissoc keys that end up in the mandatory map
            o (apply dissoc o 
                     (concat (keys (:types right))
                             ; entries mandatory on the left and optional
                             ; on the right are always in the mandatory map
                             (set/intersection 
                               (set (keys (:optional right)))
                               (set (keys (:types left))))))
            ;_ (prn "after first optional pass" o)
            ; now we merge any new :optional entries
            o (merge-with c/Un 
                          o
                          ; if the left is partial then we only add optional entries
                          ; common to both maps.
                          ; if left is complete, we are safe to merge both maps.
                          ;
                          ; (merge (HMap :optional {:a Number}) 
                          ;        (HMap :optional {:b Number}))
                          ; => (HMap)
                          ;
                          ; (merge (HMap :mandatory {:a '5})
                          ;        (HMap :optional {:a '10}))
                          ; => (HMap :mandatory {:a (t/U '5 '10)})
                          ;
                          ; (merge (HMap :optional {:a Number}) 
                          ;        (HMap :optional {:a t/Sym}))
                          ; => (HMap :optional {:a (t/U Number t/Sym)})
                          ;
                          ; (merge (HMap :optional {:a Number}) 
                          ;        (HMap :optional {:b Number} :complete? true))
                          ; => (HMap :optional {:a Number :b Number})
                          ;
                          ; (merge (HMap :optional {:a Number} :complete? true) 
                          ;        (HMap :optional {:b Number}))
                          ; => (HMap :optional {:a Number :b Number})
                          ;
                          ; (merge (HMap :optional {:a Number} :complete? true) 
                          ;        (HMap :optional {:b Number} :complete? true))
                          ; => (HMap :optional {:a Number :b Number})
                          (select-keys (:optional right) 
                                       (set/difference 
                                         (set (keys (:optional right)))
                                         ;remove keys that will be mandatory in the result
                                         (set (keys (:types left)))
                                         (if (c/partial-hmap? left)
                                           ; remove keys that give no new information.
                                           ; If left is partial, we remove optional
                                           ; keys in right that are not mentioned in left.
                                           (set/difference
                                             (set (keys (:optional right)))
                                             (set (keys (:types left)))
                                             (set (keys (:optional left)))
                                             (:absent-keys left))
                                           #{}))))]
        ;(prn "after final optional pass" o)
        o)
    :absent-keys
      (cond 
        ; (merge (HMap :absent-keys [:a :b :c]) (HMap :optional {:a Foo} :mandatory {:b Bar} :absent-keys [:c]))
        ; => (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar})
        ; (merge (HMap :absent-keys [:a :b :c]) (HMap :optional {:a Foo} :mandatory {:b Bar}))
        ; => (HMap :absent-keys [] :optional {:a Foo} :mandatory {:b Bar})
        (and (c/partial-hmap? left) 
             (c/partial-hmap? right))
          (set/intersection
            (set/difference (:absent-keys left)
                            (set (keys (:optional right)))
                            (set (keys (:types right))))
            (:absent-keys right))

        ; (merge (HMap :absent-keys [:a :b :c]) 
        ;        (HMap :optional {:a Foo} :mandatory {:b Bar} :complete? true))
        ; => (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar})
        (and (c/partial-hmap? left) 
             (c/complete-hmap? right))
          (set/difference (:absent-keys left)
                          (set (keys (:optional right)))
                          (set (keys (:types right))))

        ; (merge (HMap :complete? true)
        ;        (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar}))
        ; => (HMap :absent-keys [:c] :optional {:a Foo} :mandatory {:b Bar})
        (and (c/complete-hmap? left)
             (c/partial-hmap? right))
          (:absent-keys right)

        ; (merge (HMap :absent-keys [:a :b :c] :complete? true) 
        ;        (HMap :optional {:a Foo} :mandatory {:b Bar} :absent-keys [:c] :complete? true))
        ; => (HMap :optional {:a Foo} :mandatory {:b Bar} :complete? true)
        (and (c/complete-hmap? left) 
             (c/complete-hmap? right))
          #{}
        :else (throw (Exception. "should never get here")))
    :complete?
      (and (c/complete-hmap? left)
           (c/complete-hmap? right))))

(defn- merge-pair
  [left right]
  {:pre [(r/Type? left)
         (r/TCResult? right)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [left-map (ind/subtype? left (impl/impl-case
                                  :clojure (c/RClass-of IPersistentMap [r/-any r/-any])
                                  :cljs (c/Protocol-of 'cljs.core/IMap [r/-any r/-any])))
        right-map (ind/subtype? (r/ret-t right) (impl/impl-case
                                            :clojure (c/RClass-of IPersistentMap [r/-any r/-any])
                                            :cljs (c/Protocol-of 'cljs.core/IMap [r/-any r/-any])))]
    (cond
     ; preserve the rhand alias when possible
     (and (ind/subtype? left r/-nil) right-map)
     (r/ret-t right)
     
     :else
     (c/union-or-nil
      (for [rtype (c/resolved-type-vector (r/ret-t right))]
        (cond
         (and (or left-map (ind/subtype? left r/-nil))
              (ind/subtype? rtype r/-nil))
           left
         
         (and (ind/subtype? left r/-nil) 
              (ind/subtype? rtype (impl/impl-case
                                :clojure (c/RClass-of IPersistentMap [r/-any r/-any])
                                :cljs (c/Protocol-of 'cljs.core/IMap [r/-any r/-any]))))
           rtype
         
         (and (r/HeterogeneousMap? left) (r/HeterogeneousMap? rtype))
           (merge-hmaps left rtype)
         
         (and (not (ind/subtype? left (impl/impl-case
                                    :clojure (c/RClass-of IPersistentVector [r/-any])
                                    :cljs (c/Protocol-of 'cljs.core/IVector [r/-any]))))
              (satisfies? AssocableType left)
              (r/HeterogeneousMap? rtype))
         (do
           ;TODO
           (assert (empty? (:optional rtype)))
           (apply assoc-type-pairs left (map (fn [[k t]]
                                               [(r/ret k) (r/ret t)])
                                             (:types rtype))))))))))

(defn merge-types [left & r-tcresults]
  {:pre [(r/Type? left)
         (every? r/TCResult? r-tcresults)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform merge-pair left r-tcresults))

; conj helper

(defn- conj-pair [left right]
  {:pre [(r/Type? left)
         (r/TCResult? right)]
   :post [((some-fn nil? r/TCResult?) right)]}
  (cond
    (r/HeterogeneousVector? left)
    (assoc-type-pairs left [(r/ret (r/-val (count (:types left))))
                            right])

    (ind/subtype? left r/-nil)
    (r/-hvec [(:t right)]
             :filters [(:fl right)]
             :objects [(:o right)])

    ; other rules need to unwrap the rhs
    :else
    (c/union-or-nil
      (for [rtype (c/resolved-type-vector right)]
        (cond
          (and (r/HeterogeneousMap? left)
               (r/HeterogeneousVector? rtype))
          (if (= (count (:types rtype)) 2)
            (assoc-type-pairs left (map r/ret (:types rtype)))
            (err/int-error "Need vector of length 2 to conj to map"))

          (and (r/HeterogeneousMap? left)
               (ind/subtype? rtype r/-nil))
          left)))))

(defn conj-types [left & rtypes]
  {:pre [(r/Type? left)
         (every? r/TCResult? rtypes)]
   :post [((some-fn nil? r/Type?) %)]}
  (c/reduce-type-transform conj-pair left rtypes))
