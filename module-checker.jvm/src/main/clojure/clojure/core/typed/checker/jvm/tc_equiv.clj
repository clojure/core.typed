;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.tc-equiv
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.math.combinatorics :as comb]
            [clojure.core.typed.current-impl :as impl]))

(defn equivable [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Type?) %)]}
  (or (when (r/Value? t)
        (when ((some-fn number? symbol? keyword? nil? true? false? class?) (:val t))
          t))
      (impl/impl-case
        :clojure nil
        ; (if (= undefined a)
        ;   .. ; a :- nil
        ;   .. ; a :- (Not nil))
        ; (if (= null a)
        ;   .. ; a :- nil
        ;   .. ; a :- (Not nil))
        :cljs (when ((some-fn r/JSNull? r/JSUndefined?) t)
                (r/-val nil)))))

(defn identicalable [branch t]
  {:pre [(#{:then :else} branch)
         (r/Type? t)]
   :post [((some-fn nil? r/Type?) %)]}
  (or (when (r/Value? t)
        (let [v (:val t)]
          (case branch
            :then (when ((some-fn number? symbol? keyword? nil? true? false? class?) v)
                    t)
            :else (or (when ((some-fn true? false?) v)
                        t)
                      (when (nil? v)
                        (impl/impl-case
                          :clojure t
                          ; when (identical? (ann-form ... nil) a) is false, we can't
                          ; say `a` is non-nil (either arg could be JSUndefined or JSNull).
                          :cljs nil))))))
      (impl/impl-case
        :clojure nil
        :cljs (when ((some-fn r/JSNull? r/JSUndefined?) t)
                t))))

;[Any TCResult * -> TCResult]
(defn tc-equiv [comparator vs expected]
  {:pre [(every? r/TCResult? vs)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (assert (seq vs))
  (let [[then-equivable else-equivable]
        (case comparator
          := [equivable equivable]
          :identical? [#(identicalable :then %)
                       #(identicalable :else %)])
        ; TODO sequence behaviour is subtle
        ; conservative for now
        vs-combinations (comb/combinations vs 2)
        ;_ (prn (count vs-combinations))
        then-filter (apply fo/-and (apply concat
                                          (for [[{t1 :t fl1 :fl o1 :o}
                                                 {t2 :t fl2 :fl o2 :o}] vs-combinations]
                                            (concat
                                              (when-some [t1 (then-equivable t1)]
                                                [(fo/-filter-at t1 o2)])
                                              (when-some [t2 (then-equivable t2)]
                                                [(fo/-filter-at t2 o1)])))))
        ;_ (prn "then" then-filter)
        else-filter (apply fo/-or 
                           (if-let [fs (seq (apply concat
                                                   (for [[{t1 :t fl1 :fl o1 :o}
                                                          {t2 :t fl2 :fl o2 :o}] vs-combinations]
                                                     (concat
                                                       (when-some [t1 (else-equivable t1)]
                                                         ;(prn "else t1" t1 o2 (fo/-not-filter-at t1 o2))
                                                         [(fo/-not-filter-at t1 o2)])
                                                       (when-some [t2 (else-equivable t2)]
                                                         ;(prn "else t2" t2 o1 (fo/-not-filter-at t2 o1))
                                                         [(fo/-not-filter-at t2 o1)])))))]
                             fs
                             ; ensure we don't simplify to ff if we have more than one
                             ; argument to = (1 arg is always a true value)
                             (when (< 1 (count vs))
                               [fl/-top])))
        ;_ (prn "else" else-filter)
        ]
    (below/maybe-check-below
      (r/ret (c/Un r/-false r/-true)
             (fo/-FS then-filter else-filter))
      expected)))
