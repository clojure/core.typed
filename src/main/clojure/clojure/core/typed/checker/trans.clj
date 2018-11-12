;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.trans
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.subst :as subst]
            [clojure.core.typed.checker.fold-rep :as fold]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.object-rep :as or])
  (:import (clojure.core.typed.checker.type_rep HSequential Function AssocType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted pre-type expansion

(derive ::trans-dots fold/fold-rhs-default)

;tdr from Practical Variable-Arity Polymorphism paper
; Expand out dotted pretypes to fixed domain, using types bm, if (:name bound) = b
(defn trans-dots [t b bm]
  (fold/fold-rhs ::trans-dots
                 {:type-rec #(trans-dots % b bm)
                  :locals {:b b :bm bm}}
                 t))

(fold/add-fold-case ::trans-dots
  HSequential
  (fn [{:keys [kind] :as t} {{:keys [b bm]} :locals}]
    (let [tfn #(trans-dots % b bm)]
      (cond
        (:drest t)
        (let [{:keys [pre-type name]} (:drest t)]
          (assert (symbol? name))
          (if (= b name) ;identical bounds
            (let [fixed (vec
                          (concat 
                            ;keep fixed entries
                            (doall (map tfn (:types t)))
                            ;expand dotted type to fixed entries
                            (doall (map (fn [bk]
                                          {:post [(r/Type? %)]}
                                          ;replace free occurences of bound with bk
                                          (-> (subst/substitute bk b pre-type)
                                              tfn))
                                        bm))))
                  extra-fixed (- (count fixed)
                                 (count (:types t)))]
              (r/-hsequential fixed
                              :filters (vec
                                         (concat (map tfn (:fs t))
                                                 (repeat extra-fixed
                                                         (fo/-simple-filter))))
                              :objects (vec
                                         (concat (map tfn (:objects t))
                                                 (repeat extra-fixed
                                                         or/-empty)))
                              ;drest is expanded into fixed
                              :kind kind))
            (r/-hsequential (mapv tfn (:types t))
                            :filters (mapv tfn (:fs t))
                            :objects (mapv tfn (:objects t))
                            :drest (when-let [drest (:drest t)]
                                     (-> drest
                                         (update-in [:pre-type] tfn))) ;translate pre-type
                            :kind kind)))
        :else
        (r/-hsequential (mapv tfn (:types t))
                        :filters (mapv tfn (:fs t))
                        :objects (mapv tfn (:objects t))
                        :rest (when-let [r (:rest t)]
                                (tfn r))
                        :repeat (:repeat t)
                        :kind kind)))))

(fold/add-fold-case ::trans-dots
  AssocType
  (fn [{:keys [target entries dentries]} {{:keys [b bm]} :locals}]
    (let [tfn #(trans-dots % b bm)
          t-target (tfn target)
          t-entries (map (fn [ent]
                           [(tfn (first ent)) (tfn (second ent))])
                         entries)]
      (if (and dentries
               (= b (:name dentries)))
        (r/AssocType-maker t-target
                           (concat t-entries
                                   (->> bm
                                     (map (fn [bk]
                                            {:post [(r/Type? %)]}
                                            (-> (subst/substitute bk b (:pre-type dentries))
                                              tfn)))
                                     (partition 2)
                                     (map vec)))
                           nil)
        (r/AssocType-maker t-target
                           t-entries
                           (when dentries
                             (update-in dentries [:pre-type] tfn)))))))

(fold/add-fold-case ::trans-dots
  Function
  (fn 
    [t {{:keys [b bm]} :locals}]
    (let [tfn #(trans-dots % b bm)]
      (cond
        (:drest t)
        (let [{:keys [pre-type name]} (:drest t)]
          (assert (symbol? name))
          (if (= b name) ;identical bounds
            (let [dom (concat 
                        ;keep fixed domain
                        (doall (map tfn (:dom t)))
                        ;expand dotted type to fixed domain
                        (doall (map (fn [bk]
                                      {:post [(r/Type? %)]}
                                      ;replace free occurences of bound with bk
                                      (-> (subst/substitute bk b pre-type)
                                          tfn))
                                    bm)))]
              (r/Function-maker dom
                                (tfn (:rng t))
                                nil
                                nil
                                nil ;dotted pretype now expanded to fixed domain
                                nil
                                nil))
            (-> t
                (update-in [:dom] #(doall (map tfn %)))
                (update-in [:rng] tfn)
                (update-in [:drest] (fn [drest]
                                      (when drest
                                        (-> drest
                                            (update-in [:pre-type] tfn)))))))) ;translate pre-type
        :else
        (-> t
            (update-in [:dom] #(doall (map tfn %)))
            (update-in [:rng] tfn)
            (update-in [:rest] #(when %
                                  (tfn %)))
            (update-in [:prest] #(when %
                                   (tfn %))))))))
