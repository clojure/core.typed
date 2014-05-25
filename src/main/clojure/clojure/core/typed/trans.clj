(ns clojure.core.typed.trans
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed.fold-rep :as fold]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.object-rep :as or])
  (:import (clojure.core.typed.type_rep HSequential HeterogeneousVector
                                        Function)))

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
  (fn
    [t {{:keys [b bm]} :locals}]
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
                       ))
            (r/-hsequential (mapv tfn (:types t))
                     :filters (mapv tfn (:fs t))
                     :objects (mapv tfn (:objects t))
                     :drest (when-let [drest (:drest t)]
                              (-> drest
                                  (update-in [:pre-type] tfn)))))) ;translate pre-type
        :else
        (r/-hsequential (mapv tfn (:types t))
                 :filters (mapv tfn (:fs t))
                 :objects (mapv tfn (:objects t))
                 :rest (when-let [r (:rest t)]
                         (tfn r))
                 :repeat (:repeat t))))))

(fold/add-fold-case ::trans-dots
  HeterogeneousVector
  (fn 
    [t {{:keys [b bm]} :locals}]
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
              (r/-hvec fixed
                       :filters (vec
                                  (concat (map tfn (:fs t))
                                          (repeat extra-fixed
                                                  (fo/-simple-filter))))
                       :objects (vec
                                  (concat (map tfn (:objects t))
                                          (repeat extra-fixed
                                                  or/-empty)))
                       ;drest is expanded into fixed
                       ))
            (r/-hvec (mapv tfn (:types t))
                     :filters (mapv tfn (:fs t))
                     :objects (mapv tfn (:objects t))
                     :drest (when-let [drest (:drest t)]
                              (-> drest
                                  (update-in [:pre-type] tfn)))))) ;translate pre-type
        :else
        (r/-hvec (mapv tfn (:types t))
                 :filters (mapv tfn (:fs t))
                 :objects (mapv tfn (:objects t))
                 :rest (when-let [r (:rest t)]
                         (tfn r)))))))

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
                                  (tfn %))))))))
