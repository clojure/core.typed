;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.subst
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.fold-rep :as f]
            [clojure.core.typed.checker.frees :as frees]
            [clojure.core.typed.checker.cs-rep :as crep]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.object-rep :as orep]
            [clojure.core.typed.checker.jvm.assoc-utils :as assoc-u])
  (:import (clojure.core.typed.checker.type_rep F Function HSequential AssocType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(t/tc-ignore
(derive ::substitute f/fold-rhs-default)
(f/add-fold-case ::substitute
               F
               (fn [{name* :name :as f} {{:keys [name image]} :locals}]
                 (if (= name* name)
                   image
                   f)))
  )

(t/ann ^:no-check substitute [r/Type t/Sym r/Type -> r/Type])
(defn substitute [image name target]
  {:pre [(r/AnyType? image)
         (symbol? name)
         (r/AnyType? target)]
   :post [(r/AnyType? %)]}
  (f/fold-rhs ::substitute
              {:locals {:name name
                        :image image}}
              target))

(t/ann ^:no-check substitute-many [r/Type (t/U nil (t/Seqable r/Type)) (t/U nil (t/Seqable t/Sym))
                                 -> r/Type])
(defn substitute-many [target images names]
  (reduce (fn [t [im nme]] (substitute im nme t))
          target
          (map vector images names)))

(declare substitute-dots substitute-dotted)

(t/ann ^:no-check subst-all [crep/SubstMap r/Type -> r/Type])
(defn subst-all [s t]
  {:pre [(crep/substitution-c? s)
         (r/AnyType? t)]
   :post [(r/AnyType? %)]}
  (reduce (fn [t [v r]]
            (cond
              (crep/t-subst? r) (substitute (:type r) v t)
              (crep/i-subst? r) (substitute-dots (:types r) nil v t)
              (crep/i-subst-starred? r) (substitute-dots (:types r) (:starred r) v t)
              (and (crep/i-subst-dotted? r)
                   (empty? (:types r))) (substitute-dotted (:dty r) (:name (:dbound r)) v t)
              (crep/i-subst-dotted? r) (err/nyi-error "i-subst-dotted nyi")
              :else (err/nyi-error (str "Other substitutions NYI"))))
          t s))

;; Substitute dots

(derive ::substitute-dots f/fold-rhs-default)
(f/add-fold-case ::substitute-dots
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot] :as ftype} {{:keys [name sb images rimage]} :locals}]
   (when kws (err/nyi-error "substitute keyword args"))
   (if (and (or drest pdot)
            (= name (:name (or drest pdot))))
     (r/Function-maker (doall
                         (concat (map sb dom)
                                 (if drest
                                   ;; We need to recur first, just to expand out any dotted usages of this.
                                   (let [expanded (sb (:pre-type drest))]
                                     ;(prn "expanded" (unparse-type expanded))
                                     (map (fn [img] (substitute img name expanded)) images))
                                   (let [expandeds (map sb (-> pdot :pre-type :types))
                                         _ (assert (zero? (rem (count images) (count expandeds))))
                                         list-of-images (partition (count expandeds) images)
                                         list-of-result (map (fn [expandeds images]
                                                               (map (fn [expanded img]
                                                                      (substitute img name expanded))
                                                                    expandeds
                                                                    images))
                                                             (repeat expandeds)
                                                             list-of-images)]
                                     (reduce concat list-of-result)))))
                       (sb rng)
                       rimage nil nil nil nil)
     (r/Function-maker (doall (map sb dom))
                       (sb rng)
                       (and rest (sb rest))
                       (and drest (r/DottedPretype1-maker (sb (:pre-type drest))
                                                          (:name drest)))
                       nil
                       (and prest (sb prest))
                       (and pdot (r/DottedPretype1-maker (sb (:pre-type pdot))
                                                         (:name pdot)))))))

(f/add-fold-case ::substitute-dots
  AssocType
  (fn [{:keys [target entries dentries] :as atype} {{:keys [name sb images rimage]} :locals}]
    (let [sb-target (sb target)
          sb-entries (map (fn [ent]
                            [(sb (first ent)) (sb (second ent))])
                          entries)]
      (if (and dentries
               (= name (:name dentries)))
        (let [entries (concat sb-entries
                              (let [expanded (sb (:pre-type dentries))]
                                (->> images
                                  (map (fn [img] (substitute img name expanded)))
                                  (partition 2)
                                  (map vec))))]
          ; try not to use AssocType, because subtype and cs-gen support for it
          ; is not that mature
          (if-let [assoced (apply assoc-u/assoc-pairs-noret sb-target entries)]
            assoced
            (r/AssocType-maker sb-target entries nil)))
        (r/AssocType-maker sb-target
                           sb-entries
                           (and dentries (r/DottedPretype1-maker (sb (:pre-type dentries))
                                                                 (:name dentries))))))))

(f/add-fold-case ::substitute-dots
  HSequential
  (fn [{:keys [types fs objects rest drest kind] :as ftype} {{:keys [name sb images rimage]} :locals}]
    (if (and drest
             (= name (:name drest)))
      (r/-hsequential
        (into (mapv sb types)
              ;; We need to recur first, just to expand out any dotted usages of this.
              (let [expanded (sb (:pre-type drest))]
                (map (fn [img] (substitute img name expanded)) images)))
        :filters (into (mapv sb fs) (repeat (count images) (fo/-FS fl/-top fl/-top)))
        :objects (into (mapv sb objects) (repeat (count images) orep/-empty))
        :kind kind)
      (r/-hsequential
        (mapv sb types)
        :filters (mapv sb fs)
        :objects (mapv sb objects)
        :rest (when rest (sb rest))
        :drest (when drest (r/DottedPretype1-maker (sb (:pre-type drest))
                                                   (:name drest)))
        :repeat (:repeat ftype)
        :kind kind))))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(t/ann ^:no-check substitute-dots [(t/U nil (t/Seqable r/Type)) (t/U nil r/Type) t/Sym r/Type -> r/Type])
(defn substitute-dots [images rimage name target]
  {:pre [(every? r/AnyType? images)
         ((some-fn nil? r/AnyType?) rimage)
         (symbol? name)
         (r/AnyType? target)]}
  ;(prn "substitute-dots" (unparse-type target) name "->" (map unparse-type images))
  (letfn [(sb [t] (substitute-dots images rimage name t))]
    (if (or ((frees/fi target) name)
            ((frees/fv target) name))
      (f/fold-rhs ::substitute-dots 
                {:type-rec sb
                 :filter-rec (f/sub-f sb ::substitute-dots)
                 :locals {:name name
                          :sb sb
                          :images images
                          :rimage rimage}}
                target)
      target)))


(derive ::substitute-dotted f/fold-rhs-default)
(f/add-fold-case ::substitute-dotted
  F
  (fn [{name* :name :as t} {{:keys [name image]} :locals}]
   (if (= name* name)
     image
     t)))

(f/add-fold-case ::substitute-dotted
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot]} {{:keys [sb name image]} :locals}]
   (when kws (err/nyi-error "substitute-dotted with kw arguments"))
   (r/Function-maker (doall (map sb dom))
                     (sb rng)
                     (and rest (sb rest))
                     (and drest
                          (r/DottedPretype1-maker (substitute image (:name drest) (sb (:pretype drest)))
                                                  (if (= name (:name drest))
                                                    name
                                                    (:name drest))))
                     nil
                     (and prest (sb prest))
                     (and pdot
                          (err/nyi-error "NYI pdot of substitute-dotted for Function")))))

(f/add-fold-case ::substitute-dotted
  AssocType
  (fn [{:keys [target entries dentries]} {{:keys [sb name image]} :locals}]
   (r/AssocType-maker (sb target)
                      (into {} (map (fn [ent]
                                      [(sb (first ent)) (sb (second ent))])
                                    entries))
                      (and dentries
                           (r/DottedPretype1-maker (substitute image (:name dentries) (sb (:pretype dentries)))
                                                   (if (= name (:name dentries))
                                                     name
                                                     (:name dentries)))))))

(f/add-fold-case ::substitute-dotted
  HSequential
  (fn [{:keys [types fs objects rest drest kind] :as ftype} {{:keys [sb name image]} :locals}]
    (r/-hsequential
      (mapv sb types)
      :filters (mapv sb fs))
      :objects (mapv sb objects)
      :rest (when rest (sb rest))
      :drest (when drest
               (r/DottedPretype1-maker (substitute image (:name drest) (sb (:pretype drest)))
                                       (if (= name (:name drest))
                                         name
                                         (:name drest))))
      :repeat (:repeat ftype)
      :kind kind))

;; implements curly brace substitution from the formalism
;; substitute-dotted : Type Name Name Type -> Type
(t/ann ^:no-check substitute-dotted [r/Type t/Sym t/Sym r/Type -> r/Type])
(defn substitute-dotted [image image-bound name target]
  {:pre [(r/AnyType? image)
         (symbol? image-bound)
         (symbol? name)
         (r/AnyType? target)]
   :post [(r/AnyType? %)]}
  (letfn [(sb [t] (substitute-dotted image image-bound name t))]
    (if ((frees/fi target) name)
      (f/fold-rhs ::substitute-dotted
                {:type-rec sb 
                 :filter-rec (f/sub-f sb ::substitute-dotted)
                 :locals {:name name
                          :sb sb
                          :image image}}
                target
                target)
      target)))
