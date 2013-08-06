(ns clojure.core.typed.subst
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.fold-rep :as f]
            [clojure.core.typed.type-ctors :as tc]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.cs-rep :as crep])
  (:import (clojure.core.typed.type_rep F Function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(derive ::substitute f/fold-rhs-default)

(f/add-fold-case ::substitute
               F
               (fn [{name* :name :as f} {{:keys [name image]} :locals}]
                 (if (= name* name)
                   image
                   f)))

(defn substitute [image name target]
  {:pre [(r/AnyType? image)
         (symbol? name)
         (r/AnyType? target)]
   :post [(r/AnyType? %)]}
  (f/fold-rhs ::substitute
              {:locals {:name name
                        :image image}}
              target))

(defn substitute-many [target images names]
  (reduce (fn [t [im nme]] (substitute im nme t))
          target
          (map vector images names)))

(declare substitute-dots substitute-dotted)

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
              (crep/i-subst-dotted? r) (throw (Exception. "i-subst-dotted nyi"))
              :else (throw (Exception. "Other substitutions NYI"))))
          t s))

;; Substitute dots

(derive ::substitute-dots f/fold-rhs-default)

(f/add-fold-case ::substitute-dots
               Function
               (fn [{:keys [dom rng rest drest kws] :as ftype} {{:keys [name sb images rimage]} :locals}]
                 (assert (not kws) "TODO substitute keyword args")
                 (if (and drest
                          (= name (:name drest)))
                   (r/Function-maker (concat (map sb dom)
                                       ;; We need to recur first, just to expand out any dotted usages of this.
                                       (let [expanded (sb (:pre-type drest))]
                                         ;(prn "expanded" (unparse-type expanded))
                                         (map (fn [img] (substitute img name expanded)) images)))
                               (sb rng)
                               rimage nil nil)
                   (r/Function-maker (map sb dom)
                               (sb rng)
                               (and rest (sb rest))
                               (and drest (r/DottedPretype-maker (sb (:pre-type drest))
                                                           (:name drest)))
                               nil))))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
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
               (fn [{:keys [dom rng rest drest kws]} {{:keys [sb name image]} :locals}]
                 (assert (not kws))
                 (r/Function-maker (map sb dom)
                             (sb rng)
                             (and rest (sb rest))
                             (and drest
                                  (r/DottedPretype-maker (substitute image (:name drest) (sb (:pretype drest)))
                                                     (if (= name (:name drest))
                                                       name
                                                       (:name drest))))
                             nil)))

;; implements curly brace substitution from the formalism
;; substitute-dotted : Type Name Name Type -> Type
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
                target))))
