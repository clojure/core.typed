(in-ns 'typed.core)



(declare sub-f sub-o sub-pe)

(derive ::substitute-dots fold-rhs-default)

(add-fold-case ::substitute-dots
               Function
               (fn [{:keys [dom rng rest drest kws] :as ftype} {{:keys [name sb images rimage]} :locals}]
                 (assert (not kws) "TODO substitute keyword args")
                 (if (and drest
                          (= name (:name drest)))
                   (->Function (concat (map sb dom)
                                       ;; We need to recur first, just to expand out any dotted usages of this.
                                       (let [expanded (sb (:pre-type drest))]
                                         ;(prn "expanded" (unparse-type expanded))
                                         (map (fn [img] (substitute img name expanded)) images)))
                               (sb rng)
                               rimage nil nil)
                   (->Function (map sb dom)
                               (sb rng)
                               (and rest (sb rest))
                               (and drest (->DottedPretype (sb (:pre-type drest))
                                                           (:name drest)))
                               nil))))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(defn substitute-dots [images rimage name target]
  {:pre [(every? AnyType? images)
         ((some-fn nil? AnyType?) rimage)
         (symbol? name)
         (AnyType? target)]}
  ;(prn "substitute-dots" (unparse-type target) name "->" (map unparse-type images))
  (letfn [(sb [t] (substitute-dots images rimage name t))]
    (if (or ((fi target) name)
            ((fv target) name))
      (fold-rhs ::substitute-dots 
                {:type-rec sb
                 :filter-rec (sub-f sb ::substitute-dots)
                 :locals {:name name
                          :sb sb
                          :images images
                          :rimage rimage}}
                target)
      target)))

(derive ::substitute-dotted fold-rhs-default)

(add-fold-case ::substitute-dotted
               F
               (fn [{name* :name :as t} {{:keys [name image]} :locals}]
                 (if (= name* name)
                   image
                   t)))

(add-fold-case ::substitute-dotted
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [sb name image]} :locals}]
                 (assert (not kws))
                 (->Function (map sb dom)
                             (sb rng)
                             (and rest (sb rest))
                             (and drest
                                  (->DottedPretype (substitute image (:name drest) (sb (:pretype drest)))
                                                   (if (= name (:name drest))
                                                     name
                                                     (:name drest))))
                             nil)))

;; implements curly brace substitution from the formalism
;; substitute-dotted : Type Name Name Type -> Type
(defn substitute-dotted [image image-bound name target]
  {:pre [(AnyType? image)
         (symbol? image-bound)
         (symbol? name)
         (AnyType? target)]
   :post [(AnyType? %)]}
  (letfn [(sb [t] (substitute-dotted image image-bound name t))]
    (if ((fi target) name)
      (fold-rhs ::substitute-dotted
                {:type-rec sb 
                 :filter-rec (sub-f sb ::substitute-dotted)
                 :locals {:name name
                          :sb sb
                          :image image}}
                target
                target))))
