
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(declare subtype)

(derive ::substitute fold-rhs-default)

(add-fold-case ::substitute
               F
               (fn [{name* :name :as f} {{:keys [name image]} :locals}]
                 (if (= name* name)
                   image
                   f)))

(defn substitute [image name target]
  {:pre [(AnyType? image)
         (symbol? name)
         (AnyType? target)]
   :post [(AnyType? %)]}
  (fold-rhs ::substitute
            {:locals {:name name
                      :image image}}
            target))

(defn substitute-many [target images names]
  (reduce (fn [t [im nme]] (substitute im nme t))
          target
          (map vector images names)))

(defn subst-all [s t]
  {:pre [(substitution-c? s)
         (AnyType? t)]
   :post [(AnyType? %)]}
  (reduce (fn [t [v r]]
            (cond
              (t-subst? r) (substitute (:type r) v t)
              (i-subst? r) (substitute-dots (:types r) nil v t)
              (i-subst-starred? r) (substitute-dots (:types r) (:starred r) v t)
              (and (i-subst-dotted? r)
                   (empty? (:types r))) (substitute-dotted (:dty r) (:name (:dbound r)) v t)
              (i-subst-dotted? r) (throw (Exception. "i-subst-dotted nyi"))
              :else (throw (Exception. "Other substitutions NYI"))))
          t s))
