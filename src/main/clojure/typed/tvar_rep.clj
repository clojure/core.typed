(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(defn add-scopes [n t]
  "Wrap type in n Scopes"
  {:pre [(nat? n)
         (Type? t)]}
  (doall
    (last 
      (take (inc n) (iterate ->Scope t)))))

(defn remove-scopes 
  "Unwrap n Scopes"
  [n sc]
  {:pre [(nat? n)
         (or (zero? n)
             (Scope? sc))]
   :post [(or (Scope? %) (Type? %))]}
  (doall
    (last
      (take (inc n) (iterate (fn [t]
                               (assert (Scope? t) "Tried to remove too many Scopes")
                               (:body t))
                             sc)))))

(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (iterate dec (dec (count c))) c))

(derive ::abstract-many fold-rhs-default)

(add-fold-case ::abstract-many
               F
               (fn [{name* :name :as t} {{:keys [name count outer sb]} :locals}]
                 (if (= name name*)
                   (->B (+ count outer))
                   t)))

(add-fold-case ::abstract-many
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [name count outer sb]} :locals}]
                 (assert (not kws))
                 (->Function (map sb dom)
                             (sb rng)
                             (when rest (sb rest))
                             (when drest
                               (->DottedPretype (sb (:pre-type drest))
                                                (if (= (:name drest) name)
                                                  (+ count outer)
                                                  (:name drest))))
                             nil)))

(add-fold-case ::abstract-many
               Mu
               (fn [{:keys [scope]} {{:keys [name count type outer name-to]} :locals}]
                 (let [body (remove-scopes 1 scope)]
                   (->Mu (->Scope (name-to name count type (inc outer) body))))))

(add-fold-case ::abstract-many
               PolyDots
               (fn [{bbnds* :bbnds n :nbound body* :scope} {{:keys [name count type outer name-to]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(visit-bounds % rs) bbnds*)
                       as #(add-scopes n (name-to name count type (+ n outer) %))]
                   (->PolyDots n 
                               (mapv #(visit-bounds % rs) bbnds)
                               (as body)))))

(add-fold-case ::abstract-many
               Poly
               (fn [{bbnds* :bbnds n :nbound body* :scope :as poly} {{:keys [name count type outer name-to]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(visit-bounds % rs) bbnds*)
                       as #(add-scopes n (name-to name count type (+ n outer) %))]
                   (->Poly n 
                           (mapv #(visit-bounds % as) bbnds)
                           (as body)
                           (Poly-free-names* poly)))))

(add-fold-case ::abstract-many
               TypeFn
               (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances]} {{:keys [name count type outer name-to]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(visit-bounds % rs) bbnds*)
                       as #(add-scopes n (name-to name count type (+ n outer) %))]
                   (->TypeFn n 
                             variances
                             (mapv #(visit-bounds % as) bbnds)
                             (as body)))))

(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         ((some-fn Type? TypeFn?) ty)]}
  (letfn [(name-to 
            ([name count type] (name-to name count type 0 type))
            ([name count type outer ty]
             (letfn [(sb [t] (name-to name count type outer t))]
               (fold-rhs ::abstract-many
                 {:type-rec sb
                  :filter-rec (sub-f sb ::abstract-many)
                  :object-rec (sub-o sb ::abstract-many)
                  :locals {:name name
                           :count count
                           :outer outer
                           :sb sb
                           :name-to name-to}}
                 ty))))]
    (if (empty? names)
      ty
      (let [n (count names)]
        (loop [ty ty
               names names
               count (dec n)]
          (if (zero? count)
            (add-scopes n (name-to (first names) 0 ty))
            (recur (name-to (first names) count ty)
                   (next names)
                   (dec count))))))))

(derive ::instantiate-many fold-rhs-default)

(add-fold-case ::instantiate-many
               B
               (fn [{:keys [idx] :as t} {{:keys [count outer image sb]} :locals}]
                 (if (= (+ count outer) idx)
                   (->F image)
                   t)))

(add-fold-case ::instantiate-many
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [count outer image sb]} :locals}]
                 (assert (not kws))
                 (->Function (map sb dom)
                             (sb rng)
                             (when rest
                               (sb rest))
                             (when drest
                               (->DottedPretype (sb (:pre-type drest))
                                                (let [{:keys [name]} drest]
                                                  (assert (nat? name))
                                                  (if (= (+ count outer) name)
                                                    image
                                                    name))))
                             nil)))

(add-fold-case ::instantiate-many
               Mu
               (fn [{:keys [scope]} {{:keys [replace count outer image sb type]} :locals}]
                 (let [body (remove-scopes 1 scope)]
                   (->Mu (->Scope (replace image count type (inc outer) body))))))

(add-fold-case ::instantiate-many
               PolyDots
               (fn [{bbnds* :bbnds n :nbound body* :scope} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (->PolyDots n 
                               (mapv #(visit-bounds % as) bbnds)
                               (as body)))))

(add-fold-case ::instantiate-many
               Poly
               (fn [{bbnds* :bbnds n :nbound body* :scope :as poly} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (->Poly n 
                           (mapv #(visit-bounds % as) bbnds)
                           (as body)
                           (Poly-free-names* poly)))))

(add-fold-case ::instantiate-many
               TypeFn
               (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances]} {{:keys [replace count outer image sb type]} :locals}]
                 (let [rs #(remove-scopes n %)
                       body (rs body*)
                       bbnds (mapv #(visit-bounds % rs) bbnds*)
                       as #(add-scopes n (replace image count type (+ n outer) %))]
                   (->TypeFn n 
                             variances
                             (mapv #(visit-bounds % as) bbnds)
                             (as body)))))

(defn instantiate-many 
  "instantiate-many : List[Symbols] Scope^n -> Type
  Instantiate de Bruijn indices in sc to frees named by
  images, preserving upper/lower bounds"
  [images sc]
  {:pre [(every? symbol? images)
         (or (Scope? sc)
             (empty? images))]
   :post [((some-fn Type? TypeFn?) %)]}
  (letfn [(replace 
            ([image count type] (replace image count type 0 type))
            ([image count type outer ty]
             (letfn [(sb [t] (replace image count type outer t))]
               (let [sf (sub-f sb ::instantiate-many)]
                 (fold-rhs ::instantiate-many
                   {:type-rec sb 
                    :filter-rec sf 
                    :object-rec (sub-o sb ::instantiate-many)
                    :locals {:count count
                             :outer outer
                             :image image
                             :sb sb
                             :type type
                             :replace replace}}
                   ty)))))]
    (if (empty? images)
      sc
      (let [n (count images)]
        (loop [ty (remove-scopes n sc)
               images images
               count (dec n)]
          (if (zero? count)
            (replace (first images) 0 ty)
            (recur (replace (first images) count ty)
                   (next images)
                   (dec count))))))))

(defn abstract [name ty]
  "Make free name bound"
  {:pre [(symbol? name)
         (Type? ty)]}
  (abstract-many [name] ty))

(defn instantiate [f sc]
  "Instantiate bound name to free"
  {:pre [(symbol? f)
         (Scope? sc)]}
  (instantiate-many [f] sc))

