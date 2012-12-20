(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Folding

(def fold-rhs-default ::fold-rhs)

;1. fold-rhs calls sends
; a. Type to type-rec
; b. Filter to filter-rec
; c. Object to object-rec

(declare unparse-type)

;visit a type nested inside ty. Add methods with a mode deriving ::visit-type-default 
(defmulti fold-rhs (fn [mode options ty]
                     [mode (class ty)]))

; fld-fn has type-rec, filter-rec and object-rec in scope
(defmacro add-fold-case [mode ty fld-fn]
  `(defmethod fold-rhs [~mode ~ty]
     [mode# options# ty#]
     (let [~'[type-rec filter-rec object-rec pathelem-rec]
           (map #(or (% options#)
                     (partial fold-rhs mode# options#))
                [:type-rec :filter-rec :object-rec :pathelem-rec])]
       (~fld-fn ty# options#))))

(defmacro add-default-fold-case [ty fld-fn]
  `(add-fold-case fold-rhs-default ~ty ~fld-fn))

(declare sub-pe)

(defn sub-f [st mode]
  #(fold-rhs mode
             {:type-rec st
              :filter-rec (sub-f st mode)
              :pathelem-rec (sub-pe st mode)}
             %))

(defn sub-o [st mode]
  #(fold-rhs mode
             {:type-rec st
              :object-rec (sub-o st mode)
              :pathelem-rec (sub-pe st mode)}
             %))

(defn sub-pe [st mode]
  #(fold-rhs fold-rhs-default
             {:type-rec st
              :pathelem-rec (sub-pe st mode)}
             %))

(add-default-fold-case NotType
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec))))

(add-default-fold-case Intersection
                       (fn [ty _]
                         (apply In (mapv type-rec (:types ty)))))

(add-default-fold-case Union 
                       (fn [ty _]
                         (apply Un (mapv type-rec (:types ty)))))

(add-default-fold-case FnIntersection
                       (fn [ty _]
                         (-> ty
                           (update-in [:types] #(mapv type-rec %)))))

(defn visit-bounds 
  "Apply f to each element of bounds"
  [ty f]
  {:pre [(Bounds? ty)]
   :post [(Bounds? ty)]}
  (-> ty
    (update-in [:upper-bound] #(when %
                                 (f %)))
    (update-in [:lower-bound] #(when %
                                 (f %)))
    (update-in [:higher-kind] #(when %
                                 (f %)))))

(add-default-fold-case Bounds
                       (fn [ty _]
                         (visit-bounds ty type-rec)))

(add-default-fold-case Projection
                       (fn [ty _]
                         (-> ty
                           (update-in [:ts] #(mapv type-rec %)))))

(add-default-fold-case DottedPretype
                       (fn [ty _]
                         (-> ty
                           (update-in [:pre-type] type-rec))))

(add-default-fold-case Function
                       (fn [ty _]
                         (-> ty
                           (update-in [:dom] #(mapv type-rec %))
                           (update-in [:rng] type-rec)
                           (update-in [:rest] #(when %
                                                 (type-rec %)))
                           (update-in [:drest] #(when %
                                                  (-> %
                                                    (update-in [:pre-type] type-rec)))))))

(add-default-fold-case RClass 
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:replacements] #(into {} (for [[k v] %]
                                                                  [k (type-rec v)]))))))

(add-default-fold-case App
                       (fn [ty _]
                         (-> ty
                           (update-in [:rator] type-rec)
                           (update-in [:rands] #(mapv type-rec %)))))

(add-default-fold-case TApp
                       (fn [ty _]
                         (-> ty
                           (update-in [:rator] type-rec)
                           (update-in [:rands] #(mapv type-rec %)))))

(add-default-fold-case PrimitiveArray
                       (fn [ty _]
                         (-> ty
                           (update-in [:input-type] type-rec)
                           (update-in [:output-type] type-rec))))

(add-default-fold-case DataType
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:fields] (fn [fs]
                                                  (apply array-map
                                                         (apply concat
                                                                (for [[k v] fs]
                                                                  [k (type-rec v)]))))))))

(add-default-fold-case Protocol
                       (fn [ty _]
                         (-> ty
                           (update-in [:poly?] #(when %
                                                  (mapv type-rec %)))
                           (update-in [:methods] (fn [ms]
                                                   (into {}
                                                         (for [[k v] ms]
                                                           [k (type-rec v)])))))))

(add-default-fold-case TypeFn
                       (fn [ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (TypeFn-body* names ty)
                               bbnds (TypeFn-bbnds* names ty)]
                           (TypeFn* names 
                                    (.variances ty)
                                    (mapv #(visit-bounds % type-rec) bbnds)
                                    (type-rec body)))))


(add-default-fold-case Poly
                       (fn [ty _]
                         (let [names (repeatedly (.nbound ty) gensym)
                               body (Poly-body* names ty)
                               bbnds (Poly-bbnds* names ty)]
                           (Poly* names 
                                  (mapv #(visit-bounds % type-rec) bbnds)
                                  (type-rec body)
                                  (Poly-free-names* ty)))))

(add-default-fold-case PolyDots
                       (fn [ty _]
                         (let [names (repeatedly (:nbound ty) gensym)
                               body (PolyDots-body* names ty)
                               bbnds (PolyDots-bbnds* names ty)]
                           (PolyDots* names 
                                      (mapv #(visit-bounds % type-rec) bbnds)
                                      (type-rec body)))))

(add-default-fold-case Mu
                       (fn [ty _]
                         (let [name (gensym)
                               body (Mu-body* name ty)]
                           (Mu* name (type-rec body)))))

(add-default-fold-case HeterogeneousVector
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousList 
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousSeq
                       (fn [ty _]
                         (-> ty (update-in [:types] #(mapv type-rec %)))))

(add-default-fold-case HeterogeneousMap
                       (fn [ty _]
                         (-> ty 
                           (update-in [:types] #(into {} (for [[k v] %]
                                                           [(type-rec k) (type-rec v)]))))))

(def ret-first (fn [a & rest] a))

(add-default-fold-case CountRange ret-first)
(add-default-fold-case Name ret-first)
(add-default-fold-case Value ret-first)
(add-default-fold-case Top ret-first)
(add-default-fold-case TopFunction ret-first)
(add-default-fold-case B ret-first)
(add-default-fold-case F ret-first)

(add-default-fold-case Result 
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec))))


;filters

(add-default-fold-case NoFilter ret-first)
(add-default-fold-case TopFilter ret-first)
(add-default-fold-case BotFilter ret-first)

(add-default-fold-case TypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (map pathelem-rec %))))))

(add-default-fold-case NotTypeFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:type] type-rec)
                           (update-in [:path] #(seq (map pathelem-rec %))))))

(add-default-fold-case ImpFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:a] filter-rec)
                           (update-in [:c] filter-rec))))

(add-default-fold-case AndFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:fs] #(set (map filter-rec %))))))

(add-default-fold-case OrFilter
                       (fn [ty _]
                         (-> ty
                           (update-in [:fs] #(set (map filter-rec %))))))

(add-default-fold-case FilterSet
                       (fn [ty _]
                         (-> ty
                           (update-in [:then] filter-rec)
                           (update-in [:else] filter-rec))))

;objects
(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty _]
                         (-> ty
                           (update-in [:path] #(when %
                                                 (mapv pathelem-rec %))))))
(add-default-fold-case NoObject ret-first)

;path-elems

(add-default-fold-case KeyPE ret-first)

;TCResult

(add-default-fold-case TCResult
                       (fn [ty _]
                         (-> ty
                           (update-in [:t] type-rec)
                           (update-in [:fl] filter-rec)
                           (update-in [:o] object-rec))))
