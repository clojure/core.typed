(set! *warn-on-reflection* true)

(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(def variance-map? (hash-c? symbol? variance?))

(declare ^:dynamic *frees-mode* frees-in)

(defn fv-variances 
  "Map of frees to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::frees]
    (frees-in t)))

(defn idx-variances 
  "Map of indexes to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::idxs]
    (frees-in t)))

(defn fv 
  "All frees in type"
  [t]
  {:post [((set-c? symbol?) %)]}
  (set (keys (fv-variances t))))

(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t]
  {:post [((set-c? symbol?) %)]}
  (set (keys (idx-variances t))))

(defn flip-variances [vs]
  {:pre [(variance-map? vs)]}
  (into {} (for [[k vari] vs]
             [k (case vari
                  :covariant :contravariant
                  :contravariant :covariant
                  vari)])))

(defn combine-frees [& frees]
  {:pre [(every? variance-map? frees)]
   :post [(variance-map? %)]}
  (into {}
        (apply merge-with (fn [old-vari new-vari]
                            (cond 
                              (= old-vari new-vari) old-vari
                              (= old-vari :dotted) new-vari
                              (= new-vari :dotted) old-vari
                              (= old-vari :constant) new-vari
                              (= new-vari :constant) old-vari
                              :else :invariant))
               frees)))

(derive ::frees ::any-var)
(derive ::idxs ::any-var)

(def ^:dynamic *frees-mode* nil)
(set-validator! #'*frees-mode* #(or (= ::frees %)
                                    (= ::idxs %)
                                    (nil? %)))

(declare frees)

(defn frees-in [t]
  {:post [(variance-map? %)]}
  (frees t))

(defmulti frees (fn [t] [*frees-mode* (class t)]))

(defmethod frees [::any-var Result]
  [{:keys [t fl o]}]
  (combine-frees (frees t)
                 (frees fl)
                 (frees o)))

;; Filters

(defmethod frees [::any-var FilterSet]
  [{:keys [then else]}]
  (combine-frees (frees then)
                 (frees else)))

(defmethod frees [::any-var TypeFilter]
  [{:keys [type]}]
  (frees type))

(defmethod frees [::any-var NotTypeFilter]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var ImpFilter]
  [{:keys [a c]}] 
  (combine-frees (frees a)
                 (frees c)))

(defmethod frees [::any-var AndFilter]
  [{:keys [fs]}] 
  (apply combine-frees (mapv frees fs)))

(defmethod frees [::any-var OrFilter]
  [{:keys [fs]}]
  (apply combine-frees (mapv frees fs)))

(defmethod frees [::any-var TopFilter] [t] {})
(defmethod frees [::any-var BotFilter] [t] {})

;; Objects

(defmethod frees [::any-var Path]
  [{:keys [path]}]
  (apply combine-frees (mapv frees path)))

(defmethod frees [::any-var EmptyObject] [t] {})
(defmethod frees [::any-var NoObject] [t] {})
(defmethod frees [::any-var KeyPE] [t] {})


(defmethod frees [::frees F]
  [{:keys [name] :as t}]
  {name :covariant})

(defmethod frees [::idxs F] [t] {})

(defmethod frees [::any-var B] [t] {})
(defmethod frees [::any-var CountRange] [t] {})
(defmethod frees [::any-var Value] [t] {})
(defmethod frees [::any-var AnyValue] [t] {})
(defmethod frees [::any-var Top] [t] {})
(defmethod frees [::any-var Name] [t] {})

(defmethod frees [::any-var DataType]
  [{:keys [fields poly?]}]
  (apply combine-frees 
         (mapv frees (concat (vals fields) poly?))))

(defmethod frees [::any-var HeterogeneousList]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var App]
  [{:keys [rator rands]}]
  (apply combine-frees (mapv frees (cons rator rands))))

(defmethod frees [::any-var TApp]
  [{:keys [rator rands]}]
  (apply combine-frees
         (let [^TypeFn
               tfn (loop [rator rator]
                     (cond
                       (F? rator) (when-let [bnds (free-with-name-bnds (.name ^F rator))]
                                    (.higher-kind bnds))
                       (Name? rator) (if (= declared-name-type (@TYPE-NAME-ENV (.id ^Name rator)))
                                       (recur (get-declared-kind (.id ^Name rator)))
                                       (recur (resolve-Name rator)))
                       (TypeFn? rator) rator
                       :else (throw (Exception. (error-msg "NYI case " (class rator) (unparse-type rator))))))
               _ (assert (TypeFn? tfn))]
           (mapv (fn [[v arg-vs]]
                   (case v
                     :covariant arg-vs
                     :contravariant (flip-variances arg-vs)
                     :invariant (into {} (for [[k _] arg-vs]
                                           [k :invariant]))))
                 (map vector (.variances tfn) (map frees rands))))))

(defmethod frees [::any-var PrimitiveArray]
  [{:keys [input-type output-type]}] 
  (combine-frees (flip-variances (frees input-type))
                 (frees output-type)))

(defmethod frees [::any-var HeterogeneousSeq]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var HeterogeneousMap]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees (concat (keys types) (vals types)))))

(defmethod frees [::any-var HeterogeneousVector]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var FnIntersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::frees Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               [(vals kws)])))
                               [(frees rng)]
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees flip-variances)
                                          (:name drest))]))))

(defmethod frees [::idxs Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               (vals kws))))
                               [(frees rng)]
                               (when drest
                                 (let [{:keys [name pre-type]} drest]
                                   [{name :contravariant}
                                    (-> pre-type
                                      frees flip-variances)])))))

(defmethod frees [::any-var RClass]
  [t]
  (let [varis (:variances t)
        args (:poly? t)]
    (assert (= (count args) (count varis)))
    (apply combine-frees (for [[arg va] (map vector args varis)]
                           (case va
                             :covariant (frees arg)
                             :contravariant (flip-variances (frees arg))
                             :invariant (let [fvs (frees arg)]
                                          (into {}
                                                (for [[k _] fvs]
                                                  [k :invariant]))))))))

(defmethod frees [::any-var Scope]
  [{:keys [body]}]
  (frees body))

;FIXME Type variable bounds should probably be checked for frees
(defmethod frees [::any-var TypeFn]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var Poly]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var PolyDots]
  [{:keys [nbound scope]}]
  (frees scope))

