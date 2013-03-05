(in-ns 'clojure.core.typed)

(defmulti generalize* class)

(defn generalize [type]
  {:pre [(Type? type)]
   :post [(Type? %)]}
  (generalize* (if (requires-resolving? type)
                 (-resolve type)
                 type)))

(defmethod generalize* HeterogeneousMap
  [type]
  (RClass-of APersistentMap [Any Any]))

(defmethod generalize* Union
  [type]
  (apply Un (map generalize (:types type))))

(defmethod generalize* Intersection
  [type]
  (apply In (map generalize (:types type))))

(defmethod generalize* HeterogeneousVector
  [type]
  (RClass-of APersistentVector [(apply Un (:types type))]))

(defmethod generalize* Value
  [{:keys [val] :as type}]
  (cond
    (integer? val) (parse-type 'AnyInteger)
    (number? val) (RClass-of Number)
    (symbol? val) (RClass-of Symbol)
    (keyword? val) (RClass-of Keyword)
    :else type))

(defmethod generalize* :default
  [type]
  type)
