(ns typed-clojure.infer)

(defmacro map-all-true? [& body]
  `(every? true? (map ~@body)))

(def type-db (atom {}))

(def ^:dynamic *local-type-db* {})

(defn reset-type-db []
  (swap! type-db (constantly {})))

(defn type-of [sym-or-var]
  (let [qual-sym (if (var? sym-or-var)
                   sym-or-var
                   (symbol (str (.name (.ns sym-or-var))) (str (.sym sym-or-var))))
        _ (assert (namespace qual-sym))]
    (if-let [the-local-type (*local-type-db* qual-sym)]
      the-local-type
      (if-let [the-type (@type-db qual-sym)]
        the-type
        (throw (Exception. (str "No type for " qual-sym)))))))

(defmacro with-local-types [type-map & body]
  `(binding [*local-type-db* (merge *local-type-db* ~type-map)]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed Clojure Kinds

(def tc-any ::tc-any)

(def bot ::bot)
(def top ::top)

(def fun ::fun)
(def fixed-arity ::fixed-arity) ; not a type

(def tc-types #{bot top fun})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inheritance relationships

(doseq [t tc-types]
  (derive t tc-any))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(defmulti subtype? (fn [s t] (vec (map :tc-type [s t]))))

(prefer-method subtype? 
  [bot tc-any]
  [tc-any top])

(defmethod subtype? [bot tc-any]
  [s t]
  true)

(defmethod subtype? [tc-any top]
  [s t]
  true)

(defmethod subtype? [fun fun]
  [s t]
  (map-all-true? subtype? (:arities s) (:arities t)))

(defmethod subtype? [fixed-arity fixed-arity]
  [s t]
  (and (map-all-true? subtype? (:dom t) (:dom s))
       (subtype? (:rng s) (:rng t))))

(defmethod subtype? [tc-any top]
  [s t]
  (= s t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Bidirectional checking (Local Type Inference (2000) Pierce & Turner, Chapter 4)

(defmulti infer-type :op)

(defmulti synthesize :op)
(defmulti check :op)

(defmethod infer-type :var
  [{:keys [var tag] :as expr}]
  (if (::+T expr)
    (check expr)
    (synthesize expr)))

(defmethod synthesize :var
  [{:keys [var tag] :as expr}]
  (assoc expr ::+T (type-of var)))

(defmethod check :var
  [{:keys [var tag] :as expr}]
  (assert (subtype? (type-of var) (::+T expr)))
  expr)

(defmethod infer-type :fn-expr
  [{:keys [methods] :as expr}]
  (let [ann-methods (->> methods
                      (map #(assoc % ::+T (::+T expr)))
                      (map infer-type))]
    (merge expr 
           {::+T {:tc-type fun
                  :arities (map ::+T ann-methods)}
            :methods ann-methods})))

(defmethod infer-type :fn-method
  [{:keys [required-params rest-param body] :as expr}]
  (assert (not rest-param))
  (if (::+T expr)
    (synthesize expr)
    (check expr)))

(defmethod synthesize :fn-method
  [{:keys [required-params rest-param body] :as expr}]
  (assert (not rest-param))
  (assert (::+T expr))
  ;; TODO keep track of locals, preferably in analyze
)

