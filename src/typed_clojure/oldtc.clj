
;(defmethod check :var
;  [{:keys [var tag] :as expr}]
;  (let [expected-type (::+T expr)
;        actual-type (type-of var)]
;    (assert-subtype actual-type expected-type (str " for var " var))
;    (assoc expr
;           ::+T actual-type)))
;
;(defmethod synthesize :var
;  [{:keys [var tag] :as expr}]
;  (let [actual-type (type-of var)]
;    (assoc expr
;           ::+T actual-type)))
;
;;; def
;
;(defn infer-def [{:keys [var init init-provided] :as expr}]
;  (let [var-type (type-of var)
;        checked-init-expr (if init-provided
;                            (-> init
;                              (assoc ::+T var-type)
;                              check)
;                            (do
;                              (debug "No init provided for" var ", ignore body")
;                              init))]
;    (assoc expr
;           :init checked-init-expr
;           ::+T (map->TClass
;                  {:the-class Var}))))
;
;(defmethod check :def
;  [expr]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type "def in checking mode requires full type annotation")
;        inferred-def (infer-def expr)
;        actual-type (::+T inferred-def)
;        _ (assert-subtype actual-type expected-type)]
;    inferred-def))
;
;(defmethod synthesize :def
;  [expr]
;  (infer-def expr))
;
;(defn- infer-invoke [{:keys [fexpr args] :as expr}]
;  (let [synthesized-fexpr (synthesize fexpr)
;        fexpr-type (::+T synthesized-fexpr)
;        arity-type (some #(matches-args % args) (:arities fexpr-type))
;
;        _ (assert arity-type)
;
;        expected-dom (take (count args)
;                           (concat (:dom arity-type)
;                                   (when (:rest-type arity-type)
;                                     (repeat (:rest-type arity-type)))))
;
;        checked-args (doall (map #(-> %1
;                                    (assoc ::+T %2)
;                                    check)
;                                 args
;                                 expected-dom))
;
;        ;; instatiate type arguments
;        constraint-set (let [cs (doall (map #(constraint-gen %1 %2 (set (:type-params arity-type)) #{})
;                                            (doall (map ::+T checked-args))
;                                            expected-dom))]
;                         (apply intersect-constraint-sets cs))
;        
;        min-sub (minimal-substitution arity-type constraint-set)
;        _ (println "before" (unp arity-type))
;        arity-type (let [rplc #(replace-variables % min-sub)]
;                     (-> arity-type
;                       (update-in [:dom] #(doall (map rplc %)))
;                       (update-in [:rest-type] #(when % (rplc %)))
;                       (update-in [:rng] rplc)))
;
;        _ (println "after" (unp arity-type))
;
;        instatiated-fexpr (assoc synthesized-fexpr
;                                 ::+T (map->Fun {:arities [arity-type]}))
;        return-type (:rng arity-type)]
;    (assoc expr
;           :fexpr instatiated-fexpr
;           :args checked-args
;           ::+T return-type)))
;
;;; invoke
;
;(defmethod synthesize :invoke
;  [expr]
;  (infer-invoke expr))
;
;(defmethod check :invoke
;  [expr]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type "Checking context for function invocation requires full
;                                type annotations")
;        inferred-expr (infer-invoke expr)
;        actual-type (::+T inferred-expr)
;        _ (assert-subtype actual-type expected-type)]
;    inferred-expr))
;
;;; if
;
;(defmethod synthesize :if
;  [{:keys [test then else] :as expr}]
;  (let [[synthesized-test
;         synthesized-then
;         synthesized-else
;         :as typed-exprs]
;        (map synthesize [test then else])
;        
;        actual-type (union (map ::+T typed-exprs))]
;    (assoc expr
;           :test synthesized-test
;           :then synthesized-then
;           :else synthesized-else
;           ::+T actual-type)))
;
;(defmethod check :if
;  [{:keys [test then else] :as expr}]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type "if in checking mode requires full annotation")
;
;        synthesized-test (synthesize test)
;
;        check-else? (boolean (falsy-values (::+T synthesized-test)))
;
;        checked-then (check (assoc then 
;                                   ::+T expected-type))
;
;        inferred-else (if check-else?
;                        (check (assoc else
;                                      ::+T expected-type))
;                        (do 
;                          (warn "Unreachable else clause")
;                          (synthesize else)))
;        
;        actual-type (union (map ::+T (concat [checked-then] (when check-else?
;                                                              inferred-else))))
;        _ (assert-subtype actual-type expected-type)]
;    (assoc expr 
;           :test synthesized-test
;           :then checked-then
;           :else inferred-else
;           ::+T actual-type)))
;
;;; local bindings
;
;(defmethod synthesize :local-binding-expr
;  [{:keys [local-binding] :as expr}]
;  (let [synthesized-lb (synthesize local-binding)
;        actual-type (::+T synthesized-lb)]
;    (assoc expr
;           :local-binding synthesized-lb
;           ::+T actual-type)))
;
;(defmethod check :local-binding-expr
;  [{:keys [local-binding] :as expr}]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type (str "Local binding " (:sym local-binding)
;                                     " requires type annotation in checking context."))
;        checked-lb (check (assoc local-binding
;                                 ::+T expected-type))
;        actual-type (::+T checked-lb)
;        _ (assert-subtype actual-type expected-type)]
;    (assoc expr
;           :local-binding checked-lb
;           ::+T actual-type)))
;
;(defmethod check :local-binding
;  [{:keys [sym init] :as expr}]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type sym)
;        actual-type (type-of sym)
;        _ (assert-subtype actual-type expected-type)]
;    (assoc expr
;           ::+T actual-type)))
;
;;; literals
;
;(defmulti constant-type class)
;
;(defmethod constant-type Boolean
;  [b]
;  (if (true? b)
;    True
;    False))
;
;(defmethod constant-type nil
;  [k]
;  Nil)
;
;(defmethod constant-type Keyword
;  [k]
;  (->KeywordType k))
;
;(defmethod constant-type Symbol
;  [s]
;  (->SymbolType s))
;
;(defmethod constant-type String
;  [s]
;  (->StringType s))
;
;(defmethod constant-type Long
;  [l]
;  (->LongType l))
;
;(defmethod constant-type Double
;  [d]
;  (->DoubleType d))
;
;(defmethod constant-type IPersistentVector
;  [v]
;  (->ConstantVector (map constant-type v)))
;
;(defmacro literal-dispatches [disp-keyword]
;  `(do
;     (defmethod synthesize ~disp-keyword
;       [expr#]
;       (let [val# (:val expr#)
;             actual-type# (constant-type val#)]
;         (assoc expr#
;                ::+T actual-type#)))
;
;     (defmethod check ~disp-keyword
;       [expr#]
;       (let [val# (:val expr#)
;             expected-type# (::+T expr#)
;             actual-type# (constant-type val#)]
;         (assert-subtype actual-type# expected-type#)
;         (assoc expr#
;                ::+T actual-type#)))))
;
;(doseq [k #{:keyword :string :symbol :constant :number :boolean :nil}]
;  (literal-dispatches k))
;
;;; empty-expr
;
;(defmethod check :empty-expr
;  [{:keys [coll] :as expr}]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type "empty-expr: must provide expected type in checking mode")
;        actual-type (map->TClass {:the-class (class coll)})
;        _ (assert-subtype actual-type expected-type)]
;    (assoc expr
;           ::+T actual-type)))
;
;(defmethod synthesize :empty-expr
;  [{:keys [coll] :as expr}]
;  (let [actual-type (map->TClass {:the-class (class coll)})]
;    (assoc expr
;           ::+T actual-type)))
;
;;; let
;
;(defmethod synthesize :binding-init
;  [{:keys [sym init] :as expr}]
;  (let [synthesized-init (synthesize init)]
;    (assoc expr
;           :init synthesized-init
;           ::+T (::+T synthesized-init))))
;
;(defmethod check :let
;  [{:keys [binding-inits body is-loop] :as expr}]
;  (assert (not is-loop) "Loop not implemented")
;  (let [expected-type (::+T expr)
;        _ (assert expected-type)
;        
;        [typed-binding-inits local-types]
;        (loop [binding-inits binding-inits
;               typed-binding-inits []
;               local-types {}]
;          (if (empty? binding-inits)
;            [typed-binding-inits local-types]
;            (let [[bnd-init] binding-inits
;                  typed-bnd-init (with-local-types local-types
;                                   (synthesize bnd-init))
;                  local-type-entry [(-> typed-bnd-init :local-binding :sym)
;                                    (-> typed-bnd-init ::+T)]
;                  _ (assert (every? identity local-type-entry))]
;              (recur (rest binding-inits)
;                     (conj typed-binding-inits typed-bnd-init)
;                     (conj local-types local-type-entry)))))
;        
;        checked-body (with-local-types local-types
;                       (check (assoc body
;                                     ::+T expected-type)))]
;    (assoc expr
;           :binding-inits typed-binding-inits
;           :body checked-body
;           ::+T (-> checked-body ::+T))))
;
;;; fn
;
;(defmethod check :fn-expr
;  [{:keys [methods] :as expr}]
;  (let [expected-type (::+T expr)
;        _ (assert (instance? Fun expected-type) (str "Expected Fun type, instead found " (unparse-type expected-type)))
;
;        checked-methods (doall 
;                          (for [method methods]
;                            (let [_ (assert (not (:rest-param method)))
;                                  arity (some #(matches-args % (:required-params method)) (:arities expected-type))
;                                  _ (assert arity 
;                                            (str "No arity with " (count (:required-params method)) 
;                                                 " parameters in type "
;                                                 expected-type))]
;                              (check (assoc method
;                                            ::+T arity)))))
;
;        actual-type (map->Fun
;                      {:arities (doall (map ::+T checked-methods))})
;
;        _ (assert-subtype actual-type expected-type)]
;    (assoc expr
;           ::+T actual-type
;           :methods checked-methods)))
;
;(defmethod check :fn-method
;  [{:keys [required-params rest-param body] :as expr}]
;  (assert (not rest-param))
;  (let [expected-arity-type (::+T expr)
;        _ (assert (not (:rest-type expected-arity-type)))
;        
;        typed-required-params (doall 
;                                (map #(assoc %1 ::+T %2)
;                                     required-params
;                                     (:dom expected-arity-type)))
;
;        typed-lbndings (apply hash-map 
;                              (doall (mapcat #(vector (:sym %) 
;                                                      (::+T %))
;                                             typed-required-params)))
;
;        checked-body (with-local-types typed-lbndings
;                       (check (assoc body
;                                     ::+T (:rng expected-arity-type))))]
;    (assoc expr
;           :required-params typed-required-params
;           :body checked-body)))
;
;;; do
;
;(defmethod synthesize :do
;  [{:keys [exprs] :as expr}]
;  (let [synthesized-exprs (vec (doall (map synthesize exprs)))
;        actual-type (-> synthesized-exprs last ::+T)
;        _ (assert actual-type)]
;    (assoc expr
;           :exprs synthesized-exprs
;           ::+T actual-type)))
;
;(defmethod check :do
;  [{:keys [exprs] :as expr}]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type "do requires type annotation in checking mode")
;
;        butlast-synthesized-exprs (vec (doall (map synthesize (butlast exprs))))
;        _ (assert (seq exprs))
;        last-checked-expr (check (assoc (last exprs)
;                                        ::+T expected-type))
;        typed-exprs (conj butlast-synthesized-exprs last-checked-expr)
;
;        actual-type (::+T last-checked-expr)
;        _ (assert actual-type last-checked-expr)]
;    (assoc expr
;           :exprs typed-exprs
;           ::+T actual-type)))
;
;;; static method
;
;(defn- overriden-annotation [{name-sym :name, class-sym :declaring-class,
;                              :keys [declaring-class parameter-types] :as method}]
;  (let [_ (assert (and class-sym name-sym)
;                  (str "Unresolvable static method " class-sym name-sym))
;        method-sym (symbol (name class-sym) (name name-sym))]
;    (try
;      (type-of method-sym)
;      (catch Exception e))))
;
;(defn infer-static-method [{:keys [method args] :as expr}]
;  (let [override (overriden-annotation method)
;        _ (if override
;            (println "Overriding static method " (symbol (name (:declaring-class method))
;                                                         (name (:name method))))
;            (println "Not overriding static method " (symbol (name (:declaring-class method))
;                                                             (name (:name method)))))
;        fun-type (if override
;                   override
;                   (method->Fun method))
;        arity-type (some #(matches-args % args) (:arities fun-type))
;
;        checked-args (doall 
;                       (map #(-> %1
;                               (assoc ::+T %2)
;                               check)
;                            args
;                            (:dom arity-type)))
;
;        actual-type (:rng arity-type)]
;    (assoc expr
;           :args checked-args
;           ::+T actual-type)))
;
;(defmethod synthesize :static-method
;  [expr]
;  (infer-static-method expr))
;
;(defmethod check :static-method
;  [expr]
;  (let [expected-type (::+T expr)
;        _ (assert expected-type "Static method in checking mode requires annotation")
;
;        inferred-expr (infer-static-method expr)
;
;        actual-type (::+T inferred-expr)
;        _ (assert-subtype actual-type expected-type)]
;    expr))
