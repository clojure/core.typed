(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.fold)

(def fold-expr-default ::fold-expr)

(defn derive-default-fold [tag]
  (derive tag fold-expr-default))

(defmulti fold-expr (fn [mode options expr]
                      [mode (:op expr)]))

(defmacro add-fold-case [mode op fld-fn]
  `(defmethod fold-expr [~mode ~op]
     [mode# options# expr#]
     (let [~'[expr-rec]
           (map #(or (% options#)
                     (partial fold-expr mode# options#))
                [:expr-rec])
           ~'map-expr-rec #(if % (map ~'expr-rec %) %)
           ~'if-expr-rec #(if % (~'expr-rec %) %)
           fld-fn# ~fld-fn]
       (fld-fn# expr# options#))))

(defmacro add-default-fold-case [ty fld-fn]
  `(add-fold-case fold-expr-default ~ty ~fld-fn))

(defn return-first [a & _] a)

(add-default-fold-case :keyword return-first)
(add-default-fold-case :constant return-first)
(add-default-fold-case :number return-first)
(add-default-fold-case :string return-first)
(add-default-fold-case :nil return-first)
(add-default-fold-case :boolean return-first)

(add-default-fold-case :def 
  (fn [expr _]
    (-> expr
      (update-in [:init] if-expr-rec))))

(add-default-fold-case :local-binding
  (fn [expr _]
    (-> expr
      (update-in [:init] if-expr-rec))))

(add-default-fold-case :binding-init
  (fn [expr _]
    (-> expr
      (update-in [:init] expr-rec))))

(add-default-fold-case :let
  (fn [expr _]
    (-> expr
      (update-in [:binding-inits] map-expr-rec)
      (update-in [:body] expr-rec))))

(add-default-fold-case :letfn
  (fn [expr _]
    (-> expr
      (update-in [:binding-inits] map-expr-rec)
      (update-in [:body] expr-rec))))

(add-default-fold-case :local-binding-expr
  (fn [expr _]
    (-> expr
      (update-in [:local-binding] expr-rec))))

(add-default-fold-case :static-method
  (fn [expr _]
    (-> expr
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :instance-method
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec)
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :static-field return-first)

(add-default-fold-case :instance-field 
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec))))

(add-default-fold-case :new
  (fn [expr _]
    (-> expr
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :empty-expr return-first)

(add-default-fold-case :set
  (fn [expr _]
    (-> expr
      (update-in [:keys] map-expr-rec))))

(add-default-fold-case :vector
  (fn [expr _]
    (-> expr
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :map
  (fn [expr _]
    (-> expr
      (update-in [:keyvals] map-expr-rec))))

(add-default-fold-case :monitor-enter
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec))))

(add-default-fold-case :monitor-exit
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec))))

(add-default-fold-case :throw
  (fn [expr _]
    (-> expr
      (update-in [:exception] expr-rec))))

(add-default-fold-case :invoke
  (fn [expr _]
    (-> expr
      (update-in [:fexpr] expr-rec)
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :keyword-invoke
  (fn [expr _]
    (-> expr
      (update-in [:kw] expr-rec)
      (update-in [:target] expr-rec))))

(add-default-fold-case :the-var return-first)
(add-default-fold-case :var return-first)
(add-default-fold-case :unresolved-var return-first)
(add-default-fold-case :obj-expr return-first)

(add-default-fold-case :new-instance-method 
  (fn [expr _]
    (-> expr
      (update-in [:body] expr-rec)
      (update-in [:required-params] map-expr-rec))))

(add-default-fold-case :fn-method
  (fn [expr _]
    (-> expr
      (update-in [:body] expr-rec)
      (update-in [:required-params] map-expr-rec)
      (update-in [:rest-param] if-expr-rec))))

(add-default-fold-case :fn-expr
  (fn [expr _]
    (-> expr
      (update-in [:methods] map-expr-rec)
      (update-in [:variadic-method] if-expr-rec))))

(add-default-fold-case :deftype*
  (fn [expr _]
    (-> expr
      (update-in [:methods] map-expr-rec))))

(add-default-fold-case :instance-of
  (fn [expr _]
    (-> expr
      (update-in [:the-expr] expr-rec))))

(add-default-fold-case :meta
  (fn [expr _]
    (-> expr
      (update-in [:meta] expr-rec)
      (update-in [:expr] expr-rec))))

(add-default-fold-case :do
  (fn [expr _]
    (-> expr
      (update-in [:exprs] map-expr-rec))))

(add-default-fold-case :if
  (fn [expr _]
    (-> expr
      (update-in [:test] expr-rec)
      (update-in [:then] expr-rec)
      (update-in [:else] expr-rec))))

(add-default-fold-case :case*
  (fn [expr _]
    (-> expr
      (update-in [:the-expr] expr-rec)
      (update-in [:tests] map-expr-rec)
      (update-in [:thens] map-expr-rec)
      (update-in [:default] expr-rec))))

(add-default-fold-case :import* return-first)

(add-default-fold-case :set!
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec)
      (update-in [:val] expr-rec))))

(add-default-fold-case :catch
  (fn [expr _]
    (-> expr
      (update-in [:local-binding] expr-rec)
      (update-in [:handler] expr-rec))))

(add-default-fold-case :try
  (fn [expr _]
    (-> expr
      (update-in [:try-expr] expr-rec)
      (update-in [:finally-expr] if-expr-rec)
      (update-in [:catch-exprs] map-expr-rec))))

(add-default-fold-case :recur
  (fn [expr _]
    (-> expr
      (update-in [:loop-locals] map-expr-rec)
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :method-param return-first)
