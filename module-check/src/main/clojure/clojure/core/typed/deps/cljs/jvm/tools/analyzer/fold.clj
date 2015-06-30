(ns clojure.core.typed.deps.cljs.jvm.tools.analyzer.fold)

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

(add-default-fold-case :invoke
  (fn [expr _]
    (-> expr
      (update-in [:f] expr-rec)
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :var return-first)

(add-default-fold-case :constant return-first)

(add-default-fold-case :vector
  (fn [expr _]
    (-> expr
      (update-in [:items] map-expr-rec))))

(add-default-fold-case :set
  (fn [expr _]
    (-> expr
      (update-in [:items] map-expr-rec))))

(add-default-fold-case :list
  (fn [expr _]
    (-> expr
      (update-in [:items] map-expr-rec))))

(add-default-fold-case :map
  (fn [expr _]
    (-> expr
      (update-in [:keys] map-expr-rec)
      (update-in [:vals] map-expr-rec))))

(add-default-fold-case :let
  (fn [expr _]
    (-> expr
      (update-in [:bindings] (fn [bes] 
                               (doall
                                 (map #(update-in % [:init] expr-rec) bes))))
      (update-in [:expr] expr-rec))))

(add-default-fold-case :loop
  (fn [expr _]
    (-> expr
      (update-in [:bindings] (fn [bes] 
                               (doall
                                 (map #(update-in % [:init] expr-rec) bes))))
      (update-in [:expr] expr-rec))))

(add-default-fold-case :recur
  (fn [expr _]
    (-> expr
      (update-in [:exprs] map-expr-rec))))

(add-default-fold-case :do
  (fn [expr _]
    (-> expr
      (update-in [:statements] map-expr-rec)
      (update-in [:ret] expr-rec))))

(add-default-fold-case :fn
  (fn [expr _]
    (-> expr
      (update-in [:methods]
                 (fn [ms]
                   (map #(update-in % [:expr] expr-rec) ms))))))

(add-default-fold-case :try*
  (fn [expr _]
    (-> expr
      (update-in [:try] expr-rec)
      (update-in [:catch] expr-rec)
      (update-in [:finally] expr-rec))))

(add-default-fold-case :if
  (fn [expr _]
    (-> expr
      (update-in [:test] expr-rec)
      (update-in [:then] expr-rec)
      (update-in [:else] expr-rec))))

(add-default-fold-case :js
  (fn [expr _]
    (-> expr
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :deftype* return-first)

(add-default-fold-case :set! 
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec)
      (update-in [:val] expr-rec))))

(add-default-fold-case :dot
  (fn [expr _]
    (-> expr
      (update-in [:target] expr-rec)
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :throw
  (fn [expr _]
    (-> expr
      (update-in [:throw] expr-rec))))

(add-default-fold-case :new
  (fn [expr _]
    (-> expr
      (update-in [:ctor] expr-rec)
      (update-in [:args] map-expr-rec))))

(add-default-fold-case :letfn
  (fn [expr _]
    (-> expr
      (update-in [:bindings] (fn [bes] 
                               (doall
                                 (map #(update-in % [:init] expr-rec) bes))))
      (update-in [:expr] expr-rec))))

(add-default-fold-case :ns return-first)

(add-default-fold-case :def
  (fn [expr _]
    (-> expr
      (update-in [:init] #(if % (expr-rec %) %)))))

(add-default-fold-case :meta
  (fn [expr _]
    (-> expr
      (update-in [:expr] expr-rec)
      (update-in [:meta] expr-rec))))
