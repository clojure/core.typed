(ns typed-clojure.infer
  (:require [analyze.core :as a]
            [analyze.util :as util]))

(defmacro ast [form]
  `(-> (a/analyze-one {:ns {:name '~'user} :context :eval} '~form)
    (util/print-expr :children :Expr-obj :LocalBinding-obj :ObjMethod-obj :env)))

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

(defrecord Any [])
(defrecord Nothing [])
(defrecord NilType [])
(defrecord Fun [arities])
(defrecord TClass [the-class])
(defrecord Union [types])

(def types #{Any Nothing Fun TClass Union NilType})

(defprotocol IArity
  (matches-args [this args] "Return the arity if it matches the number of args,
                            otherwise nil"))

(defrecord FixedArity [dom rng]
  IArity
  (matches-args [this args]
    (when (= (count dom)
             (count args))
      this)))

(defrecord VariableArity [fixed-dom rest-type rng]
  IArity
  (matches-args [this args]
    (when (<= (count fixed-dom)
              (count args))
      this)))

(def arities #{FixedArity VariableArity})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inheritance relationships

(doseq [t types]
  (derive t ::any-type))

(doseq [t (remove #(= Nothing %) types)]
  (derive t ::any-type-but-Nothing))

(doseq [t (remove #(= Any %) types)]
  (derive t ::any-type-but-Any))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping


(defmulti subtype? (fn [s t] (vec (map class [s t]))))
(defmulti subtype-arity? (fn [s t] (vec (map class [s t]))))

(defmethod subtype? [::any-type Any]
  [s t]
  true)

(defmethod subtype? [Nothing ::any-type]
  [s t]
  true)

(defmethod subtype? [Any Nothing]
  [s t]
  false)

(defmethod subtype? [Any ::any-type-but-Any]
  [s t]
  false)

(defmethod subtype? [::any-type-but-Nothing Nothing]
  [s t]
  false)

(defmethod subtype? [Fun Fun]
  [s t]
  (map-all-true? subtype-arity? (.arities s) (.arities t)))

(defmethod subtype-arity? [FixedArity FixedArity]
  [s t]
  (and (map-all-true? subtype? (.dom t) (.dom s))
       (subtype? (.rng s) (.rng t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Bidirectional checking (Local Type Inference (2000) Pierce & Turner, Chapter 4)

(defmulti check :op)
(defmulti synthesize :op)

(defmethod check :def
  [{:keys [var init init-provided] :as expr}]
  (assert init-provided)
  (let [synthesized-var-expr (synthesize var)
        var-type (::+T synthesized-var-expr)
        checked-init-expr (-> init
                            (assoc ::+T var-type)
                            check)]
    (assoc expr
           :var synthesized-var-expr
           :init checked-init-expr)))

(defmethod check :invoke
  [{:keys [fexpr args] :as expr}]
  (assert (::+T expr) "Checking context for function invocation requires full
                      type annotations")
  (let [return-type (::+T expr)
        synthesized-fexpr (synthesize fexpr)
        fexpr-type (::+T synthesized-fexpr)
        arity-type (some #(matches-args % args) fexpr-type)

        _ (assert arity-type)
        _ (assert (instance? FixedArity arity-type))

        checked-args (map #(-> %1
                             (assoc ::+T %2)
                             check) 
                          args (.dom arity-type))]
    (assoc expr
           :fexpr synthesized-fexpr
           :args checked-args)))

(comment

{:op :fn-expr,
 :methods
 ({:op :fn-method,
   :body
   {:op :do,
    :exprs
    ({:op :local-binding-expr,
      :local-binding {:op :local-binding, :sym a, :tag nil, :init nil},
      :tag nil})},
   :required-params
   ({:op :local-binding, :sym a, :tag nil, :init nil}),
   :rest-param nil}),
 :variadic-method nil,
 :tag nil})
