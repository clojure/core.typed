(ns typed-clojure.core
  (:require [clojure-analyzer.compiler :as analyze]
            [typed-clojure.types :as t]
            [clojure.walk :as walk]
            [trammel.core :as c])
  (:import [typed_clojure.types Arity Function]))

;; Type ann database

(defn lookup-type [id]
;  (@analyze/namespaces *ns* 
    (throw (Exception. (str "No type annotation for " id))))

(defn lookup-local-type [env id]
  (assert (symbol? id))
  (analyze/resolve-var env id))

;; Primitives

(defn type-error [& msg]
  (throw (Exception. (apply str msg))))

(declare type-check)

(defn type-check* [exp-obj]
  (case (:op exp-obj)
    :constant (let [f (:form exp-obj)]
                (cond (zero? f) t/+zero
                      (integer? f) t/+integer
                      (float? f) t/+float
                      (number? f) t/+number
                      :else t/+top))

    :var (if-let [t (lookup-local-type (:env exp-obj) (:form exp-obj))]
           t
           (lookup-type (resolve (:form exp-obj))))

    :def (let [id-var (-> (:form exp-obj) second resolve)
               body (:init exp-obj)
               id-type (lookup-type id-var)
               _ (assert id-type)]
           (type-check body id-type))

    :if (let [then-type (type-check (:then exp-obj))
              else-type (type-check (:else exp-obj))]
          (t/+union then-type else-type))

    :fn (let [arities (doall
                        (map (fn [{params :params variadic :variadic ret :ret}]
                               (assert (not variadic) "Variadic functions not yet supported")
                               (let [arg-types (doall (map (comp ::type meta) params))
                                     rng (type-check ret)]
                                 ;; TODO side effects from body
                                 (t/->Arity arg-types rng)))
                             (:methods exp-obj)))]
          (t/->Function arities))

    :invoke (let [invoked-fn-type (type-check (-> exp-obj :f))
                  actual-args (-> exp-obj :args)
                  same-arity-type (some (fn [^Arity a] 
                                          (and (= (count (.dom a)) (count actual-args))
                                               a))
                                        (.arrs ^Function invoked-fn-type))
                  _ (when-not same-arity-type
                      (type-error "Wrong number of arguments"))
                  _ (map type-check actual-args (.dom ^Arity same-arity-type))
                  rng (.rng ^Arity same-arity-type)]
              rng)

    :let (if-not (:loop exp-obj)
           ;TODO side effects from body
           (type-check (:ret exp-obj))
           (throw (Exception. ":loop not implemented")))

    :ns nil ;;todo

    :T nil

    (throw (Exception. (str (:op exp-obj) " not implemented, fell through" exp-obj)))
    ))

;; Languages as Libraries, Sam TH
(defn type-check 
  "Type check a form returned from analyze"
  ([exp-obj]
   (type-check* exp-obj))
  ([exp-obj expected-type]
   (let [t (type-check* exp-obj)]
     (when-not (t/subtype t expected-type)
       (type-error "Not a subtype: " t ", " expected-type))
     t)))

(defmacro type-check-form
  "For debugging"
  [form]
  (let [t (type-check (analyze/analyze {} form))]
    (str t)))

;; Frontend

(defmacro def-T [name & body]
  `(def ~name ~@body))

(defn add-fn-arg-types 
  "Convert fn body from (fn [arg] ..) to (fn [[arg :- type]] ..)"
  [body ^Function type]
  (letfn [(prepare-param [name ptype]
            [name :- ptype])
          (prepare-arg-vector [v atypes]
            (vec (map prepare-param v atypes)))
          (prepare-method [[args & rest] ^Arity mtype]
            (cons (prepare-arg-vector args (.dom ^Arity mtype)) rest))]
    (if (vector? (first body))
      (do
        (assert (= 1 (count (.arrs type)))) ;; Assume only one arity
        (prepare-method body (first (.arrs type))))                ;; Single arity)
      (map prepare-method body (.arrs type))))) ;; Multi arity

(defmacro defn-T [name & body]
  `(def-T ~name
     (fn-T ~@(add-fn-arg-types body type))))

(defn normalize-fn-arg-types
  "Convert fn body from (fn [[arg :- type]] ...) to (fn [^{::type type} name] ..)"
  [body]
  (letfn [(prepare-param [[name _ type]]
            (with-meta name (merge {::type type} (meta name))))
          (prepare-arg-vector [v]
            (vec (map prepare-param v)))
          (prepare-method [[args & rest]]
            (cons (prepare-arg-vector args) rest))]
    (if (vector? (first body))
      (prepare-method body)        ;; Single arity
      (map prepare-method body)))) ;; Multi arity

(defmacro fn-T [& body]
  `(fn* ~@(normalize-fn-arg-types body))) ;; TODO should fn be a special form?

(defn normalize-lhs [lhs]
  (letfn [(normalize-type-syntax [form]
            (if (and (vector? form)
                     (= (second form) :-))
              (let [[name _ type] form]
                (with-meta name (merge {::type type} (meta name))))
              form))]
    (walk/walk normalize-type-syntax normalize-type-syntax lhs)))


(defn normalize-bindings-vector 
  "Convert binding vector from [[a :- IntegerT] 1] to [^{::type IntegerT} a 1]
  Handles destructuring"
  [bind-v]
  (let [clauses (partition 2 bind-v)
        norm-clauses (map (fn [[lhs rhs]]
                            [(normalize-lhs lhs) rhs])
                          clauses)]
    (vec (apply concat norm-clauses))))

(defmacro let-T [bindings & body]
  `(let ~(normalize-bindings-vector bindings) ~@body))

(defmacro T [id syntax type]
  nil)

;; Frontend type checker

(defmethod analyze/parse 'T
  [_ env [_ id _ type :as form] _]
  (let [t (analyze/analyze env type)
        exec (analyze/emit t)
        type-obj (eval exec)]
    {:env env :op :T :form form :type type-obj}))

(defn type-check-namespace [ns-sym]
  (analyze/with-specials ['T]
    (println "specials: " analyze/*specials*)
    (analyze/analyze-namespace ns-sym)))

(comment
  (type-check-namespace 'typed-clojure.test)
)
