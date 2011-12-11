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

(defmulti get-type :op)

(defmethod get-type :constant
  [{:keys [form]}]
  (cond (zero? form) t/+zero
        (integer? form) t/+integer
        (float? form) t/+float
        (number? form) t/+number
        :else t/+top))

;; TODO fix
(defmethod get-type :var
  [{:keys [env form]}]
  (if-let [t (lookup-local-type env form)]
         t
         (lookup-type (resolve form))))

(defmethod get-type :def
  [{:keys [form init]}]
  (let [id-var (-> form second resolve)
        id-type (lookup-type id-var)
        _ (assert id-type)]
    (type-check init id-type)))

(defmethod get-type :if
  [{:keys [then else]}]
  (let [then-type (get-type then)
        else-type (get-type else)]
    (t/+union then-type else-type)))

(defmethod get-type :fn
  [{:keys [methods]}]
  (let [arities (doall
                  (map (fn [{params :params variadic :variadic ret :ret}]
                         (assert (not variadic) "Variadic functions not yet supported")
                         (let [arg-types (doall (map (comp ::type meta) params))
                               rng (get-type ret)]
                           ;; TODO side effects from body
                           (t/->Arity arg-types rng)))
                       methods))]
    (t/->Function arities)))

(defmethod get-type :invoke
  [{:keys [f args]}]
  (let [invoked-fn-type (get-type f)
        same-arity-type (some (fn [^Arity a] 
                                (and (= (count (.dom a)) (count args))
                                     a))
                              (.arrs ^Function invoked-fn-type))
        _ (when-not same-arity-type
            (type-error "Wrong number of arguments"))
        _ (map type-check args (.dom ^Arity same-arity-type))
        rng (.rng ^Arity same-arity-type)]
    rng))

(defmethod get-type :let
  [{:keys [loop ret]}]
  (if-not loop 
    ;TODO side effects from body
    (get-type ret)
    (throw (Exception. ":loop not implemented"))))

(defmethod get-type :default
  [{:keys [op]}]
  (throw (Exception. (str op " not implemented, fell through"))))

;; Languages as Libraries, Sam TH
(defn type-check 
  "Type check a form returned from analyze"
  [exp-obj expected-type]
  (let [t (get-type exp-obj)]
    (when-not (t/subtype t expected-type)
      (type-error "Not a subtype: " t ", " expected-type))
    t))

;(defmacro type-check-form
;  "For debugging"
;  [form]
;  (let [t (type-check (analyze/analyze {} form))]
;    (str t)))

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
        type-obj (eval exec)
        {varname :name} (analyze/resolve-var env id)
        _ (assert (namespace varname) (name varname))]
    (swap! analyze/namespaces assoc-in [(symbol (namespace varname)) :types (symbol (name varname))] type-obj)
    {:env env :op :T :form form :type type-obj}))

(defn type-check-namespace [ns-sym]
  (analyze/with-specials ['T]
    (analyze/analyze-namespace ns-sym)))

(comment
  (type-check-namespace 'typed-clojure.test)
)
