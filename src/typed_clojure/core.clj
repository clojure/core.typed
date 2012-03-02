(ns typed-clojure.core
  (:require [clojure-analyzer.compiler :as analyze]
            [typed-clojure.types :as t]
            [clojure.walk :as walk]
            [trammel.core :as c]))

(defrecord Union [types])

(defrecord TypeFilter [type])
(defrecord NotTypeFilter [not-type])
(defrecord ValueFilter [value])
(defrecord NotValueFilter [not-value])

(defrecord ImplFilter [if then])
(defrecord AndFilter [])
(defrecord OrFilter [ else])

(defrecord Fn [max-fixed-arity variadic refinements])
(defrecord Arity [max-fixed-arity variadic refinements])
(defrecord Refinement [dom rng])

(def ^:dynamic *type-check* false)

(def type-db (atom {}))

(defn lookup-var-type [id]
  (if-let [t (@type-db id)]
    t
    (throw (Exception. (str "No type for " id)))))

;; Primitives

(defn type-error [& msg]
  (throw (Exception. (apply str msg))))

;(+T ensure-type (IPersistentMap :-> Type))
(defmulti ensure-type :op)

(defmethod ensure-type :constant
  [{:keys [form]}]
  (map->Type {
   :type (class form)}))

(defn get-namespaced-var [env nme]
  (if (namespace nme)
    nme
    (symbol (str (-> env :ns :name) nme))))

(defmethod ensure-type :var
  [{:keys [env form info]}]
  (let [t (if-let [local (-> env :locals form)]
            (ensure-type (:init local))
            (let [ns-sym (get-namespaced-var env (-> info :name))]
               (lookup-var-type ns-sym)))]
    (map->Type {
     :type t})))

(defmethod ensure-type :def
  [{:keys [env form init name]}]
  (let [ns-sym (get-namespaced-var env name)
        expected-type (lookup-var-type ns-sym)
        actual-type (ensure-type (assoc init :type expected-type))]
  ;TODO  (unify-type expected-type actual-type)
    (map->Type {
     :type clojure.lang.Var})))

(defmethod ensure-type :if
  [{:keys [then else]}]
  (let [then-type (ensure-type then)
        else-type (ensure-type else)]
    (map->Union {
     :types #{then-type else-type}})))

(defmethod ensure-type :fn
  [{:keys [methods type]}]
  (let [arities (map (fn [{params :params variadic :variadic ret :ret}]
                       (let [arg-types (map (comp ::type meta) params)
                             rng (ensure-type ret)]
                         (map->Arity {
                          :variadic variadic
                          :params arg-types
                          :rng rng})))
                     methods)]
    (map->Fn {
     :arities arities})))

(defmethod ensure-type :invoke
  [{:keys [f args]}]
  (let [invoked-fn-type (ensure-type f)]
        ;TODO
        ))
;        same-arity-type (some (fn [^Arity a] 
;                                (and (= (count (.dom a)) (count args))
;                                     a))
;                              (.arrs ^Function invoked-fn-type))
;        _ (when-not same-arity-type
;            (type-error "Wrong number of arguments"))
;        _ (map type-check args (.dom ^Arity same-arity-type))
;        rng (.rng ^Arity same-arity-type)]
;    rng))

(defmethod ensure-type :let
  [{:keys [env bindings loop statements ret]}]
  (if-not loop
    (let [bnd-inits (map :init bindings)
          bnd-types (map ensure-type bindings)]
      (dorun (map ensure-type statements))
      (ensure-type ret))
    (throw (Exception. ":loop not implemented"))))

(defmethod ensure-type :default
  [{:keys [op]}]
  (when-not (#{:ns ::+T} op)
    (throw (Exception. (str op " not implemented, fell through")))))

(defmacro +T [id syntax type]
  (when *type-check*
    `(let [qualid# (if (namespace id)
                    id
                    (symbol (str (.name *ns*) id)))]
       (swap! type-db assoc qualid# ~type))))

;; Frontend type checker

(defn type-check-namespace [ns-sym]
  (binding [*type-check* true]
    (require ns-sym :reload-all)))
;  (let [ast (analyze/with-specials ['+T '+def '+let]
;              (analyze/analyze-namespace ns-sym))]
;    ))

(comment
  (type-check-namespace 'typed-clojure.test)
)
