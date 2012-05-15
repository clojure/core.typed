(ns typed.core
  (:refer-clojure :exclude [type])
  (:require [analyze.core :refer [analyze-path]]))

;; Front end

(defmacro +T [sym type]
  `(ann (qualify-symbol '~sym) ~type))

(defmacro def-type [nme type]
  `(def ~nme ~type))

(defmacro def-poly-type [nme tvars type]
  `(def ~nme (All ~tvars ~type)))

(defn qualify-symbol [sym]
  (if (namespace sym)
    sym
    (symbol (-> *ns* ns-name name) (name sym))))

;; Type environments

;map from symbols to types
(def VAR-ANNOTATIONS (atom {}))

(defn ann [sym type]
  (swap! VAR-ANNOTATIONS assoc sym type)
  type)

;; Type Rep

(deftype FunctionType [arities])
(deftype ArityType [dom rng rest])
(deftype PolyConstructorType [tvars scope])
(deftype PolyInstanceType [constructor insts])
(deftype TV [nme bound variance])
(deftype UnionType [types])
(deftype CaseType [types])
(deftype IntersectionType [types])

;; Type Constructors

(defmacro Fn [& arities]
  `(->Function 
     (map arity-from-syntax ~(mapv massage-arity-syntax arities))))

(defmacro All [tvars type]
  (let [normalised (normalise-tv-syntax tvars)
        nmes (map first normalised)]
    `(let [~@(mapcat vector nmes (map tv-from-syntax normalised))]
       (->PolyConstructorType (map tv-from-syntax ~normalised) ~type))))

(defn U [& types]
  (->UnionType types))

(defn Case [& types]
  (->CaseType types))

(defn I [& types]
  (->IntersectionType types))

(defn Inst [this & types]
  {:pre [(instance? PolyConstructorType this)]}
  (->PolyConstructorType this types))

;; Syntax helpers

(defn- massage-arity-syntax [arity-syntax]
  (replace {'& :& '-> :->} arity-syntax))

(defn arity-from-syntax [arity-syntax]
  (let [[dom rng] (split-with #(= :-> %) arity-syntax)
        [fixed-dom rest-dom] (split-with #(= :& %) dom)]
    (->Arity fixed-dom rng rest-dom)))

(def ^:private bound-syn :<)

(defn tv-from-syntax [[nme & opt-list]]
  (let [opts (apply hash-map opt-list)
        variance (:variance opts)
        bound (if (contains? opts bound-syn)
                (bound-syn opts)
                `Any)]
    `(->TV '~name ~bound ~variance)))

;; Base types

(def-type Any (U Object nil))
(def-type Nothing (U))

(def-type NonseqableSeqable
          (U Iterable
             ;array
             CharSequence
             java.util.Map))

(def-poly-type Seqable [[a :variance :covariant]]
               (U clojure.lang.Seqable
                  NonseqableSeqable))

(def-poly-type Option [[a :variance :covariant]]
               (Case a 
                     nil))

(def-poly-type Coll [[a :variance :covariant]]
               (I clojure.lang.IPersistentCollection
                  (Inst Seqable a)))

(def-poly-type Seq [[a :variance :covariant]]
               (I clojure.lang.ASeq
                  (Inst Seqable a)
                  (Inst Coll a)))

;; Subtype

(defmulti subtype (fn [type sup] [(class type) (class sup)]))

(defmethod subtype [Class Class]
  [type sup]
  (isa? type sup))

;; Checker

(def type-key ::type)

(declare check)

(defn tc 
  "Type check namespace nsym"
  [nsym]
  (let [asts (analyze-path nsym)]
    (doseq [a asts]
      (check a))))

(deftype ExpectedType [t])

(defn assert-expected-type [type expected]
  {:pre [(or (nil? expected)
             (instance? ExpectedType expected))]}
  (when expected
    (subtype type (.t expected))))

(defmulti check :op)

(defmethod check :do
  [expr & [expected-type]]
  (let [{cexprs :exprs
         :as expr}
        (-> expr
          (update-in [:exprs] #(concat (map check (butlast %))
                                       [(check (last %) expected-type)])))
        
        _ (assert-expected-type (-> cexprs last type-key) expected-type)]
    (assoc expr
           type-key (-> cexprs last type-key))))

(defmethod check :invoke
  [expr & [expected-type]]
  (let [{cfexpr :fexpr
         cargs :args
         :as expr}
        (-> expr
          (update-in [:fexpr] check)
          (update-in [:args] #(map check %)))

        fn-type (type-key cfexpr)
        arg-types (map type-key cargs)
        inst-fn-type (if (instance? PolyConstructorType fn-type)
                       (synthesise-type-args fn-type arg-types)
                       fn-type)
        _ (assert (subtype inst-fn-type clojure.lang.IFn))]
    expr))


(comment
(tc 'mini-typed.test.example)
  )
