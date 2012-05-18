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

(deftype FunctionType [dom rng has-uniform-rest uniform-rest]
  Object
  (toString [this]
    (str (vec (doall (concat dom 
                             (when has-uniform-rest
                               ['& uniform-rest '*])
                             ['-> rng]))))))
(deftype PolyConstructorType [tvars scope]
  Object
  (toString [this]
    (str (list 'All (mapv #(vector (.nme %) :< (.bound %) :variance (.variance %)) tvars)
               scope))))
(deftype PolyInstanceType [constructor insts])
(deftype TV [nme bound variance]
  Object
  (toString [this]
    (str nme)))
(deftype UnionType [types]
  Object
  (toString [this]
    (str (apply list 'U types))))
(deftype IntersectionType [types]
  Object
  (toString [this]
    (str (apply list 'I types))))

;; Type Constructors

(declare massage-function-syntax normalise-tv-syntax tv-from-syntax)

(defmacro Fn [& arities]
  `(apply I (map function-from-syntax ~(mapv massage-function-syntax arities))))

(defmacro All [tvars type]
  (let [normalised tvars
        nmes (map first normalised)]
    `(let [~@(mapcat vector nmes (map tv-from-syntax normalised))]
       (->PolyConstructorType ~(mapv tv-from-syntax normalised) ~type))))

(defn U [& types]
  (if (= 1 (count types))
    (first types)
    (->UnionType types)))

(defn I [& types]
  (if (= 1 (count types))
    (first types)
    (->IntersectionType types)))

(defn Inst [this & types]
  {:pre [(instance? PolyConstructorType this)]}
  (->PolyConstructorType this types))

;; Syntax helpers

(defn- massage-function-syntax [fun-syntax]
  (replace {'& :& '-> :-> '* :*} fun-syntax))

;takes massaged syntax
(defn function-from-syntax [fun-syntax]
  (let [[dom [rng]] (split-with #(= :-> %) fun-syntax)
        [fixed-dom rest-syn] (split-with #(= :& %) dom)
        [has-uniform-rest uniform-rest]
        (when (= ':* (last rest-syn))
          [true (first rest-syn)])]
    (->FunctionType fixed-dom rng has-uniform-rest uniform-rest)))

(def ^:private bound-syn :<)

(defn tv-from-syntax [[nme & opt-list]]
  (let [opts (apply hash-map opt-list)
        variance (:variance opts)
        bound (if (contains? opts bound-syn)
                (bound-syn opts)
                `Any)]
    `(->TV '~nme ~bound ~variance)))

;; Type Protocols

(defprotocol ITCType
  "Marker protocol for all Typed Clojure types"
  (-tctype-marker [this]))

(defprotocol IPolyInst
  "Marker protocol for polymorphic instances"
  (-polyinst-marker [this]))

(defprotocol IPolyConstructor
  "Marker protocol for polymorphic type constructors"
  (-polyconstructor-marker [this]))

(defprotocol ISubtype
  "Subtyping between ITCTypes"
  (-subtype [this sup] "True if this is a subtype of sup"))

(defprotocol IRuntimeClass
  "Runtime class of ITCTypes"
  (-runtime-class [this] "Returns the class representing this ITCType"))

(defprotocol ISuperTypes
  "Extract supertypes of an ITCType type"
  (-supertype-of [this] "Returns the supertypes of an ITCType"))

(defprotocol IInstanceOf
  "Runtime predicates for ITCTypes"
  (-instance-of [this v] "True if v is an instance of ITCType this"))

;; Subtyping

(defn tc-isa? [sub sup]
  (cond
    (and (instance? ISubtype sub)
         (instance? ISubtype sup))
    (-subtype sub sup)

    (and (instance? IRuntimeClass sub)
         (class? sup))
    (isa? (-runtime-class sub) sup)

    (and (class? sub)
         (class? sup))
    (isa? sub sup)

    :else (throw (Exception. "no case in tc-isa?"))))

(defn subtype? [sub sup]
  (cond
    (= Any sup) true
    (instance? ISubtype sub) (-subtype sub sup)))

(deftype Any []
  ITCType
  ISubtypesOf
  (-supertype-of [this]
    Any)
  ISubType
  (-subtype [this sup]
    (identical? Any sup))
  IInstanceOf
  (-instance-of [this v]
    true))

(deftype Nothing []
  ITCType
  ISubtypesOf
  (-supertype-of [this]
    Any)
  ISubType
  (-subtype [this _]
    true))

(def-type EmptySeqable 
          Nothing)

(def-poly-type NonEmptySeqable [[a :variance :covariant]]
               Nothing)

(def-poly-type Seqable [[a :variance :covariant]]
               (U (NonEmptySeqable a)
                  EmptySeqable))

(def-poly-type Option [[a :variance :covariant]]
               (U a nil))

(def-type EmptyColl
          Nothing
          :class clojure.lang.IPersistentCollection)

(def-poly-type NonEmptyColl [[a :variance :covariant]]
               Nothing
               :class clojure.lang.IPersistentCollection)

(def-poly-type Coll [[a :variance :covariant]]
               (I (U EmptyColl
                     (Inst NonEmpty a))
                  (Inst AnySeqable a))
               :class clojure.lang.IPersistentCollection)

(def-type EmptySeq 
          EmptySeqable
          :class clojure.lang.ISeq)

(def-poly-type NonEmptySeq [[a :variance :covariant]]
               (Inst NonEmptySeqable a)
               :class clojure.lang.ASeq)

(def-poly-type Seq [[a :variance :covariant]]
               (I (U EmptySeq
                     (Inst NonEmptySeq a))
                  (Inst Seqable a)
                  (Inst Coll a))
               :class clojure.lang.ISeq)

(def-type EmptyVector
          EmptySeqable
          :class clojure.lang.IPersistentVector)

(def-poly-type NonEmptyVector [[a :variance :covariant]]
               (Inst NonEmptySeqable a)
               :class clojure.lang.IPersistentVector)

(def-poly-type Vector [[a :variance :covariant]]
               (I (U EmptyVector
                     (Inst NonEmptyVector a))
                  (Inst Seqable a)
                  (Inst Coll a))
               :class clojure.lang.IPersistentVector)

(+T clojure.core/seq
    (All [[x :variance :covariant]]
         (Fn [(EmptySeqable x) -> nil]
             [(NonEmptySeqable x) -> (Seq x)]
             [(Seqable x) -> (U (Seq x) nil)])))

(+T clojure.core/first
    (All [[x :variance :covariant]]
         (Fn [EmptySeqable -> nil]
             [(NonEmptySeqable x) -> x]
             [(Seqable x) -> (U x nil)])))

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

(declare synthesise-type-args)

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
