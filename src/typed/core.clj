(ns typed.core
  (:refer-clojure :exclude [type])
  (:require [analyze.core :refer [analyze-path]]))

;; Front end

(defmacro +T [sym type]
  `(ann (qualify-symbol '~sym) ~type))

(defmacro def-type [nme type & opts]
  (let [hopts (apply hash-map opts)
        rntime-class (:class opts)]
    `(def ~nme 
       (let [tc-name# (qualify-symbol '~nme)]
         (reify
           ITCType
           ITypeName
           (~'-type-name [this#]
             tc-name#)
           ISuperTypes
           (~'-supertype-of [this#]
             ~type)
           ~@(when rntime-class
               (list 'IRuntimeClass
                     `(~'-runtime-class [this#]
                          ~rntime-class))))))))

(declare parse-tvars tv-from-syntax)

(defmacro All [tvars type]
  (let [tvar-bndings (map first tvars)
        tvar-objs (mapv tv-from-syntax tvars)]
    `(reify
       IPolyConstructor
       (~'-tvars [this#]
         ~tvar-objs)

       (~'-poly-type [this#]
         (let [~@(mapcat #(vector %1 %2) tvar-bndings tvar-objs)]
           ~type))

       (~'-inst [const# types#]
         (reify 
           ITCType
           IPolyInst
           (~'-constructed-from [this#]
             const#)
           ISuperTypes
           (~'-supertype-of [this#]
             (throw (Exception. ""))))))))

(defmacro def-poly-type [nme tvars type & opts]
  (let [tvar-bndings (map first tvars)
        tvar-objs (mapv tv-from-syntax tvars)
        hopts (apply hash-map opts)
        rntime-class (:class opts)]
    `(def ~nme 
       (let [type-nme# (qualify-symbol '~nme)]
         (reify
           ITypeName
           (~'-type-name [this#]
             type-nme#)
           IPolyConstructor
           (~'-tvars [this#]
             ~tvar-objs)

           (~'-poly-type [this#]
             (let [~@(mapcat #(vector %1 %2) tvar-bndings tvar-objs)]
               ~type))

           (~'-inst [const# types#]
             (reify 
               ITCType
               ITypeName
               (~'-type-name [this#]
                 type-nme#)
               IPolyInst
               (~'-constructed-from [this#]
                 const#)
               ISuperTypes
               (~'-supertype-of [this#]
                 (throw (Exception. "")))
               IRuntimeClass
               (~'-runtime-class [this#]
                 ~rntime-class))))))))

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

;; Type Protocols

(defprotocol ITCType
  "Marker protocol for all Typed Clojure types"
  (-tctype-marker [this]))

(defprotocol ITypeName
  "Name of types"
  (-type-name [this] "Returns the name of type"))

(defprotocol IPolyInst
  "Marker protocol for polymorphic instances"
  (-constructed-from [this] "Returns the type used to construct this"))

(defprotocol IPolyConstructor
  "Marker protocol for polymorphic type constructors"
  (-poly-type [this] "Returns the polymorphic type of this")
  (-tvars [this] "Returns a sequential of type variables")
  (-inst [this types] "Returns an instance of this type, and ITCType, 
                      with type variables substituted with types"))

(defprotocol IRuntimeClass
  "Runtime class of ITCTypes"
  (-runtime-class [this] "Returns the class representing this ITCType"))

(defprotocol ISuperTypes
  "Extract supertypes of an ITCType type"
  (-supertype-of [this] "Returns the supertypes of an ITCType"))

(defprotocol IInstanceOf
  "Runtime predicates for ITCTypes"
  (-instance-of [this v] "True if v is an instance of ITCType this"))

;; Type Rep

(deftype FunctionType [dom rng has-uniform-rest uniform-rest]
  Object
  (toString [this]
    (str (vec (doall (concat dom 
                             (when has-uniform-rest
                               ['& uniform-rest '*])
                             ['-> rng]))))))
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

(defn U [& types]
  (if (= 1 (count types))
    (first types)
    (->UnionType types)))

(defn I [& types]
  (if (= 1 (count types))
    (first types)
    (->IntersectionType types)))

(defn Inst [this & types]
  (-inst this types))

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
    `(->TV '~(gensym nme) ~bound ~variance)))

;; Subtyping

(declare subtype?)

(defn tc-isa? [sub sup]
  (cond
    (and (instance? ITCType sub)
         (instance? ITCType sup))
    (subtype? sub sup)

    (and (instance? IRuntimeClass sub)
         (class? sup))
    (isa? (-runtime-class sub) sup)

    (and (class? sub)
         (class? sup))
    (isa? sub sup)

    :else (throw (Exception. "no case in tc-isa?"))))

(declare Any)

(def Any
  (let [nme (qualify-symbol 'Any)]
    (reify
      ITCType
      ITypeName
      (-type-name [this]
        nme)
      ISuperTypes
      (-supertype-of [this]
        Any)
      IInstanceOf
      (-instance-of [this v]
        true))))

(def Nothing
  (let [nme (qualify-symbol 'Nothing)]
    (reify
      ITCType
      ITypeName
      (-type-name [this]
        nme)
      ISuperTypes
      (-supertype-of [this]
        Nothing))))

(defn subtype? [sub sup]
  #_(cond
    (= Nothing sub) true
    :else (-subtype sub sup)))

(def-type EmptySeqable 
          Nothing)

(def-poly-type NonEmptySeqable [[a :variance :covariant]]
               Nothing)

(def-poly-type Seqable [[a :variance :covariant]]
               (U (Inst NonEmptySeqable a)
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
                     (Inst NonEmptyColl a))
                  (Inst Seqable a))
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
    (subtype? type (.t expected))))

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
  (assert false)
  (let [{cfexpr :fexpr
         cargs :args
         :as expr}
        (-> expr
          (update-in [:fexpr] check)
          (update-in [:args] #(map check %)))]
    expr))


(comment
(tc 'mini-typed.test.example)
  )
