(ns typed.core
  (:refer-clojure :exclude [type])
  (:import (clojure.lang Seqable IPersistentCollection IPersistentVector IPersistentStack
                         Associative IFn AFn ISeq ASeq ILookup APersistentVector
                         PersistentVector)
           (java.util Map))
  (:require [analyze.core :refer [analyze-path]]))

;; Front end

(defmacro +T [sym type]
  `(ann (qualify-symbol '~sym) ~type))

(defmacro def-type [nme type & opts]
  (let [hopts (apply hash-map opts)
        rntime-class (:class opts)]
    `(def ~nme 
       (let [tc-name# (qualify-symbol '~nme)]
         (->NewType '~nme ~type ~rntime-class)))))

(declare tv-from-syntax)

(defmacro All [tvars type]
  (let [tvar-bndings (map first tvars)
        tvar-objs (mapv tv-from-syntax tvars)]
    `(->PolyConstructor
       nil
       ~tvar-objs
       (let [~@(mapcat #(vector %1 %2) tvar-bndings tvar-objs)]
         ~type)
       nil)))

(defmacro def-poly-type [nme tvars type & opts]
  (let [tvar-bndings (map first tvars)
        tvar-objs (mapv tv-from-syntax tvars)
        hopts (apply hash-map opts)
        rntime-class (:class opts)]
    `(def ~nme 
       (let [type-nme# (qualify-symbol '~nme)]
         (->PolyConstructor
           '~nme
           ~tvar-objs
           (let [~@(mapcat vector tvar-bndings tvar-objs)]
             ~type)
           ~rntime-class)))))

(defmacro restrict-poly-class [the-class tvars & opts]
  (let [opts-m (apply hash-map opts)
        tvar-bndings (map first tvars)
        tvar-objs (mapv tv-from-syntax tvars)
        restrictions (:restrict opts-m)]
    `(do
       (add-restricted-class-constructor
         ~the-class
         (->PolyRestrictedClassConstructor 
           ~the-class 
           ~tvar-objs
           (let [~@(mapcat vector tvar-bndings tvar-objs)]
             ~restrictions)))
       ~the-class)))

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
  (-constructed-from [this] "Returns the type used to construct this")
  (-inst-from [this] "Returns the types used to instantiate this type"))

(defprotocol IPolyConstructor
  "Marker protocol for polymorphic type constructors"
  (-poly-type [this] "Returns the polymorphic type of this")
  (-tvars [this] "Returns a sequential of type variables")
  (-inst [this types] "Returns an instance of this type, and ITCType, 
                      with type variables substituted with types"))

(defprotocol IRestrictedClassConstructor
  "Protocol for restricted class constructors"
  (-inst-restricted-class [this types] "Returns an instances of this"))

(defprotocol IRuntimeClass
  "Runtime class of ITCTypes"
  (-runtime-class [this] "Returns the class representing this ITCType"))

(defprotocol ISuperType
  "Extract supertypes of an ITCType type"
  (-get-supertype [this] "Returns the supertypes of an ITCType"))

(defprotocol IInstanceOf
  "Runtime predicates for ITCTypes"
  (-instance-of [this v] "True if v is an instance of ITCType this"))

(defprotocol IVarVisit
  (-visit-tvar [this f]))

;; Type Rep

(deftype RestrictedClass [class restrictions])
(deftype PolyRestrictedClassInstance [class constructor restrictions])
(deftype PolyRestrictedClassConstructor [class tvars restrictions]
  IRestrictedClassConstructor
  (-inst-restricted-class [this types]
    (->PolyRestrictedClassInstance
      class
      this
      (into {}
            (map (fn [[cl type]]
                   [cl (-visit-tvar type (fn [old]
                                           (if-let [[tv typ] (some #(and (= % old)
                                                                         (vector %1 %2) )
                                                                   tvars
                                                                   types)]
                                             (do (assert (subtype? typ (.bound tv)))
                                               typ)
                                             this)))])
                 restrictions)))))

;class -> PolyRestrictedClassConstructor
(def RESTRICTED-CLASS-CONSTRUCTORS (atom {}))
;class -> RestrictedClass
(def RESTRICTED-CLASSES (atom {}))

(defn add-restricted-class-constructor [class constr]
  (assert (class? class))
  (assert (instance? PolyRestrictedClassConstructor constr))
  (swap! RESTRICTED-CLASS-CONSTRUCTORS assoc class constr))

(deftype PolyInstance [nme inst-from constructor type rntime-class]
  ITCType
  ITypeName
  (-type-name [this]
    nme)
  IPolyInst
  (-constructed-from [this]
    constructor)
  (-inst-from [this]
    inst-from)
  ISuperType
  (-get-supertype [this]
    (throw (Exception. "")))
  IRuntimeClass
  (-runtime-class [this]
    rntime-class))

(declare subtype?)

(deftype PolyConstructor [nme tvars type rntime-class]
  ITypeName
  (-type-name [this]
    nme)

  IPolyConstructor
  (-tvars [this]
    tvars)
  (-poly-type [this]
    type)

  (-inst [this types]
    (->PolyInstance nme 
                    this
                    types
                    (-visit-tvar type (fn [old]
                                        (if-let [[tv typ] (some #(and (= % old)
                                                                      (vector %1 %2) )
                                                                tvars
                                                                types)]
                                          (do (assert (subtype? typ (.bound tv)))
                                            typ)
                                          this)))
                    rntime-class)))

(deftype NewType [nme type rntime-class]
  ITCType
  ITypeName
  (-type-name [this]
    nme)
  ISuperType
  (-get-supertype [this]
    type)
  IRuntimeClass
  (-runtime-class [this]
    rntime-class))

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

(deftype Literal [value]
  ITCType
  IRuntimeClass
  (-runtime-class [this]
    (class value)))

(defn poly? [a]
  (instance? PolyConstructor a))
(defn function? [a]
  (instance? FunctionType a))
(defn tv? [a]
  (instance? tv? a))

;; Type Constructors

(declare massage-function-syntax)

;TODO IFn constructor, for invocable things
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

(defn inst-poly-class [pclass types]
  (if-let [contr (@RESTRICTED-CLASS-CONSTRUCTORS pclass)]
    (-inst-restricted-class contr types)
    (throw (Exception. (str "Cannot instantiate " pclass)))))

(defn Inst [this & types]
  (if (class? this)
    (inst-poly-class this types)
    (-inst this types)))

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

;; Visit tvar

(extend-protocol IVarVisit
  PolyRestrictedClassConstructor
  (-visit-tvar [this f]
    (->PolyRestrictedClassConstructor
      (.class this)
      (map -visit-tvar (.tvars this) f)
      (into {}
            (map (fn [[cl type]]
                   [cl (-visit-tvar type f)])
                 (.restrictions this)))))

  PolyRestrictedClassInstance
  (-visit-tvar [this f]
    (->PolyRestrictedClassInstance
      (.class this)
      (-visit-tvar (.constructor this) f)
      (into {}
            (map (fn [[cl type]]
                   [cl (-visit-tvar type f)])
                 (.restrictions this)))))

  NewType
  (-visit-tvar [this f]
    (->NewType 
      (.nme this)
      (-visit-tvar (.type this) f)
      (.rntime-class this)))
  NewType
  (-visit-tvar [this f]
    (->NewType 
      (.nme this)
      (-visit-tvar (.type this) f)
      (.rntime-class this)))

  PolyInstance
  (-visit-tvar [this f]
    (->PolyInstance
      (.nme this)
      (map -visit-tvar (.inst-from this) (repeat f))
      (-visit-tvar (.constructor this) f)
      (-visit-tvar (.type this) f)
      (.rntime-class this)))

  PolyConstructor
  (-visit-tvar [this f]
    (->PolyConstructor
      (.nme this)
      (map -visit-tvar (.tvars this) (repeat f))
      (-visit-tvar (.type this) f)
      (.rntime-class this)))

  TV
  (-visit-tvar [this f]
    (f this))

  UnionType
  (-visit-tvar [this f]
    (->UnionType
      (map -visit-tvar (.types this) (repeat f))))

  Class
  (-visit-tvar [this f]
    this)

  FunctionType
  (-visit-tvar [this f]
    (->FunctionType
      (map -visit-tvar (.dom this) (repeat f))
      (-visit-tvar (.rng this) f)
      (.has-uniform-rest this)
      (if (.has-uniform-rest this)
        (-visit-tvar (.uniform-rest this) f)
        (.uniform-rest this)))))

;; Variable Elimination

(defprotocol IVarElim
  (-promote [this vs])
  (-demote [this vs]))

(declare Nothing Any)

(extend-protocol IVarElim
  NewType
  (-promote [this vs]
    (->NewType 
      (.nme this)
      (-promote (.type this) vs)
      (.rntime-class this)))
  (-demote [this vs]
    (->NewType 
      (.nme this)
      (-demote (.type this) vs)
      (.rntime-class this)))

  PolyInstance
  (-promote [this vs]
    (->PolyInstance
      (.nme this)
      (map -promote (.inst-from this) (repeat vs))
      (-promote (.constructor this) vs)
      (-promote (.type this) vs)
      (.rntime-class this)))
  (-demote [this vs]
    (->PolyInstance
      (.nme this)
      (map -demote (.inst-from this) (repeat vs))
      (-demote (.constructor this) vs)
      (-demote (.type this) vs)
      (.rntime-class this)))

  PolyConstructor
  (-promote [this vs]
    (->PolyConstructor
      (.nme this)
      (.tvars this) ;type variables are uniquely named, guaranteed not in vs
      (-promote (.type this) vs)
      (.rntime-class this)))
  (-demote [this vs]
    (->PolyConstructor
      (.nme this)
      (.tvars this) ;type variables are uniquely named, guaranteed not in vs
      (-demote (.type this) vs)
      (.rntime-class this)))

  TV
  (-promote [this vs]
    (if (contains? vs this)
      (.bound this)
      this))
  (-demote [this vs]
    (if (contains? vs this)
      Nothing
      this))

  UnionType
  (-promote [this vs]
    (->UnionType
      (map -promote (.types this) (repeat vs))))
  (-demote [this vs]
    (->UnionType
      (map -demote (.types this) (repeat vs))))

  FunctionType
  (-promote [this vs]
    (->FunctionType
      (map -demote (.dom this) (repeat vs))
      (-promote (.rng this) vs)
      (.has-uniform-rest this)
      (if (.has-uniform-rest this)
        (-demote (.uniform-rest this) vs)
        (.uniform-rest this))))
  (-demote [this vs]
    (->FunctionType
      (map -promote (.dom this) (repeat vs))
      (-demote (.rng this) vs)
      (.has-uniform-rest this)
      (if (.has-uniform-rest this)
        (-promote (.uniform-rest this) vs)
        (.uniform-rest this)))))

;; Constraint Generation

(deftype EqConstraint [type])
(deftype SubConstraint [lower upper])

(defmulti intersect-cs-fn (fn [a b] [(class a) (class b)]))

(defmethod intersect-cs-fn [EqConstraint EqConstraint]
  [a b]
  (assert (= a b))
  a)

(defmethod intersect-cs-fn [EqConstraint SubConstraint]
  [a b]
  (assert (and (subtype? (.lower b) (.type a))
               (subtype? (.type a) (.upper b))))
  a)

(defmethod intersect-cs-fn [SubConstraint EqConstraint]
  [b a]
  (assert (and (subtype? (.lower b) (.type a))
               (subtype? (.type a) (.upper b))))
  a)

(defmethod intersect-cs-fn [SubConstraint SubConstraint]
  [b a]
  (let [j (U (.lower a) (.lower b))
        m (I (.upper a) (.upper b))]
    (->SubConstraint j m)))

(defn intersect-cs [& cs]
  (merge-with intersect-cs-fn cs))

(defn empty-cs [xs]
  (zipmap xs (repeat (->SubConstraint Nothing Any))))
(defn singleton-cs [xs x c]
  (intersect-cs (empty-cs xs) {x c}))

(defn matching-rel 
  "Returns [r cs] where r is a type such that sub == type,
  and cs are the constraints"
  [sub type xs]
  (cond
    (and (= Any sub)
         (= Any type))
    [Any (empty-cs xs)]

    (and (= Nothing sub)
         (= Nothing type))
    [Nothing (empty-cs xs)]

    (and (tv? sub)
         (contains? xs sub))
    [type (singleton-cs xs sub (->EqConstraint type))]

    (and (tv? type)
         (contains? xs type))
    [sub (singleton-cs xs type (->EqConstraint sub))]

    (and (tv? sub)
         (tv? type)
         (= sub type))
    [sub (empty-cs xs)]

    (and (poly? sub)
         (poly? type)
         (= (map #(.nme %) (-tvars sub))
            (map #(.nme %) (-tvars type))))
    (let [sub-tvars (-tvars sub)
          type-tvars (-tvars type)
          _ (assert (= sub-tvars type-tvars))
          [new-tvars ds] (let [res (map #(matching-rel (.bound %1) (.bound %2) xs)
                                        sub-tvars
                                        type-tvars)
                               ks-bnds (map first res)
                               new-tvars (map #(->TV
                                                 (.nme %1)
                                                 %2
                                                 (.variance %2))
                                              sub-tvars
                                              ks-bnds)
                               ds (intersect-cs (map second res))]
                           [new-tvars ds])
          rplc-fn (fn [old]
                    (if-let [ntvar (some #(.nme new-tvars) old)]
                      (->TV
                        (.nme old)
                        (.bound ntvar)
                        (.variance old))
                      old))
          sub-rplced (-visit-tvar (-poly-type sub) rplc-fn)
          type-rplced (-visit-tvar (-poly-type type) rplc-fn)
          [mtch-type cs] (matching-rel sub-rplced type-rplced xs)
          all-cs (intersect-cs cs ds)]
      [(->PolyConstructor
         (.nme sub)
         new-tvars
         mtch-type
         (.rntime-class sub))
       all-cs])

    (and (function? sub)
         (function? type))
    (let [rs (.dom sub)
          s (.rng sub)
          ts (.dom type)
          u (.rng type)
          rest-sub (.uniform-rest sub)
          rest-type (.uniform-rest type)
          [ls cs] (let [res (map #(matching-rel %1 %2 xs)
                                 ts
                                 rs)
                        ls (map first res)
                        cs (apply intersect-cs (map second res))]
                    [ls cs])
          [m d] (matching-rel s u xs)
          [rest e] (when (.has-uniform-rest sub)
                     (matching-rel rest-sub rest-type xs))]
      [(->FunctionType
         ls
         m
         (.has-uniform-rest sub)
         rest)
       (apply intersect-cs (concat [d] (when e [e]) cs))])))

(defn gen-constraint [sub type xs]
  (cond
    (or (= Any type)
        (= Nothing sub))
    (empty-cs xs)

    (and (tv? sub)
         (tv? type)
         (= sub type))
    (empty-cs xs)

    (and (tv? sub)
         (contains? xs sub))
    (let [r (-demote type xs)]
      (singleton-cs xs sub (->SubConstraint Nothing r)))

    (and (tv? type)
         (contains? xs type))
    (let [r (-promote sub xs)]
      (singleton-cs xs type (->SubConstraint r Any)))

    (tv? sub)
    (gen-constraint (.bound sub) type xs)

    (and (poly? sub)
         (poly? type)
         (= (map #(.nme %) (-tvars sub))
            (map #(.nme %) (-tvars type))))
    (let [res (map #(matching-rel (.bound %1) (.bound %2) xs)
                   (-tvars sub)
                   (-tvars type))
          ks (map first res)
          ds (map second res)

          rplc-fn (fn [old]
                    (if-let [t (some #(.nme old) (-tvars sub))]
                      (->TV
                        (.nme old)
                        (.bound t)
                        (.variance old))
                      old))
          cs (gen-constraint (-visit-tvar (.type sub) rplc-fn)
                             (-visit-tvar (.type type) rplc-fn))]
      (apply intersect-cs cs ds))

    (and (function? sub)
         (function? type))
    (let [_ (assert (= (.has-uniform-rest sub) (.has-uniform-rest type))) ;assume both have rest or both don't
          rs (.dom sub)
          s (.rng sub)
          ts (.dom type)
          u (.rng type)
          rest-sub (.uniform-rest sub)
          rest-type (.uniform-rest type)
          _ (assert (= (count ts) (count rs))) ;assume same length args always
          cs (apply intersect-cs 
                    (map #(gen-constraint %1 %2 xs)
                         ts
                         rs))
          d (gen-constraint s u xs)
          e (when (.has-uniform-rest sub)
              (gen-constraint rest-type rest-sub xs))]
      (apply intersect-cs (concat d (when e [e]) cs)))))

(declare Any)

(def Any
  (let [nme (qualify-symbol 'Any)]
    (reify
      ITCType
      ITypeName
      (-type-name [this]
        nme)
      IInstanceOf
      (-instance-of [this v]
        true))))

(def AnyType (class Any))

(def Nothing
  (let [nme (qualify-symbol 'Nothing)]
    (reify
      ITCType
      ITypeName
      (-type-name [this]
        nme))))

(def NothingType (class Nothing))

(def Nil
  (let [nme (qualify-symbol 'Nil)]
    (reify
      ITCType
      ITypeName
      (-type-name [this]
        nme))))

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

(defmulti subtype? (fn [sub sup] [(class sub) (class sup)]))

(defmethod subtype? [NothingType AnyType] [_ _] true)
(defmethod subtype? [NothingType Object] [_ _] true)
(defmethod subtype? [Object AnyType] [_ _] true)

(defmethod subtype? [NewType NewType] 
  [sup type]
  (= sup type))

;(defmethod subtype? [PolyInstance PolyInstance]
;  [sup type]
;  (cond
;    (= (-type-name sup)
;       (-type-name type))
;    (let [tvars (-> sup -constructed-from -tvars)
;          sup-insts (-inst-from sup)
;          type-insts (-inst-from type)]
;      (every? true?
;              (map (fn [v s t]
;                     (case (.variance v)
;                       :covariant (subtype? s t)
;                       :contravariant (subtype? t s)
;                       :invariant false))
;                   tvars
;                   sup-insts
;                   type-insts)))
;    :else (subtype? (.type
;    ))))

;(defmethod subtype? [UnionType UnionType]
;
;(defmethod subtype? [ISuperType ISuperType] 
;  [sub type]
  

;#; Base Types

(restrict-poly-class Seqable [[a :variance :covariant]])

(restrict-poly-class IPersistentCollection [[a :variance :covariant]]
                     :restrict {Seqable (Inst Seqable a)})

(restrict-poly-class ISeq [[a :variance :covariant]]
                     :restrict {Seqable (Inst Seqable a)
                                IPersistentCollection (Inst IPersistentCollection a)})

(restrict-poly-class ASeq [[a :variance :covariant]]
                :restrict {Seqable (Inst Seqable a)
                           IPersistentCollection (Inst IPersistentCollection a)
                           ISeq (Inst ISeq a)})

(restrict-poly-class ILookup [[a :variance :invariant] [b :variance :covariant]])

(restrict-poly-class Associative [[a :variance :invariant] [b :variance :covariant]]
                :restrict {IPersistentCollection (Inst IPersistentCollection a)
                           Seqable (Inst Seqable Any)
                           ILookup (Inst ILookup a b)})

(restrict-poly-class IPersistentStack [[a :variance :covariant]]
                :restrict
                {IPersistentCollection (Inst IPersistentCollection a)
                 Seqable (Inst Seqable a)})

(restrict-poly-class IPersistentVector [[a :variance :covariant]]
                :restrict 
                {Seqable (Inst Seqable a)
                 IPersistentCollection (Inst IPersistentCollection a)
                 Associative (Inst Associative Long a) ;TODO Integer alias
                 IPersistentStack (Inst IPersistentStack a)
                 ILookup (Inst ILookup Long a)})

(restrict-poly-class APersistentVector [[a :variance :covariant]]
                :restrict
                {Seqable (Inst Seqable a)
                 IPersistentCollection (Inst IPersistentCollection a)
                 Associative (Inst Associative Long a)
                 IPersistentStack (Inst IPersistentStack a)
                 IFn (Fn [Long -> a])
                 AFn (Fn [Long -> a])})

(restrict-poly-class PersistentVector [[a :variance :covariant]]
                :restrict
                {APersistentVector (Inst APersistentVector a)
                 Seqable (Inst Seqable a)
                 IPersistentCollection (Inst IPersistentCollection a)
                 Associative (Inst Associative Long a)
                 IPersistentStack (Inst IPersistentStack a)
                 IFn (Fn [Long -> a])
                 AFn (Fn [Long -> a])})

(comment
  (+T a [(Seqable Integer) -> Integer])
  (defn a [s]
    (if (vector? s)
      (-> s   ; s = (I (Seqable Integer) (IPersistentVector Any))
        first)
      (first s)))
  )


(+T clojure.core/seq
    (All [[x :variance :invariant]]
         (Fn [(Inst Seqable Nothing) -> Nil]
             [(Inst Seqable x) -> (U (Inst ASeq x) Nil)]
             [String -> (U (Inst ASeq Character) Nil)]
             [(U Map Iterable) -> (U (Inst ASeq Any) Nil)]
             [Nil -> Nil])))

(+T clojure.core/first
    (All [[x :variance :invariant]]
         (Fn [(Inst Seqable Nothing) -> Nil]
             [(Inst Seqable x) -> (U x Nil)]
             [String -> (U Character Nil)]
             [Map -> (U Any Nil)]
             [Iterable -> (U Any Nil)]
             [Nil -> Nil])))

(+T clojure.core/rest
    (All [[x :variance :invariant]]
      (Fn [(Inst Seqable Nothing) -> (Inst ISeq Nothing)]
          [(Inst Seqable x) -> (U (Inst ISeq Nothing)
                             (Inst ASeq x))]
          [Nil -> (Inst ISeq Nothing)]
          [String -> (U (Inst ISeq Nothing)
                        (Inst ASeq Character))]
          [(U Iterable Map) -> (U (Inst ISeq Nothing)
                                  (Inst ASeq Any))])))

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
