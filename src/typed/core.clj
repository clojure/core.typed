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

(defmacro alter-poly-class [the-class tvars & opts]
  (let [opts-m (apply hash-map opts)
        tvar-bndings (map first tvars)
        tvar-objs (mapv tv-from-syntax tvars)]
    `(do
       (add-restricted-class-constructor
         ~the-class
         (->PolyRestrictedClassConstructor 
           ~the-class 
           ~tvar-objs
           (let [~@(mapcat vector tvar-bndings tvar-objs)]
             ~(:replace opts-m))))
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

(declare subtype?)

(deftype RestrictedClass [class replacements])
(deftype PolyRestrictedClassInstance [class constructor replacements])
(deftype PolyRestrictedClassConstructor [class tvars replacements]
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
                                             (do (assert (subtype? typ (.upper-bound tv)))
                                               typ)
                                             this)))])
                 replacements)))))

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
                                          (do (assert (subtype? typ (.upper-bound tv)))
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
(deftype TV [nme upper-bound variance]
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

(def ^:private upper-bound-syn :<)

(defn tv-from-syntax [[nme & opt-list]]
  (let [opts (apply hash-map opt-list)
        variance (:variance opts)
        upper-bound (if (contains? opts upper-bound-syn)
                (upper-bound-syn opts)
                `Any)]
    `(->TV '~(gensym nme) ~upper-bound ~variance)))

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
                 (.replacements this)))))

  PolyRestrictedClassInstance
  (-visit-tvar [this f]
    (->PolyRestrictedClassInstance
      (.class this)
      (-visit-tvar (.constructor this) f)
      (into {}
            (map (fn [[cl type]]
                   [cl (-visit-tvar type f)])
                 (.replacements this)))))

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
      (.upper-bound this)
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
          [new-tvars ds] (let [res (map #(matching-rel (.upper-bound %1) (.upper-bound %2) xs)
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
                        (.upper-bound ntvar)
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
    (gen-constraint (.upper-bound sub) type xs)

    (and (poly? sub)
         (poly? type)
         (= (map #(.nme %) (-tvars sub))
            (map #(.nme %) (-tvars type))))
    (let [res (map #(matching-rel (.upper-bound %1) (.upper-bound %2) xs)
                   (-tvars sub)
                   (-tvars type))
          ks (map first res)
          ds (map second res)

          rplc-fn (fn [old]
                    (if-let [t (some #(.nme old) (-tvars sub))]
                      (->TV
                        (.nme old)
                        (.upper-bound t)
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

(alter-poly-class Seqable [[a :< (Inst ISeq _) :variance :covariant]])

(comment
  ;Is this sound? ArraySeqs are immutable, but argument Arrays are mutable
(Seqable (Inst ArraySeq Integer)) ;<:
(Seqable (Inst ArraySeq Object))
  )

;cons, count, empty, equiv
(alter-poly-class IPersistentCollection [[a :variance :covariant]  ;object to cons
                                         [b :< (Inst IPersistentCollection _ _ _ _ _)
                                            :variance :covariant]  ;cons result
                                         [c :< (Inst IPersistentCollection _ _ _ _ _ )
                                            :variance :covariant]  ;empty
                                         [d :variance :covariant]  ;first
                                         [e :< (Inst IPersistentCollection _ _ _ _ _ )
                                            :variance :covariant] ;rest output
                                         [f :variance :covariant] ;next output, minus nil
                                         ])

(alter-poly-class Cons [[a :variance :covariant]
                        [b :< (U Nil (Inst ISeq a)) :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst Cons a b))
                   IPersistentColection (Inst IPersistentColection 
                                              a                             ;object to cons
                                              (Inst Cons a (Inst Cons a b)) ;cons result
                                              PersistentList$EmptyList      ;empty
                                              a                             ;first
                                              b)                            ;rest
                   ASeq (Inst ASeq a)
                   ISeq (Inst ISeq a)})

(comment
  (empty (Cons a b)) :- PersistentList$EmptyList
  (seq (Cons a b)) :- (Cons a b)
  (first (Cons a Any)) :- a
  (rest (Cons Any nil)) :- PersistentList$EmptyList
  (rest (Cons Any (Seqable a b))) :- b
  (next (Cons Any Nil)) :- Nil
  (next (Cons Any a)) :- a
  )

(alter-poly-class PersistentList [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst PersistentList a))
                   IPersistentColection (Inst IPersistentColection 
                                              a                          ;cons obj
                                              (Inst PersistentList a)    ;cons result
                                              PersistentList$EmptyList   ;empty
                                              a                          ;first
                                              (U (Inst PersistentList a)
                                                 (Inst PersistentList$EmptyList)) ;rest
                                              (Inst PersistentList a)    ;next
                                              )
                   IPersistentList (Inst IPersistentList a)})

(alter-class PersistentList$EmptyList [[a :variance :covariant]]
             :replace
             {Seqable (Inst Seqable Nothing)  ;hmm, is this a good way to represent "unseqable" things that are Seqable? - Ambrose
              IPersistentCollection (Inst IPersistentCollection
                                          a                     ;cons obj
                                          (Inst Cons a PersistentList$EmptyList) ;cons result
                                          PersistentList$EmptyList  ;empty
                                          Nothing                   ;first
                                          IPersistentCollection$EmptyList ;rest
                                          Nothing) ;next
              })


(alter-poly-class ISeq [[a :variance :covariant]]
                  :replace {Seqable (Inst Seqable (Inst ISeq a))
                            IPersistentCollection (Inst IPersistentCollection Any _ _ Any _ _)})

(alter-poly-class ASeq [[a :variance :covariant]]
                  :replace {Seqable (Inst Seqable (Inst ASeq a))
                            IPersistentCollection (Inst IPersistentCollection 
                                                        a                ;obj to cons
                                                        (Inst ASeq a)    ;cons result
                                                        PersistentList$EmptyList  ;empty
                                                        a                ;first
                                                        (Inst ASeq a)    ;rest
                                                        (Inst ISeq a))   ;next
                            ISeq (Inst ISeq a)})

(alter-poly-class ILookup [[a :variance :invariant]  ;key
                           [b :variance :covariant]]);value

;(assoc (Associative a b) a b) => c
(alter-poly-class Associative [[a :variance :invariant]  ;key
                               [b :variance :invariant]  ;value
                               [c :variance :invariant]] ;result of assoc
                  :replace 
                  {Seqable (Inst Seqable (Inst ASeq Any))
                   IPersistentCollection (Inst IPersistentCollection _ _ _ _ _) ;is this too specific?
                   ILookup (Inst ILookup a b)})

(alter-poly-class IPersistentMap [[a :variance :invariant] ;key
                                  [b :variance :invariant] ;value
                                  [c :variance :invariant] ;dissoc key
                                  [d :variance :invariant]];dissoc result
                  :replace 
                  {Seqable (Inst Seqable (Inst ASeq (Inst IMapEntry a b)))
                   IPersistentCollection (Inst IPersistentCollection
                                               (U Nil (Inst IMapEntry a b)) ;object to cons
                                               (Inst IPersistentMap (Inst IMapEntry a b)) ;cons result
                                               _ ;empty
                                               (Inst IMapEntry a b) ;first
                                               _ ;rest
                                               _ ;next
                                               )})

(alter-poly-class APersistentMap [[a :variance :invariant] ;key
                                  [b :variance :invariant] ;value
                                  ]
                  :replace
                  {Seqable (Inst Seqable (Inst ASeq Any))
                   })

(alter-poly-class PersistentHashMap [[a :variance :invariant] ;key
                                     [b :variance :invariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst ASeq Any))
                   APersistentMap (Inst APersistentMap 
                                        a      ;key
                                        b      ;val
                                        Any    ;dissoc value TODO: What to put here? - Ambrose
                                        (Inst IPersistentMap a b))  ;dissoc result
                   })

(alter-poly-class IPersistentStack [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst IPersistentStack a) (Inst ISeq a))
                   IPersistentCollection (Inst IPersistentCollection _ _ _ _ _ _) ;too specific?
                   })

(alter-poly-class IPersistentVector [[a :variance :covariant]]
                  :replace 
                  {Seqable (Inst Seqable (Inst ASeq a))
                   IPersistentCollection (Inst IPersistentCollection _ _ _ _ _) ;too specific?
                   Associative (Inst Associative Long a) ;TODO Integer alias
                   IPersistentStack (Inst IPersistentStack a)
                   ILookup (Inst ILookup Long a)})

(alter-poly-class APersistentVector [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst APersistentVector$Seq a))
                   IPersistentCollection (Inst IPersistentCollection _ _ _ _ _ _)
                   Associative (Inst Associative Long a)
                   IPersistentStack (Inst IPersistentStack a)
                   IFn (Fn [Long -> a])
                   AFn (Fn [Long -> a])})

(alter-poly-class PersistentVector [[a :variance :covariant]]
                  :replace
                  {Seqable (Inst Seqable (Inst PersistentVector$ChunkedSeq a))
                   APersistentVector (Inst APersistentVector a)
                   IPersistentCollection (Inst IPersistentCollection 
                                               a             ;type to cons
                                               (Inst PersistentVector a) ;cons result
                                               (Inst PersistentVector Nothing) ;empty
                                               a             ;first
                                               (Inst PersistentVector a) ;rest
                                               (Inst PersistentVector a) ;next
                                               )
                   Associative (Inst Associative Long a) ;TODO clojure integer type
                   IPersistentStack (Inst IPersistentStack a)
                   IFn (Fn [Long -> a])
                   AFn (Fn [Long -> a])})

;; Some sigs

(+T clojure.core/seq
    (All [[a :variance :contravariant]
          [x :variance :invariant]]
      (Fn [(Inst Seqable x) -> (U x Nil)]
          [CharSequence -> (U StringSeq Nil)]
          ;array -> (U ArraySeq Nil)
          [Nil -> Nil]
          [(U Map Iterable) -> (U IteratorSeq Nil)])))

(+T clojure.core/conj
    (All [[x :variance :invariant]   ;object to cons
          [c :variance :invariant]]  ;cons result
      (Fn [(Inst IPersistentCollection x c _ _ _) x & x * -> c]
          [Nil & x * -> (Inst PersistentList x)])))

(+T clojure.core/first
    (All [[f :variance :invariant]]
      (Fn [(Inst IPersistentCollection _ _ _ f _ _) -> (U f Nil)]
          [(Inst Seqable (Inst IPersistentCollection _ _ _ f _ _)) -> (U f Nil)]
          [CharSequence -> (U StringSeq Nil)]
          ;array -> (U Any Nil)
          [Iterable -> (U IteratorSeq Nil)]
          [Nil -> Nil])))

(+T clojure.core/rest
    (All [[r :variance :invariant]]  ;rest
      (Fn [(Inst IPersistentCollection _ _ _ _ r _) -> r]
          [(Inst Seqable (Inst IPersistentCollection _ _ _ _ r _)) -> r]
          [CharSequence -> (U StringSeq PersistentList$EmptyList)]
          [Nil -> PersistentList$EmptyList]
          ;array -> (U ArraySeq PersistentList$EmptyList)
          [(U Map Iterable) -> (U IteratorSeq PersistentList$EmptyList)])))

(+T clojure.core/next
    (All [[n :variance :invariant]]  ;rest
      (Fn [(Inst IPersistentCollection _ _ _ _ _ n) -> (U Nil n)]
          [(Inst Seqable (Inst IPersistentCollection _ _ _ _ _ n)) -> (U Nil n)]
          [CharSequence -> (U StringSeq Nil)]
          [Nil -> Nil]
          ;array -> (U ArraySeq Nil)
          [(U Map Iterable) -> (U IteratorSeq Nil)])))

(+T clojure.core/assoc
    (All [[a :variance :invariant] ;invariant
          [b :variance :invariant]
          [c :variance :invariant]]
         (Fn [(Inst Associative a b) a b & [a b] * -> c] ;TODO "keyword" rest args
             [Nil a b & [a b] * -> (U (Inst PersistentArrayMap a b)
                                      (Inst PersistentHashMap a b))] ;sufficient return type? seems hacky - Ambrose
             )))

(+T clojure.core/dissoc
    (All [[a :variance :invariant]
          [b :variance :invariant]]
         (Fn [(Inst IPersistentMap _ _ a b) & a -> b]
             [Nil & Any -> Nil])))

; just playing with syntax 
(comment

  ;(+T-pprotocol ProtocolName tvars & methods)
  (+T-pprotocol ISeq [[a :variance :invariant]]
    (+T first (Fn [(Inst IPersistentCollection Nothing)
                   -> Nil]
                  [(Inst IPersistentCollection a)
                   -> (U (ASeq a) Nil)]
                  [String -> (U Nil (ASeq Character))]
                  [(U Iterable Map Seqable) -> (U Nil (ASeq Any))])))

  ;(+T-ptype TypeName tvars fields & impls)

  (+T-ptype VectorNode [[x :variance :invariant]] 
    [[arr (Inst Array x)]])

  (+T-ptype PersistentVector [[x :variance :covariant]] 
    [[cnt :- Long]
     [root :- (Inst VectorNode Any)]
     [tail :- (Inst ListNode x)]]
                )
  )


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
