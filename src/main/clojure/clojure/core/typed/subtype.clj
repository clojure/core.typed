(set! *warn-on-reflection* true)

(in-ns 'clojure.core.typed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

(def ^:dynamic *current-env* nil)
(def ^:dynamic *current-expr* nil)


;[Type Type -> Nothing]
(defn type-error [s t]
  (throw (Exception. (str "Type Error"
                          (when *current-env*
                            (str ", " (:source *current-env*) ":" (:line *current-env*)))
                          "\n\nActual type\n\t"
                          (or (-> s meta :source-Name)
                              (with-out-str (pr (unparse-type s))))
                          "\nis not a subtype of Expected type\n\t" 
                          (or (-> t meta :source-Name)
                              (with-out-str (pr (unparse-type t))))
                          (when *current-expr*
                            (str "\n\nForm: " (emit-form-fn *current-expr*)))))))

;keeps track of currently seen subtype relations for recursive types.
;(Set [Type Type])
(def ^:dynamic *sub-current-seen* #{})

(declare subtypes*-varargs)

;[(Seqable Type) (Seqable Type) Type -> Boolean]
(defn subtypes-varargs?
  "True if argtys are under dom"
  [argtys dom rst]
  (try 
    (subtypes*-varargs #{} argtys dom rst)
    true
    (catch Exception e
      false)))


;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument (the current-seen subtypes), called by subtype
;subtype* shouldn't be called directly, is called by subtypeA*
;
; In short, only call subtype (or subtype?)

;[Type Type -> (IPersistentSet '[Type Type])]
(defmulti subtype* (fn [s t] [(class s) (class t) @TYPED-IMPL]))

;[Type Type -> Boolean]
(defn subtype? [s t]
  (try 
    (subtype s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare subtypeA*)

;[(IPersistentSet '[Type Type]) Type Type -> Boolean]
(defn subtypeA*? [A s t]
  (try (subtypeA* A s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare supertype-of-one-arr)


;[(IPersistentMap Symbol Bounds) (Seqable Type) (Seqable Type)
;  -> Boolean]
(defn unify [X S T]
  (boolean (infer X {} S T -any)))

(declare keyword->Fn subtype-TApp?)

;[(IPersistentSet '[Type Type]) Type Type -> (IPersistentSet '[Type Type])]
(defn subtypeA* [A s t]
  {:post [(set? %)]}
  (if (or (contains? A [s t])
          (= s t)
          (Top? t)
          (Bottom? s))
    A
    (binding [*sub-current-seen* (conj A [s t])]
      (cond
        (and (Value? s)
             (Value? t))
        ;already (not= s t)
        (type-error s t)

        (Name? s)
        (subtypeA* *sub-current-seen* (resolve-Name s) t)

        (Name? t)
        (subtypeA* *sub-current-seen* s (resolve-Name t))

        (and (Poly? s)
             (Poly? t)
             (= (.nbound ^Poly s) (.nbound ^Poly t)))
        (let [names (repeatedly (.nbound ^Poly s) gensym)
              b1 (Poly-body* names s)
              b2 (Poly-body* names t)]
          (subtype b1 b2))

        ;use unification to see if we can use the Poly type here
        (and (Poly? s)
             (let [names (repeatedly (.nbound ^Poly s) gensym)
                   bnds (Poly-bbnds* names s)
                   b1 (Poly-body* names s)]
               (unify (zipmap names bnds) [b1] [t])))
        (let [names (repeatedly (.nbound ^Poly s) gensym)
              bnds (Poly-bbnds* names s)
              b1 (Poly-body* names s)]
          (if (unify (zipmap names bnds) [b1] [t])
            *sub-current-seen*
            (type-error s t)))

        (and (Poly? t)
             (let [names (repeatedly (.nbound ^Poly t) gensym)
                   b (Poly-body* names t)]
               (empty? (fv t))))
        (let [names (repeatedly (.nbound ^Poly t) gensym)
              b (Poly-body* names t)]
          (subtype s b))

        (and (TApp? s)
             (TApp? t))
        (if (subtype-TApp? s t)
          *sub-current-seen*
          (type-error s t))

        (TApp? s)
        (let [^TApp s s]
          (if (and (not (F? (.rator s)))
                   (subtypeA*? (conj *sub-current-seen* [s t])
                               (resolve-TApp s) t))
            *sub-current-seen*
            (type-error s t)))

        (TApp? t)
        (let [^TApp t t]
          (if (and (not (F? (.rator t)))
                   (subtypeA*? (conj *sub-current-seen* [s t])
                               s (resolve-TApp t)))
            *sub-current-seen*
            (type-error s t)))

        (App? s)
        (subtypeA* *sub-current-seen* (resolve-App s) t)

        (App? t)
        (subtypeA* *sub-current-seen* s (resolve-App t))

        (Bottom? t)
        (type-error s t)

        (Union? s)
        ;use subtypeA*, throws error
        (if (every? #(subtypeA* *sub-current-seen* % t) (.types ^Union s))
          *sub-current-seen*
          (type-error s t))

        ;use subtypeA*?, boolean result
        (Union? t)
        (if (some #(subtypeA*? *sub-current-seen* s %) (.types ^Union t))
          *sub-current-seen*
          (type-error s t))

        (and (FnIntersection? s)
             (FnIntersection? t))
        (loop [A* *sub-current-seen*
               arr2 (:types t)]
          (let [arr1 (:types s)]
            (if (empty? arr2) 
              A*
              (if-let [A (supertype-of-one-arr A* (first arr2) arr1)]
                (recur A (next arr2))
                (type-error s t)))))

        (and (Intersection? s)
             (Intersection? t))
        (if (every? (fn [s*]
                      (some #(subtype? s* %) (.types ^Intersection t)))
                    (.types ^Intersection s))
          *sub-current-seen*
          (type-error s t))

        (Intersection? s)
        (if (some #(subtype? % t) (.types ^Intersection s))
          *sub-current-seen*
          (type-error s t))

        (Intersection? t)
        (if (every? #(subtype? s %) (.types ^Intersection t))
          *sub-current-seen*
          (type-error s t))

        ;values are subtypes of their classes
        ;FIXME this is probably wrong because we don't account for boxing/unboxing
        ; Leaving this here to please the tests.
        (and (Value? s)
             (checking-clojure?))
        (let [^Value s s]
          (if (nil? (.val s))
            (type-error s t)
            (subtype (apply In (RClass-of (class (.val s)))
                            ;keyword values are functions
                            (when (keyword? (.val s))
                              [(keyword->Fn (.val s))]))
                     t)))

        :else (subtype* s t)))))

;[Type Type -> (IPersistentSet '[Type Type])]
(defn subtype [s t]
  {:post [(set? %)]}
  (subtypeA* *sub-current-seen* s t))

;[(IPersistentSet '[Type Type]) (Seqable Type) (Seqable Type) (Option Type)
;  -> (IPersistentSet '[Type Type])]
(defn subtypes*-varargs [A0 argtys dom rst]
  (loop [dom dom
         argtys argtys
         A A0]
    (cond
      (and (empty? dom) (empty? argtys)) A
      (empty? argtys) (throw (Exception. (prn-str "Expected arguments: " (map unparse-type dom)
                                                  " Actual: "(map unparse-type argtys))))
      (and (empty? dom) rst)
      (if-let [A (subtypeA* A (first argtys) rst)]
        (recur dom (next argtys) A)
        (type-error (first argtys) rst))

      (empty? dom) (throw (Exception. (prn-str "Expected arguments: " (map unparse-type dom)
                                               " Actual: "(map unparse-type argtys))))
      :else
      (if-let [A (subtypeA* A0 (first argtys) (first dom))]
        (recur (next dom) (next argtys) A)
        (type-error (first argtys) (first dom))))))

;; simple co/contra-variance for ->
;[(IPersistentSet '[Type Type]) Function Function -> (IPersistentSet '[Type Type])]
(defn arr-subtype [A0 ^Function s ^Function t]
  {:pre [(Function? s)
         (Function? t)]}
  (assert (not (some :kws [s t])))
  ;; top for functions is above everything
  (cond
    ;; top for functions is above everything
    (TopFunction? t) A0
    ;; the really simple case
    (and (not ((some-fn :rest :drest :kws) s))
         (not ((some-fn :rest :drest :kws) t)))
    (do
      (when-not (= (count (.dom s))
                   (count (.dom t)))
        (type-error s t))
      (-> *sub-current-seen*
        ((fn [A0]
           (reduce (fn [A* [s t]]
                     (subtypeA* A* s t))
                   A0
                   (map vector (.dom t) (.dom s)))))
        (subtypeA* (.rng s) (.rng t))))

    (and (:rest s)
         (not ((some-fn :rest :drest) t)))
    (-> *sub-current-seen*
      (subtypes*-varargs (.dom t) (.dom s) (.rest s))
      (subtypeA* (.rng s) (.rng t)))

    (and (not ((some-fn :rest :drest) s))
         (:rest t))
    (type-error s t)

    (and (.rest s)
         (.rest t))
    (-> *sub-current-seen*
      (subtypes*-varargs (:dom t) (:dom s) (:rest s))
      (subtypeA* (:rest t) (:rest s))
      (subtypeA* (:rng s) (:rng t)))

    ;; handle ... varargs when the bounds are the same
    (and (:drest s)
         (:drest t)
         (= (-> s :drest :name)
            (-> t :drest :name)))
    (-> *sub-current-seen*
      (subtypeA* (-> t :drest :pre-type) (-> s :drest :pre-type))
      ((fn [A0] 
         (reduce (fn [A* [s t]]
                   (subtypeA* A* s t))
                 A0 (map vector (:dom t) (:dom s)))))
      (subtypeA* (:rng s) (:rng t)))
    :else (type-error s t)))

;[(IPersistentSet '[Type Type]) Function (Seqable Function) -> (IPersistentSet '[Type Type])]
(defn supertype-of-one-arr [A s ts]
  (some #(try (arr-subtype A % s)
           (catch IllegalArgumentException e
             (throw e))
           (catch Exception e))
        ts))

(defmethod subtype* [Result Result ::default]
  [{t1 :t f1 :fl o1 :o :as s}
   {t2 :t f2 :fl o2 :o :as t}]
  (cond
    ;trivial case
    (and (= f1 f2)
         (= o1 o2))
    (subtype t1 t2)

    ;we can ignore some interesting results
    (and (EmptyObject? o2)
         (= f2 (-FS -top -top)))
    (subtype t1 t2)

    ;special case for (& (is y sym) ...) <: (is y sym)
    (and (AndFilter? (:then f1))
         (TypeFilter? (:then f2))
         (every? atomic-filter? (:fs (:then f1)))
         (= 1 (count (filter TypeFilter? (:fs (:then f1)))))
         (= -top (:else f2)))
    (let [f1-tf (first (filter TypeFilter? (:fs (:then f1))))]
      (if (= f1-tf (:then f2))
        (subtype t1 t2)
        (throw (Exception. (error-msg "Filters do not match: \n" (unparse-filter-set f1) "\n" (unparse-filter-set f2))))))

    :else (if (= o1 o2)
            (throw (Exception. (error-msg "Filters do not match: \n" (unparse-filter-set f1) "\n" (unparse-filter-set f2))))
            (throw (Exception. (error-msg "Objects do not match " (unparse-object o1) (unparse-object o2)))))))

(defmethod subtype* [Protocol Type ::clojure]
  [s t]
  (if (= (RClass-of Object) t)
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [Protocol Protocol ::default]
  [{var1 :the-var variances* :variances poly1 :poly? :as s}
   {var2 :the-var poly2 :poly? :as t}]
  (if (and (= var1 var2)
           (every? (fn [[v l r]]
                     (case v
                       :covariant (subtypeA* *sub-current-seen* l r)
                       :contravariant (subtypeA* *sub-current-seen* r l)
                       :invariant (and (subtypeA* *sub-current-seen* l r)
                                       (subtypeA* *sub-current-seen* r l))))
                   (map vector variances* poly1 poly2)))
    *sub-current-seen*
    (type-error s t)))

(defn subtype-TypeFn-app?
  [^TypeFn tfn ^TApp ltapp ^TApp rtapp]
  {:pre [(TypeFn? tfn)
         (TApp? ltapp)
         (TApp? rtapp)]}
  (every? (fn [[v l r]]
            (case v
              :covariant (subtypeA*? *sub-current-seen* l r)
              :contravariant (subtypeA*? *sub-current-seen* r l)
              :invariant (and (subtypeA*? *sub-current-seen* l r)
                              (subtypeA*? *sub-current-seen* r l))))
          (map vector (.variances tfn) (.rands ltapp) (.rands rtapp))))

(defmulti subtype-TApp? (fn [^TApp S ^TApp T] 
                          {:pre [(TApp? S)
                                 (TApp? T)]}
                          [(class (.rator S)) (class (.rator T))
                           (= (.rator S) (.rator T))]))

(defmethod subtype-TApp? [TypeFn TypeFn false]
  [S T]
  (subtypeA*? (conj *sub-current-seen* [S T]) (resolve-TApp S) (resolve-TApp T)))

(defmethod subtype-TApp? [TypeFn TypeFn true]
  [^TApp S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TypeFn-app? (.rator S) S T)))

(defmethod subtype-TApp? [AnyType Name false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? S (update-in T [:rator] resolve-Name))))

(defmethod subtype-TApp? [Name AnyType false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? (update-in S [:rator] resolve-Name) T)))

; for [Name Name false]
(prefer-method subtype-TApp? 
               [Name AnyType false]
               [AnyType Name false])

;same operator
(defmethod subtype-TApp? [Name Name true]
  [^TApp S T]
  (let [r (resolve-Name (.rator S))]
    (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
      (subtype-TApp? (assoc-in S [:rator] r)
                     (assoc-in T [:rator] r)))))

; only subtypes if applied to the same F
(defmethod subtype-TApp? [F F false] [S T] false)
(defmethod subtype-TApp? [F F true]
  [^TApp S T]
  (let [tfn (some (fn [[_ {{:keys [name]} :F :keys [^Bounds bnds]}]] 
                    (when (= name (.name ^F (.rator S)))
                      (.higher-kind bnds)))
                  *free-scope*)]
    (when tfn
      (subtype-TypeFn-app? tfn S T))))

(defmethod subtype-TApp? :default [S T] false)

(prefer-method subtype* [Type TApp ::default] [HeterogeneousVector Type ::default])
(prefer-method subtype* [Type TApp ::default] [HeterogeneousVector Type ::default])

(defmethod subtype* [TypeFn TypeFn ::default]
  [^TypeFn S ^TypeFn T]
  (if (and (= (.nbound S) (.nbound T))
           (= (.variances S) (.variances T))
           (= (.bbnds S) (.bbnds T))
           (let [names (repeatedly (.nbound S) gensym)
                 sbody (TypeFn-body* names S)
                 tbody (TypeFn-body* names T)]
             (subtype? sbody tbody)))
    *sub-current-seen*
    (type-error S T)))

(defmethod subtype* [PrimitiveArray Type ::clojure]
  [_ t]
  (subtype (->PrimitiveArray Object -any -any) t))

(defmethod subtype* [PrimitiveArray PrimitiveArray ::clojure]
  [^PrimitiveArray s 
   ^PrimitiveArray t]
  (if (and ;(= (.jtype s) (.jtype t))
           ;contravariant
           (subtype? (.input-type t)
                     (.input-type s))
           ;covariant
           (subtype? (.output-type s)
                     (.output-type t)))
    *sub-current-seen*
    (type-error s t)))

;Not quite correct, datatypes have other implicit ancestors (?)
(defmethod subtype* [DataType Type ::clojure]
  [{:keys [the-class] :as s} t]
  (if (some #(subtype? % t) (set/union #{(RClass-of Object)} 
                                       (or (@DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [Type DataType ::clojure]
  [s {:keys [the-class] :as t}]
  (if (some #(subtype? s %) (set/union #{(RClass-of Object)} 
                                       (or (@DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [DataType DataType ::default]
  [{cls1 :the-class poly1 :poly? :as s} 
   {cls2 :the-class poly2 :poly? :as t}]
  (if (and (= cls1 cls2)
           (every? (fn [[v l r]]
                     (case v
                       :covariant (subtypeA* *sub-current-seen* l r)
                       :contravariant (subtypeA* *sub-current-seen* r l)
                       :invariant (and (subtypeA* *sub-current-seen* l r)
                                       (subtypeA* *sub-current-seen* r l))))
                   (map vector (:variances s) poly1 poly2)))
    *sub-current-seen*
    (type-error s t)))

(defn- subtype-rclass
  [{variancesl :variances classl :the-class replacementsl :replacements :as s}
   {variancesr :variances classr :the-class replacementsr :replacements :as t}]
  (cond
    ;easy case
    (and (empty? variancesl)
         (empty? variancesr)
         (empty? replacementsl)
         (empty? replacementsr))
    (if (isa? classl classr)
      *sub-current-seen*
      (type-error s t))))

; (Cons Integer) <: (Seqable Integer)
; (ancestors (Seqable Integer)

(defn- subtype-RClass-common-base 
  [{polyl? :poly? lcls-sym :the-class :as s}
   {polyr? :poly? rcls-sym :the-class :as t}]
  (let [{variances :variances} s]
    (and (= lcls-sym rcls-sym)
         (or (and (empty? polyl?) (empty? polyr?))
             (and (seq polyl?)
                  (seq polyr?)
                  (every? identity
                          (doall (map #(case %1
                                         :covariant (subtype? %2 %3)
                                         :contravariant (subtype? %3 %2)
                                         (= %2 %3))
                                      variances
                                      polyl?
                                      polyr?))))))))

;(IPersistentMap Class Class)
(def boxed-primitives
  {Byte/TYPE Byte
   Short/TYPE Short
   Integer/TYPE Integer
   Long/TYPE Long
   Float/TYPE Float
   Double/TYPE Double
   Character/TYPE Character
   Boolean/TYPE Boolean})

;[RClass RClass -> Boolean]
(defn coerse-RClass-primitive
  [s t]
  (let [spcls (symbol->Class (:the-class s))
        tpcls (symbol->Class (:the-class t))
        scls (or (boxed-primitives spcls)
                 spcls)
        tcls (or (boxed-primitives tpcls)
                  tpcls)]
    (isa? scls tcls)))

(defmethod subtype* [RClass RClass ::clojure]
  [{polyl? :poly? :as s}
   {polyr? :poly? :as t}]
  (let [scls (RClass->Class s)
        tcls (RClass->Class t)]
    (cond
      (or
        ; use java subclassing
        (and (empty? polyl?)
             (empty? polyr?)
             (empty? (:replacements s))
             (empty? (:replacements t))
             (isa? scls tcls))

        ;same base class
        (and (= scls tcls)
             (subtype-RClass-common-base s t))

        ;one is a primitive, coerse
        (and (or (.isPrimitive scls)
                 (.isPrimitive tcls))
             (coerse-RClass-primitive s t))

        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(and (= (:the-class t) (:the-class %))
                    (subtype-RClass-common-base % t))
              (RClass-supers* s)))
      *sub-current-seen*

      ;try each ancestor

      :else (type-error s t))))

(prefer-method subtype* 
               [Type Mu ::default]
               [HeterogeneousMap Type ::clojure])

(defmethod subtype* [HeterogeneousMap Type ::clojure]
  [^HeterogeneousMap s t]
  ; HMaps do not record absence of fields, only subtype to (APersistentMap Any Any)
  (if (complete-hmap? s)
    (subtype (RClass-of APersistentMap [(apply Un (keys (.types s)))
                                        (apply Un (vals (.types s)))]) 
             t)
    (subtype (RClass-of APersistentMap [-any -any]) t)))

;every rtype entry must be in ltypes
;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
(defmethod subtype* [HeterogeneousMap HeterogeneousMap ::default]
  [{ltypes :types :as s}
   {rtypes :types :as t}]
  (or (last (doall (map (fn [[k v]]
                          (if-let [t (ltypes k)]
                            (subtype t v)
                            (type-error s t)))
                        rtypes)))
      #{}))

(prefer-method subtype* 
               [HeterogeneousVector HeterogeneousVector ::default]
               [HeterogeneousVector Type ::clojure])
(prefer-method subtype* 
               [HeterogeneousMap HeterogeneousMap ::default],
               [HeterogeneousMap Type ::clojure] )

(defmethod subtype* [HeterogeneousVector HeterogeneousVector ::default]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (or (last (doall (map #(subtype %1 %2) ltypes rtypes)))
      #{}))

(defmethod subtype* [HeterogeneousVector Type ::clojure]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (In (RClass-of APersistentVector [ss])
                 (make-ExactCountRange (count (:types s))))
             t)))

(defmethod subtype* [HeterogeneousList HeterogeneousList ::default]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (or (last (doall (map #(subtype %1 %2) ltypes rtypes)))
      #{}))

(defmethod subtype* [HeterogeneousList Type ::clojure]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (RClass-of (Class->symbol PersistentList) [ss])
             t)))

(defmethod subtype* [HeterogeneousSeq HeterogeneousSeq ::default]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (or (last (doall (map #(subtype %1 %2) ltypes rtypes)))
      #{}))

(defmethod subtype* [HeterogeneousSeq Type ::clojure]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (RClass-of (Class->symbol ASeq) [ss])
             t)))

(defmethod subtype* [Mu Type ::default]
  [s t]
  (let [s* (unfold s)]
    (subtype s* t)))

(defmethod subtype* [Type Mu ::default]
  [s t]
  (let [t* (unfold t)]
    (subtype s t*)))

;subtype if t includes all of s. 
;tl <= sl, su <= tu
(defmethod subtype* [CountRange CountRange ::default]
  [{supper :upper slower :lower :as s}
   {tupper :upper tlower :lower :as t}]
  (if (and (<= tlower slower)
           (if tupper
             (and supper (<= supper tupper))
             true))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* :default
  [s t]
  #_(prn "subtype :default" @TYPED-IMPL (unparse-type s) (unparse-type t))
  (if (Top? t)
    *sub-current-seen*
    (type-error s t)))

(defmacro sub [s t]
  `(subtype (parse-type '~s)
            (parse-type '~t)))

