(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

(def ^:dynamic *current-env* nil)

(defn error-msg [& msg]
  (apply str (when *current-env*
                  (str (:line *current-env*) ": "))
         msg))

(defn type-error [s t]
  (throw (Exception. (str "Type Error"
                          (when *current-env*
                            (str ", " (:source *current-env*) ":" (:line *current-env*)))
                          " - "
                          (or (-> s meta :source-Name)
                              (with-out-str (pr (unparse-type s))))
                          " is not a subtype of: " 
                          (or (-> t meta :source-Name)
                              (with-out-str (pr (unparse-type t))))))))

;keeps track of currently seen subtype relations for recursive types.
;(Set [Type Type])
(def ^:dynamic *sub-current-seen* #{})

(declare subtypes*-varargs)

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

(defmulti subtype* (fn [s t] [(class s) (class t) *typed-impl*]))

(defn subtype? [s t]
  (try 
    (subtype s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare subtypeA*)

(defn subtypeA*? [A s t]
  (try (subtypeA* A s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare supertype-of-one-arr)

(defn unify [X S T]
  (infer X {} S T -any))

(defn subtypeA* [A s t]
  (if (or (contains? A [s t])
          (= s t)
          (Top? t)
          (Bottom? s))
    A
    (binding [*sub-current-seen* (conj A [s t])]
      (cond
        (Name? s)
        (subtypeA* *sub-current-seen* (resolve-Name s) t)

        (Name? t)
        (subtypeA* *sub-current-seen* s (resolve-Name t))

        (and (Poly? s)
             (Poly? t))
        (do
          (when-not (= (.nbound s) (.nbound t))
            (type-error s t))
          (let [names (repeatedly (.nbound s) gensym)
                b1 (Poly-body* names s)
                b2 (Poly-body* names t)]
            (subtype b1 b2)))

        (Poly? s)
        (let [names (repeatedly (.nbound s) gensym)
              bnds (Poly-bbnds* names s)
              b1 (Poly-body* names s)]
          (if (unify (zipmap names bnds) [b1] [t])
            *sub-current-seen*
            (type-error s t)))

        ;TODO Poly? t

        (and (TApp? s)
             (not (F? (.rator s)))
             (not (TApp? t)))
        (subtypeA* *sub-current-seen* (resolve-TApp s) t)

        (and (TApp? t)
             (not (F? (.rator t)))
             (not (TApp? s)))
        (subtypeA* *sub-current-seen* s (resolve-TApp t))

        (App? s)
        (subtypeA* *sub-current-seen* (resolve-App s) t)

        (App? t)
        (subtypeA* *sub-current-seen* s (resolve-App t))

        (Union? s)
        (if (every? #(subtypeA* *sub-current-seen* % t) (:types s))
          *sub-current-seen*
          (type-error s t))

        (Union? t)
        (if (some #(subtypeA*? *sub-current-seen* s %) (.types t))
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
                      (some #(subtype? s* %) (.types t)))
                    (.types s))
          *sub-current-seen*
          (type-error s t))

        (Intersection? s)
        (if (some #(subtype? % t) (.types s))
          *sub-current-seen*
          (type-error s t))

        (Intersection? t)
        (if (every? #(subtype? s %) (.types t))
          *sub-current-seen*
          (type-error s t))

        ;values are subtypes of their classes
        (Value? s)
        (if (nil? (.val s))
          (type-error s t)
          (subtype (RClass-of (class (.val s))) t))

        :else (subtype* s t)))))

(defn subtype [s t]
  (subtypeA* *sub-current-seen* s t))

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
(defn arr-subtype [A0 s t]
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
            (throw (Exception. (error-msg "Objects do not match " (unparse-object o1) (unparse-filter o2)))))))

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
  [tfn ltapp rtapp]
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

(defmulti subtype-TApp? (fn [S T] 
                          {:pre [(TApp? S)
                                 (TApp? T)]}
                          [(class (.rator S)) (class (.rator T))
                           (= (.rator S) (.rator T))]))

(defmethod subtype-TApp? [TypeFn TypeFn false]
  [S T]
  (subtypeA*? (conj *sub-current-seen* [S T]) (resolve-TApp S) (resolve-TApp T)))

(defmethod subtype-TApp? [TypeFn TypeFn true]
  [S T]
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
  [S T]
  (let [r (resolve-Name (.rator S))]
    (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
      (subtype-TApp? (assoc-in S [:rator] r)
                     (assoc-in T [:rator] r)))))

; only subtypes if applied to the same F
(defmethod subtype-TApp? [F F false] [S T] false)
(defmethod subtype-TApp? [F F true]
  [S T]
  (let [tfn (some (fn [[_ {{:keys [name]} :F :keys [bnds]}]] 
                    (when (= name (.name (.rator S)))
                      (.higher-kind bnds)))
                  *free-scope*)]
    (subtype-TypeFn-app? tfn S T)))

(defmethod subtype-TApp? :default [S T] false)

(defmethod subtype* [TApp TApp ::default]
  [S T]
  (if (subtype-TApp? S T)
    *sub-current-seen*
    (type-error S T)))

(prefer-method subtype* [Type TApp ::default] [HeterogeneousVector Type ::default])
(prefer-method subtype* [Type TApp ::default] [HeterogeneousVector Type ::default])

(defmethod subtype* [TApp Type ::default]
  [S T]
  (if (and (not (F? (.rator S)))
           (subtypeA*? (conj *sub-current-seen* [S T])
                       (resolve-TApp S) T))
    *sub-current-seen*
    (type-error S T)))

(defmethod subtype* [Type TApp ::default]
  [S T]
  (if (and (not (F? (.rator T)))
           (subtypeA*? (conj *sub-current-seen* [S T])
                       S (resolve-TApp T)))
    *sub-current-seen*
    (type-error S T)))

(defmethod subtype* [TypeFn TypeFn ::default]
  [S T]
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

;Not quite correct, datatypes have other implicit ancestors (?)
(defmethod subtype* [DataType Type ::clojure]
  [{:keys [the-class] :as s} t]
  (if (some #(subtype? % t) (set/union #{(RClass-of (Class->symbol Object) nil)} 
                                       (or (@DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (type-error s t)))

(defmethod subtype* [Type DataType ::clojure]
  [s {:keys [the-class] :as t}]
  (if (some #(subtype? s %) (set/union #{(RClass-of (Class->symbol Object) nil)} 
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

; Class -> {:up Class :down Class}
; up : is it safe to use this primitive type in place of up
; down : whereever down is, we can replace it with primitive
(def primitive-coersions
  {Byte/TYPE {:up #{Byte}
              :down #{Byte}}
   Short/TYPE {:up #{Short Integer Long Float Double}
               :down #{Short Integer Long}}
   Integer/TYPE {:up #{Short Integer Long Float Double}
                 :down #{Short Integer Long}}
   Long/TYPE {:up #{Short Integer Long Float Double Double/TYPE}
              :down #{Short Integer Long}}
   Float/TYPE {:up #{Float Double}
               :down #{Float Double}}
   Double/TYPE {:up #{Float Double}
                :down #{Float Double}}
   Character/TYPE {:up #{Character}
                   :down #{Character}}
   Boolean/TYPE {:up #{Boolean}
                 :down #{Boolean}}})

(defn coerse-RClass-primitive
  [s t]
  (let [scls (symbol->Class (:the-class s))
        tcls (symbol->Class (:the-class t))]
    (cond
      (.isPrimitive ^Class scls)
      (-> (primitive-coersions scls) :up (get tcls))

      (.isPrimitive ^Class tcls)
      (-> (primitive-coersions tcls) :down (get scls)))))

(defmethod subtype* [RClass RClass ::clojure]
  [{polyl? :poly? :as s}
   {polyr? :poly? :as t}]
  (let [scls (symbol->Class (:the-class s))
        tcls (symbol->Class (:the-class t))]
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
        (and (or (.isPrimitive ^Class scls)
                 (.isPrimitive ^Class tcls))
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
  [s t]
  (let [sk (apply Un (map first (:types s)))
        sv (apply Un (map second (:types s)))]
    (subtype (RClass-of (Class->symbol APersistentMap) [sk sv])
             t)))

;every rtype entry must be in ltypes
;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
(defmethod subtype* [HeterogeneousMap HeterogeneousMap ::default]
  [{ltypes :types :as s}
   {rtypes :types :as t}]
  (last (doall (map (fn [[k v]]
                      (if-let [t (ltypes k)]
                        (subtype t v)
                        (type-error s t)))
                    rtypes))))

(defmethod subtype* [HeterogeneousVector HeterogeneousVector ::default]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

(defmethod subtype* [HeterogeneousVector Type ::clojure]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (In (RClass-of APersistentVector [ss])
                 (make-ExactCountRange (count (:types s))))
             t)))

(defmethod subtype* [HeterogeneousList HeterogeneousList ::default]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

(defmethod subtype* [HeterogeneousList Type ::clojure]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (RClass-of (Class->symbol PersistentList) [ss])
             t)))

(defmethod subtype* [HeterogeneousSeq HeterogeneousSeq ::default]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

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
  (if (Top? t)
    *sub-current-seen*
    (type-error s t)))

(defmacro sub [s t]
  `(subtype (parse-type '~s)
            (parse-type '~t)))

