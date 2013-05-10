(ns clojure.core.typed.subtype
  (:require [clojure.core.typed
             [current-impl :as impl]
             [type-rep :as r]
             [type-ctors :as c]
             [utils :as u]
             [util-vars :as vs]
             [parse-unparse :as prs]
             [filter-rep :as fr]
             [filter-ops :as fops]
             [object-rep :as orep]
             [frees :as frees]
             [free-ops :as free-ops]
             [datatype-ancestor-env :as dtenv]]
            [clojure.set :as set])
  (:import (clojure.core.typed.type_rep Poly TApp Union Intersection Value Function
                                        Result Protocol TypeFn Name F Bounds HeterogeneousVector
                                        PrimitiveArray DataType RClass HeterogeneousMap
                                        HeterogeneousList HeterogeneousSeq CountRange KwArgs)
           (clojure.lang APersistentMap APersistentVector PersistentList ASeq Seqable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

(def subtype-error ::subtype-error)

(u/derive-error subtype-error)

(defn subtype-error? [exdata]
  (assert (not (instance? clojure.lang.ExceptionInfo exdata)))
  (isa? (:type-error exdata) subtype-error))

;[Type Type -> Nothing]
(defn fail! [s t]
  (throw (ex-info 
           "Subtyping failed"
           #_(str "Type Error"
                          (when vs/*current-env*
                            (str ", " (:source vs/*current-env*) ":" (:line vs/*current-env*)))
                          "\n\nActual type\n\t"
                          (or (-> s meta :source-Name)
                              (with-out-str (pr (prs/unparse-type s))))
                          "\nis not a subtype of Expected type\n\t" 
                          (or (-> t meta :source-Name)
                              (with-out-str (pr (prs/unparse-type t))))
                          (when vs/*current-expr*
                            (str "\n\nForm: " (u/emit-form-fn vs/*current-expr*))))
                  {:type-error subtype-error})))

(defmacro handle-failure [& body]
  `(u/with-ex-info-handlers
     [subtype-error? (constantly false)]
     ~@body))

;keeps track of currently seen subtype relations for recursive types.
;(Set [Type Type])
(def ^:dynamic *sub-current-seen* #{})

(declare subtypes*-varargs)

;[(Seqable Type) (Seqable Type) Type -> Boolean]
(defn subtypes-varargs?
  "True if argtys are under dom"
  [argtys dom rst]
  (handle-failure
    (subtypes*-varargs #{} argtys dom rst)
    true))


;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument (the current-seen subtypes), called by subtype
;subtype* shouldn't be called directly, is called by subtypeA*
;
; In short, only call subtype (or subtype?)

;[Type Type -> (IPersistentSet '[Type Type])]
(defmulti subtype* (fn [s t] [(class s) (class t) @impl/TYPED-IMPL]))

(declare subtype)

;[Type Type -> Boolean]
(defn subtype? [s t]
  {:post [(u/boolean? %)]}
  (boolean
    (handle-failure
      (subtype s t))))

(declare subtypeA*)

;[(IPersistentSet '[Type Type]) Type Type -> Boolean]
(defn subtypeA*? [A s t]
  (handle-failure
    (subtypeA* A s t)))

(declare supertype-of-one-arr)

(defn infer-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.cs-gen) 'infer)]
    (assert (var? v) "infer unbound")
    v))

(defmacro handle-cgen-failure [& body]
  `(u/with-ex-info-handlers
     [@(ns-resolve (find-ns '~'clojure.core.typed.cs-gen) '~'cs-error?) (constantly false)]
     ~@body))

;[(IPersistentMap Symbol Bounds) (Seqable Type) (Seqable Type)
;  -> Boolean]
(defn unify [X S T]
  (let [infer @(infer-var)]
    (boolean 
      (handle-cgen-failure
        (infer X {} S T r/-any)))))

(declare subtype-TApp? protocol-descendants)

;[(IPersistentSet '[Type Type]) Type Type -> (IPersistentSet '[Type Type])]
(defn subtypeA* [A s t]
  {:post [(set? %)]}
  (if (or (contains? A [s t])
          (= s t)
          (r/Top? t)
          (r/Bottom? s)
          ;TCError is top and bottom
          (some r/TCError? [s t]))
    A
    (binding [*sub-current-seen* (conj A [s t])]
      (cond
        (and (r/Value? s)
             (r/Value? t))
        ;already (not= s t)
        (fail! s t)

        (r/Name? s)
        (subtypeA* *sub-current-seen* (c/resolve-Name s) t)

        (r/Name? t)
        (subtypeA* *sub-current-seen* s (c/resolve-Name t))

        (and (r/Poly? s)
             (r/Poly? t)
             (= (.nbound ^Poly s) (.nbound ^Poly t)))
        (let [names (repeatedly (.nbound ^Poly s) gensym)
              b1 (c/Poly-body* names s)
              b2 (c/Poly-body* names t)]
          (subtype b1 b2))

        ;use unification to see if we can use the Poly type here
        (and (r/Poly? s)
             (let [names (repeatedly (.nbound ^Poly s) gensym)
                   bnds (c/Poly-bbnds* names s)
                   b1 (c/Poly-body* names s)]
               (unify (zipmap names bnds) [b1] [t])))
        (let [names (repeatedly (.nbound ^Poly s) gensym)
              bnds (c/Poly-bbnds* names s)
              b1 (c/Poly-body* names s)]
          (if (unify (zipmap names bnds) [b1] [t])
            *sub-current-seen*
            (fail! s t)))

        (and (r/Poly? t)
             (let [names (repeatedly (.nbound ^Poly t) gensym)
                   b (c/Poly-body* names t)]
               (empty? (frees/fv t))))
        (let [names (repeatedly (.nbound ^Poly t) gensym)
              b (c/Poly-body* names t)]
          (subtype s b))

        (and (r/TApp? s)
             (r/TApp? t))
        (if (subtype-TApp? s t)
          *sub-current-seen*
          (fail! s t))

        (r/TApp? s)
        (let [^TApp s s]
          (if (and (not (r/F? (.rator s)))
                   (subtypeA*? (conj *sub-current-seen* [s t])
                               (c/resolve-TApp s) t))
            *sub-current-seen*
            (fail! s t)))

        (r/TApp? t)
        (let [^TApp t t]
          (if (and (not (r/F? (.rator t)))
                   (subtypeA*? (conj *sub-current-seen* [s t])
                               s (c/resolve-TApp t)))
            *sub-current-seen*
            (fail! s t)))

        (r/App? s)
        (subtypeA* *sub-current-seen* (c/resolve-App s) t)

        (r/App? t)
        (subtypeA* *sub-current-seen* s (c/resolve-App t))

        (r/Bottom? t)
        (fail! s t)

        (r/Union? s)
        ;use subtypeA*, throws error
        (if (every? #(subtypeA* *sub-current-seen* % t) (.types ^Union s))
          *sub-current-seen*
          (fail! s t))

        ;use subtypeA*?, boolean result
        (r/Union? t)
        (if (some #(subtypeA*? *sub-current-seen* s %) (.types ^Union t))
          *sub-current-seen*
          (fail! s t))

        (and (r/FnIntersection? s)
             (r/FnIntersection? t))
        (loop [A* *sub-current-seen*
               arr2 (:types t)]
          (let [arr1 (:types s)]
            (if (empty? arr2) 
              A*
              (if-let [A (supertype-of-one-arr A* (first arr2) arr1)]
                (recur A (next arr2))
                (fail! s t)))))

        (and (r/Intersection? s)
             (r/Intersection? t))
        (if (every? (fn [s*]
                      (some #(subtype? s* %) (.types ^Intersection t)))
                    (.types ^Intersection s))
          *sub-current-seen*
          (fail! s t))

        (r/Intersection? s)
        (if (some #(subtype? % t) (.types ^Intersection s))
          *sub-current-seen*
          (fail! s t))

        (r/Intersection? t)
        (if (every? #(subtype? s %) (.types ^Intersection t))
          *sub-current-seen*
          (fail! s t))

        (r/Mu? s)
        (subtype (c/unfold s) t)

        (r/Mu? t)
        (subtype s (c/unfold t))

        (and (r/TopFunction? t)
             (r/FnIntersection? s))
        *sub-current-seen*

        (and (r/HeterogeneousVector? s)
             (r/HeterogeneousVector? t))
        (if (= (count (:types s))
               (count (:types t)))
          (or (last (doall (map #(subtype %1 %2) (:types s) (:types t))))
              #{})
          (fail! s t))

        (and (r/HeterogeneousList? s)
             (r/HeterogeneousList? t))
        (if (= (count (:types s))
               (count (:types t)))
          (or (last (doall (map #(subtype %1 %2) (:types s) (:types t))))
              #{})
          (fail! s t))

        (and (r/HeterogeneousSeq? s)
             (r/HeterogeneousSeq? t))
        (if (= (count (:types s))
               (count (:types t)))
          (or (last (doall (map #(subtype %1 %2) (:types s) (:types t))))
              #{})
          (fail! s t))

        (r/KwArgsSeq? s)
        (subtype (c/Un r/-nil (c/RClass-of Seqable [r/-any])) t)

        ;values are subtypes of their classes
        (and (r/Value? s)
             (impl/checking-clojure?))
        (let [^Value s s]
          (if (nil? (.val s))
            (fail! s t)
            (subtype (apply c/In (c/RClass-of (class (.val s)))
                            (cond
                              ;keyword values are functions
                              (keyword? (.val s)) [(c/keyword->Fn (.val s))]
                              ;strings have a known length as a seqable
                              (string? (.val s)) [(r/make-ExactCountRange (count (.val s)))]))
                     t)))

        (and (r/Protocol? s)
             (r/Protocol? t))
        (let [{var1 :the-var variances* :variances poly1 :poly?} s
              {var2 :the-var poly2 :poly?} t]
          (if (and (= var1 var2)
                   (every? (fn [[v l r]]
                             (case v
                               :covariant (subtypeA* *sub-current-seen* l r)
                               :contravariant (subtypeA* *sub-current-seen* r l)
                               :invariant (and (subtypeA* *sub-current-seen* l r)
                                               (subtypeA* *sub-current-seen* r l))))
                           (map vector variances* poly1 poly2)))
            *sub-current-seen*
            (fail! s t)))

        (r/Protocol? s)
        (if (= (c/RClass-of Object) t)
          *sub-current-seen*
          (fail! s t))
        
        (r/Protocol? t)
        (let [desc (protocol-descendants t)]
          (if (some #(subtype? s %) desc)
            *sub-current-seen*
            (fail! s t)))

        :else (subtype* s t)))))

(defn protocol-descendants [^Protocol p]
  {:pre [(r/Protocol? p)]
   :post [(every? r/Type? %)]}
  (let [protocol-var (resolve (.the-var p))
        _ (assert protocol-var (str "Protocol cannot be resolved: " (.the-var p)))
        exts (extenders @protocol-var)]
    (for [ext exts]
      (cond
        (class? ext) (c/RClass-of-with-unknown-params ext)
        (nil? ext) r/-nil
        :else (throw (Exception. (str "What is this?" ext)))))))

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
      (empty? argtys) (fail! argtys dom)
      (and (empty? dom) rst)
      (if-let [A (subtypeA* A (first argtys) rst)]
        (recur dom (next argtys) A)
        (fail! (first argtys) rst))

      (empty? dom) (fail! argtys dom)
      :else
      (if-let [A (subtypeA* A0 (first argtys) (first dom))]
        (recur (next dom) (next argtys) A)
        (fail! (first argtys) (first dom))))))

(defn subtype-kwargs* [^KwArgs s ^KwArgs t]
  {:pre [((some-fn r/KwArgs? nil?) s)
         ((some-fn r/KwArgs? nil?) t)]}
  (if (= s t)
    *sub-current-seen*
    (u/nyi-error "subtype kwargs")))

;; simple co/contra-variance for ->
;[(IPersistentSet '[Type Type]) Function Function -> (IPersistentSet '[Type Type])]
(defn arr-subtype [A0 ^Function s ^Function t]
  {:pre [(r/Function? s)
         (r/Function? t)]}
  ;; top for functions is above everything
  (cond
    ;; top for functions is above everything
    (r/TopFunction? t) A0
    ;; the really simple case
    (and (not ((some-fn :rest :drest :kws) s))
         (not ((some-fn :rest :drest :kws) t)))
    (do
      (when-not (= (count (.dom s))
                   (count (.dom t)))
        (fail! s t))
      (-> *sub-current-seen*
        ((fn [A0]
           (reduce (fn [A* [s t]]
                     (subtypeA* A* s t))
                   A0
                   (map vector (.dom t) (.dom s)))))
        (subtypeA* (.rng s) (.rng t))))

    ;kw args
    (and (.kws s)
         (.kws t))
    (do
      (mapv subtype (.dom t) (.dom s))
      (subtype (.rng s) (.rng t))
      (subtype-kwargs* (.kws t) (.kws s)))

    (and (:rest s)
         (not ((some-fn :rest :drest) t)))
    (-> *sub-current-seen*
      (subtypes*-varargs (.dom t) (.dom s) (.rest s))
      (subtypeA* (.rng s) (.rng t)))

    (and (not ((some-fn :rest :drest) s))
         (:rest t))
    (fail! s t)

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
    :else (fail! s t)))

;[(IPersistentSet '[Type Type]) Function (Seqable Function) -> (Option (IPersistentSet '[Type Type]))]
(defn supertype-of-one-arr [A s ts]
  (some #(handle-failure 
           (arr-subtype A % s))
        ts))

(defmethod subtype* [Result Result impl/default]
  [{t1 :t f1 :fl o1 :o :as s}
   {t2 :t f2 :fl o2 :o :as t}]
  (cond
    ;trivial case
    (and (= f1 f2)
         (= o1 o2))
    (subtype t1 t2)

    ;we can ignore some interesting results
    (and (orep/EmptyObject? o2)
         (= f2 (fops/-FS fr/-top fr/-top)))
    (subtype t1 t2)

    ;special case for (& (is y sym) ...) <: (is y sym)
    (and (fr/AndFilter? (:then f1))
         (fr/TypeFilter? (:then f2))
         (every? fops/atomic-filter? (:fs (:then f1)))
         (= 1 (count (filter fr/TypeFilter? (:fs (:then f1)))))
         (= fr/-top (:else f2)))
    (let [f1-tf (first (filter fr/TypeFilter? (:fs (:then f1))))]
      (if (= f1-tf (:then f2))
        (subtype t1 t2)
        (throw (Exception. (u/error-msg "Filters do not match: \n" (prs/unparse-filter-set f1) "\n" (prs/unparse-filter-set f2))))))

    :else (if (= o1 o2)
            (throw (Exception. (u/error-msg "Filters do not match: \n" (prs/unparse-filter-set f1) "\n" (prs/unparse-filter-set f2))))
            (throw (Exception. (u/error-msg "Objects do not match " (prs/unparse-object o1) (prs/unparse-object o2)))))))

(defn subtype-TypeFn-app?
  [^TypeFn tfn ^TApp ltapp ^TApp rtapp]
  {:pre [(r/TypeFn? tfn)
         (r/TApp? ltapp)
         (r/TApp? rtapp)]}
  (every? (fn [[v l r]]
            (case v
              :covariant (subtypeA*? *sub-current-seen* l r)
              :contravariant (subtypeA*? *sub-current-seen* r l)
              :invariant (and (subtypeA*? *sub-current-seen* l r)
                              (subtypeA*? *sub-current-seen* r l))))
          (map vector (.variances tfn) (.rands ltapp) (.rands rtapp))))

(defmulti subtype-TApp? (fn [^TApp S ^TApp T] 
                          {:pre [(r/TApp? S)
                                 (r/TApp? T)]}
                          [(class (.rator S)) (class (.rator T))
                           (= (.rator S) (.rator T))]))

(defmethod subtype-TApp? [TypeFn TypeFn false]
  [S T]
  (subtypeA*? (conj *sub-current-seen* [S T]) (c/resolve-TApp S) (c/resolve-TApp T)))

(defmethod subtype-TApp? [TypeFn TypeFn true]
  [^TApp S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TypeFn-app? (.rator S) S T)))

(defmethod subtype-TApp? [r/AnyType Name false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? S (update-in T [:rator] c/resolve-Name))))

(defmethod subtype-TApp? [Name r/AnyType false]
  [S T]
  (binding [*sub-current-seen* (conj *sub-current-seen* [S T])]
    (subtype-TApp? (update-in S [:rator] c/resolve-Name) T)))

; for [Name Name false]
(prefer-method subtype-TApp? 
               [Name r/AnyType false]
               [r/AnyType Name false])

;same operator
(defmethod subtype-TApp? [Name Name true]
  [^TApp S T]
  (let [r (c/resolve-Name (.rator S))]
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
                  free-ops/*free-scope*)]
    (when tfn
      (subtype-TypeFn-app? tfn S T))))

(defmethod subtype-TApp? :default [S T] false)

(prefer-method subtype* [r/Type TApp impl/default] [HeterogeneousVector r/Type impl/default])
(prefer-method subtype* [r/Type TApp impl/default] [HeterogeneousVector r/Type impl/default])

(defmethod subtype* [TypeFn TypeFn impl/default]
  [^TypeFn S ^TypeFn T]
  (if (and (= (.nbound S) (.nbound T))
           (= (.variances S) (.variances T))
           (= (.bbnds S) (.bbnds T))
           (let [names (repeatedly (.nbound S) gensym)
                 sbody (c/TypeFn-body* names S)
                 tbody (c/TypeFn-body* names T)]
             (subtype? sbody tbody)))
    *sub-current-seen*
    (fail! S T)))

(defmethod subtype* [PrimitiveArray r/Type impl/clojure]
  [_ t]
  (subtype (r/->PrimitiveArray Object r/-any r/-any) t))

(defmethod subtype* [PrimitiveArray PrimitiveArray impl/clojure]
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
    (fail! s t)))

(defn- subtype-datatype-record-on-left
  [{:keys [the-class] :as s} t]
  (if (some #(subtype? % t) (set/union #{(c/RClass-of Object)} 
                                       (or (@dtenv/DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (fail! s t)))

;Not quite correct, datatypes have other implicit ancestors (?)
(defmethod subtype* [DataType r/Type impl/clojure] [s t] (subtype-datatype-record-on-left s t))

(defn- subtype-datatype-record-on-right
  [s {:keys [the-class] :as t}]
  (if (some #(subtype? s %) (set/union #{(c/RClass-of Object)} 
                                       (or (@dtenv/DATATYPE-ANCESTOR-ENV the-class)
                                           #{})))
    *sub-current-seen*
    (fail! s t)))

(defmethod subtype* [r/Type DataType impl/clojure] [s t] (subtype-datatype-record-on-right s t))

(defn- subtype-datatypes-or-records
  [{cls1 :the-class poly1 :poly? :as s} 
   {cls2 :the-class poly2 :poly? :as t}]
  {:pre [(every? r/DataType? [s t])]}
  (if (and (= cls1 cls2)
           (every? (fn [[v l r]]
                     (case v
                       :covariant (subtypeA* *sub-current-seen* l r)
                       :contravariant (subtypeA* *sub-current-seen* r l)
                       :invariant (and (subtypeA* *sub-current-seen* l r)
                                       (subtypeA* *sub-current-seen* r l))))
                   (map vector (:variances s) poly1 poly2)))
    *sub-current-seen*
    (fail! s t)))

(defmethod subtype* [DataType DataType impl/default] [s t] (subtype-datatypes-or-records s t))

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
      (fail! s t))))

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
  (cond
    ; (U Integer Long) <: (U int long)
    (and 
      (#{(c/RClass-of Integer) (c/RClass-of Long)} s)
      (#{(c/RClass-of 'int) (c/RClass-of 'long)} t))
    true

    :else
    (let [spcls (u/symbol->Class (:the-class s))
          tpcls (u/symbol->Class (:the-class t))
          scls (or (boxed-primitives spcls)
                   spcls)
          tcls (or (boxed-primitives tpcls)
                   tpcls)]
      (isa? scls tcls))))

(defmethod subtype* [RClass RClass impl/clojure]
  [{polyl? :poly? :as s}
   {polyr? :poly? :as t}]
  (let [scls (r/RClass->Class s)
        tcls (r/RClass->Class t)]
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
              (c/RClass-supers* s)))
      *sub-current-seen*

      ;try each ancestor

      :else (fail! s t))))

(defmethod subtype* [HeterogeneousMap r/Type impl/clojure]
  [^HeterogeneousMap s t]
  ; Partial HMaps do not record absence of fields, only subtype to (APersistentMap Any Any)
  (if (c/complete-hmap? s)
    (subtype (c/RClass-of APersistentMap [(apply c/Un (keys (.types s)))
                                        (apply c/Un (vals (.types s)))]) 
             t)
    (subtype (c/RClass-of APersistentMap [r/-any r/-any]) t)))

;every rtype entry must be in ltypes
;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
(defmethod subtype* [HeterogeneousMap HeterogeneousMap impl/default]
  [{ltypes :types :as s}
   {rtypes :types :as t}]
  (or (last (doall (map (fn [[k v]]
                          (if-let [t (ltypes k)]
                            (subtype t v)
                            (fail! s t)))
                        rtypes)))
      #{}))

(prefer-method subtype* 
               [HeterogeneousVector HeterogeneousVector impl/default]
               [HeterogeneousVector r/Type impl/clojure])
(prefer-method subtype* 
               [HeterogeneousMap HeterogeneousMap impl/default],
               [HeterogeneousMap r/Type impl/clojure] )

(defmethod subtype* [HeterogeneousVector r/Type impl/clojure]
  [s t]
  (let [ss (apply c/Un (:types s))]
    (subtype (c/In (c/RClass-of APersistentVector [ss])
                   (r/make-ExactCountRange (count (:types s))))
             t)))

(defmethod subtype* [HeterogeneousList r/Type impl/clojure]
  [s t]
  (let [ss (apply c/Un (:types s))]
    (subtype (c/RClass-of PersistentList [ss]) t)))

(defmethod subtype* [HeterogeneousSeq r/Type impl/clojure]
  [s t]
  (let [ss (apply c/Un (:types s))]
    (subtype (c/RClass-of (u/Class->symbol ASeq) [ss])
             t)))

;subtype if t includes all of s. 
;tl <= sl, su <= tu
(defmethod subtype* [CountRange CountRange impl/default]
  [{supper :upper slower :lower :as s}
   {tupper :upper tlower :lower :as t}]
  (if (and (<= tlower slower)
           (if tupper
             (and supper (<= supper tupper))
             true))
    *sub-current-seen*
    (fail! s t)))

(defmethod subtype* :default
  [s t]
  #_(prn "subtype :default" @impl/TYPED-IMPL (prs/unparse-type s) (prs/unparse-type t))
  (if (r/Top? t)
    *sub-current-seen*
    (fail! s t)))

(defmacro sub [s t]
  `(subtype (parse-type '~s)
            (parse-type '~t)))
