(ns ^:skip-wiki clojure.core.typed.check.utils
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.ns-deps :as ns-deps]
            [clojure.core.typed.ns-deps-utils :as ns-depsu]
            [clojure.core.typed.reflect-utils :as reflect-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.debug :as d]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.protocol-env :as pcl-env]
            [clojure.core.typed.method-param-nilables :as mtd-param-nil]
            [clojure.core.typed.method-return-nilables :as mtd-ret-nil]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.ast-utils :as ast-u])
  (:import (clojure.lang MultiFn)))

(alter-meta! *ns* assoc :skip-wiki true)

;(t/ann expr-ns [Any -> t/Sym])
(defn expr-ns [expr]
  {:post [(symbol? %)]}
  (impl/impl-case
    :clojure (let [nsym (get-in expr [:env :ns])
                   _ (assert (symbol? nsym) (str "Bug! " (:op expr) " expr has no associated namespace"
                                                 nsym))]
               (ns-name nsym))
    :cljs (or (-> expr :env :ns :name)
              (do (prn "WARNING: No associated ns for ClojureScript expr, defaulting to cljs.user")
                  'cljs.user))))

(defn KeyPE->Type [k]
  {:pre [(pe/KeyPE? k)]
   :post [(r/Type? %)]}
  (r/-val (:val k)))

(defn fn-self-name [{:keys [op] :as fexpr}]
  (impl/impl-case
    :clojure (do (assert (#{:fn} op))
                 (-> fexpr :local :name))
    :cljs (do (assert (#{:fn} op))
              (-> fexpr :name :name))))

;[MethodExpr -> (U nil NamespacedSymbol)]
(defn MethodExpr->qualsym [{c :class :keys [op method] :as expr}]
  {:pre [(#{:static-call :instance-call} op)]
   :post [((some-fn nil? symbol?) %)]}
  (when c
    (assert (class? c))
    (assert (symbol? method))
    (symbol (str (coerce/Class->symbol c)) (str method))))

;(t/ann expected-error [r/Type r/Type -> nil])
(defn expected-error [actual expected]
  (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                           (when vs/*current-expr*
                             (expr-ns vs/*current-expr*)))
    (err/tc-delayed-error (str "Type mismatch:"
                             "\n\nExpected: \t" (pr-str (prs/unparse-type expected))
                             "\n\nActual: \t" (pr-str (prs/unparse-type actual))))))


;(t/ann error-ret [(U nil TCResult) -> TCResult])
(defn error-ret 
  "Return a TCResult appropriate for when a type
  error occurs, with expected type expected.
  
  Use *only* in case of a type error."
  [expected]
  {:pre [((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (or expected
      (r/ret (r/TCError-maker))))

;[Type -> '[Type (Option (Seqable t/Sym)) (Option (Seqable F)) (Option (Seqable Bounds)) (Option (U :Poly :PolyDots))]
; -> Type]
(defn unwrap-poly
  "Return a pair vector of the instantiated body of the possibly polymorphic
  type and the names used"
  [t]
  {:pre [(r/Type? t)]
   :post [((con/hvector-c? r/Type? 
                           (some-fn nil? (con/every-c? r/F?))
                           (some-fn nil? (con/every-c? r/Bounds?))
                           (some-fn nil? #{:Poly :PolyDots})) %)]}
  (cond
    (r/Poly? t) (let [new-nmes (c/Poly-fresh-symbols* t)
                      new-frees (map r/make-F new-nmes)]
                  [(c/Poly-body* new-nmes t) new-frees (c/Poly-bbnds* new-nmes t) :Poly])
    (r/PolyDots? t) (let [new-nmes (c/PolyDots-fresh-symbols* t)
                          new-frees (map r/make-F new-nmes)]
                      [(c/PolyDots-body* new-nmes t) new-frees (c/PolyDots-bbnds* new-nmes t) :PolyDots])
    :else [t nil nil nil]))

(def not-special ::not-special)

;(t/ann hvec->rets [HeterogeneousVector -> (Seqable TCResult)])
(defn hvec->rets [v]
{:pre [(r/HeterogeneousVector? v)]
 :post [(every? r/TCResult? %)]}
(map r/ret
       (:types v)
       (:fs v)
       (:objects v)))

;[Type (Seqable t/Sym) (Seqable F) (U :Poly :Polydots nil) -> Type]
(defn rewrap-poly [body inst-frees bnds poly?]
  {:pre [(r/Type? body)
         (every? r/F? inst-frees)
         ((some-fn nil? #{:Poly :PolyDots}) poly?)]
   :post [(r/Type? %)]}
  (case poly?
    :Poly (c/Poly* (map :name inst-frees) bnds body)
    :PolyDots (c/PolyDots* (map :name inst-frees) bnds body)
    body))

(defn- get-demunged-protocol-method [unwrapped-p mungedsym]
  {:pre [(symbol? mungedsym)
         (r/Protocol? unwrapped-p)]
   :post [(r/Type? %)]}
  (let [munged-methods (zipmap 
                         (->> (keys (:methods unwrapped-p))
                              (map munge))
                         (vals (:methods unwrapped-p)))
        mth (get munged-methods mungedsym)
        _ (when-not mth
            (err/int-error (str "No matching annotation for protocol method implementation: "
                              mungedsym)))]
    mth))

; don't check these implicit methods in a record
(def record-implicits
  '#{entrySet values keySet clear putAll remove put get containsValue isEmpty size without
     assoc iterator seq entryAt containsKey equiv cons empty count getLookupThunk valAt
     withMeta meta equals hashCode hasheq})

(declare symbol->PArray)

;[t/Sym Boolean -> Type]
(defn Java-symbol->Type [sym nilable?]
  {:pre [(symbol? sym)
         (con/boolean? nilable?)]
   :post [(r/Type? %)]}
  (if-let [typ (or ((prs/clj-primitives-fn) sym)
                   (symbol->PArray sym nilable?)
                   (when-let [cls (resolve sym)]
                     (apply c/Un (c/RClass-of-with-unknown-params cls)
                            (when nilable?
                              [r/-nil]))))]
    typ
    (err/tc-delayed-error (str "Method or field symbol " sym " does not resolve to a type"))))

;[t/Sym Boolean -> (Option Type)]
(defn- symbol->PArray [sym nilable?]
  {:pre [(symbol? sym)
         (con/boolean? nilable?)]
   :post [((some-fn nil? r/PrimitiveArray?) %)]}
  (let [s (str sym)]
    (when (.endsWith s "<>")
      (let [^String s-nosuffix (apply str (drop-last 2 s))]
        (assert (not (.contains s-nosuffix "<>")))
        ;Nullable elements
        (let [t (Java-symbol->Type (symbol s-nosuffix) nilable?)
              c (let [c (or (when-let [rclass ((prs/clj-primitives-fn) (symbol s-nosuffix))]
                              (r/RClass->Class rclass))
                            (resolve (symbol s-nosuffix)))
                      _ (assert (class? c) s-nosuffix)]
                  c)]
          (r/PrimitiveArray-maker c t t))))))


;[clojure.reflect.Field - Type]
(defn Field->Type [{:keys [type flags] :as field}]
  {:pre [(instance? clojure.reflect.Field field)
         flags]
   :post [(r/Type? %)]}
  (cond
    (:enum flags) (Java-symbol->Type type false)
    :else (Java-symbol->Type type true)))

(def method-map?
  (con/hmap-c? :declaring-class symbol?
               :return-type symbol?
               :name symbol?
               :parameter-types (con/every-c? symbol?)
               :flags (con/set-c? keyword?)))

;[MethodMap -> Type]
(defn- instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method}]
  {:pre [(method-map? method)]
   :post [(r/FnIntersection? %)]}
  (assert (class? (resolve declaring-class)))
  (r/make-FnIntersection (r/make-Function (concat [(c/RClass-of-with-unknown-params declaring-class)]
                                                  (doall (map #(Java-symbol->Type % false) parameter-types)))
                                          (Java-symbol->Type return-type true))))


;[Type TCResult -> Type]
(defn extend-method-expected 
  "Returns the expected type with target-type intersected with the first argument"
  [target-type expected]
  {:pre [(r/Type? target-type)
         (r/Type? expected)]
   :post [(r/Type? %)]}
  (cond
    (r/FnIntersection? expected)
    (-> expected
        (update-in [:types] #(for [ftype %]
                               (do
                                 (assert (<= 1 (count (:dom ftype))))
                                 (-> ftype
                                     (update-in [:dom] (fn [dom] 
                                                         (update-in (vec dom) [0] (partial c/In target-type)))))))))

    (r/Poly? expected)
    (let [names (c/Poly-fresh-symbols* expected)
          body (c/Poly-body* names expected)
          body (extend-method-expected target-type body)]
      (c/Poly* names 
               (c/Poly-bbnds* names expected)
               body))

    (r/PolyDots? expected)
    (let [names (c/PolyDots-fresh-symbols* expected)
          body (c/PolyDots-body* names expected)
          body (extend-method-expected target-type body)]
      (c/PolyDots* names 
                   (c/PolyDots-bbnds* names expected)
                   body))
    :else (err/int-error (str "Expected Function type, found " (prs/unparse-type expected)))))

(defn protocol-implementation-type [datatype {:keys [declaring-class] :as method-sig}]
  (let [pvar (c/Protocol-interface->on-var declaring-class)
        ptype (pcl-env/get-protocol pvar)
        mungedsym (symbol (:name method-sig))
        ans (map c/fully-resolve-type (sub/datatype-ancestors datatype))]
    (when ptype
      (let [pancestor (if (r/Protocol? ptype)
                        ptype
                        (let [[an :as relevant-ancestors] 
                              (filter 
                                (fn [a] 
                                  (and (r/Protocol? a)
                                       (= (:the-var a) pvar)))
                                ans)
                              _ (when (empty? relevant-ancestors)
                                  (err/int-error (str "Must provide instantiated ancestor for datatype "
                                                    (:the-class datatype) " to check protocol implementation: "
                                                    pvar)))
                              _ (when (< 1 (count relevant-ancestors))
                                  (err/int-error (str "Ambiguous ancestors for datatype when checking protocol implementation: "
                                                    (pr-str (vec relevant-ancestors)))))]
                          an))
            _ (assert (r/Protocol? pancestor) (pr-str pancestor))
            ;_ (prn "pancestor" pancestor)
            pargs (seq (:poly? pancestor))
            unwrapped-p (if (r/Protocol? ptype)
                          ptype
                          (c/instantiate-typefn ptype pargs))
            _ (assert (r/Protocol? unwrapped-p))
            mth (get-demunged-protocol-method unwrapped-p mungedsym)]
        (extend-method-expected datatype mth)))))

(defn datatype-method-expected [datatype method-sig]
  {:post [(r/Type? %)]}
  (or (protocol-implementation-type datatype method-sig)
      (extend-method-expected datatype (instance-method->Function method-sig))))

(defn FieldExpr->Field [{c :class field-name :field :keys [op] :as expr}]
  {:pre []
   :post [(instance? clojure.reflect.Field %)]}
  (when (and c 
             (#{:static-field :instance-field} op))
    (let [fs (->> (reflect-u/reflect c)
                  :members
                  (filter #(instance? clojure.reflect.Field %))
                  (filter #(#{field-name} (:name %))))]
      (assert (#{1} (count fs)))
      (first fs))))

(defn MethodExpr->Method [{c :class method-name :method :keys [op args] :as expr}]
  {:pre []
   :post [(or (nil? %) (instance? clojure.reflect.Method %))]}
  (when (and c 
             (#{:static-call :instance-call} op))
    (let [ms (->> (reflect-u/reflect c)
                  :members
                  (filter #(instance? clojure.reflect.Method %))
                  (filter #(#{method-name} (:name %)))
                  (filter (fn [{:keys [parameter-types]}]
                            (#{(map (comp reflect-u/reflect-friendly-sym :tag) args)} parameter-types))))]
      ;(prn "MethodExpr->Method" c ms (map :tag args))
      (first ms))))

(defn NewExpr->Ctor [{:keys [op args] :as expr}]
  {:pre [(#{:new} op)]
   :post [(or (instance? clojure.reflect.Constructor %)
              (nil? %))]}
  (let [cs (->> (reflect-u/reflect (-> expr ast-u/new-op-class))
                :members
                (filter #(instance? clojure.reflect.Constructor %))
                (filter #(#{(map (comp reflect-u/reflect-friendly-sym :tag) args)} (:parameter-types %))))]
    ;(prn "NewExpr->Ctor" cs)
    (first cs)))

;FIXME I think this hurts more than it helps
;[Type (Seqable t/Sym) -> Type]
;[Type -> Type]
(defn unwrap-datatype
  "Takes a DataType that might be wrapped in a TypeFn and returns the 
  DataType after instantiating it"
  ([dt nms]
   {:pre [((some-fn r/DataType? r/TypeFn?) dt)
          (every? symbol? nms)]
    :post [(r/DataType? %)]}
   (if (r/TypeFn? dt)
     (c/TypeFn-body* nms dt)
     dt))
  ([dt] (let [nms (when (r/TypeFn? dt)
                    (c/TypeFn-fresh-symbols* dt))]
          (unwrap-datatype dt nms))))

;[t/Sym -> Type]
(defn DataType-ctor-type [sym]
  (letfn [(resolve-ctor [dtp]
            (cond
              ((some-fn r/DataType? r/Record?) dtp) 
              (let [dt dtp]
                (r/make-FnIntersection 
                  (r/make-Function (-> (c/DataType-fields* dt) vals) dt)))

              (r/TypeFn? dtp) (let [nms (c/TypeFn-fresh-symbols* dtp)
                                    bbnds (c/TypeFn-bbnds* nms dtp)
                                    body (c/TypeFn-body* nms dtp)]
                                (c/Poly* nms
                                         bbnds
                                         (free-ops/with-bounded-frees (zipmap (map r/make-F nms) bbnds)
                                           (resolve-ctor body))))

              :else (err/tc-delayed-error (str "Cannot generate constructor type for: " sym)
                                        :return r/Err)))]
    (resolve-ctor (dt-env/get-datatype sym))))

;[Method -> t/Sym]
(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

(defn method-nilable-param? [msym nparams n]
  {:post [(con/boolean? %)]}
  (let [n (mtd-param-nil/nilable-param? msym nparams n)]
    (if n
      (u/p :check.method/has-nilable-param)
      (u/p :check.method/no-nilable-param))
    n))

(defn method-nonnilable-return? [msym nparams]
  {:post [(con/boolean? %)]}
  (let [n (mtd-ret-nil/nonnilable-return? msym nparams)]
    (if n
      (u/p :check.method/has-nonnilable-return)
      (u/p :check.method/no-nonnilable-return))
    n))

;[clojure.reflect.Method -> Type]
(defn Method->Type [{:keys [parameter-types return-type flags] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(r/FnIntersection? %)]}
  (p/p :check.method/inside-Method->Type)
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (r/make-FnIntersection (r/make-Function (doall (map (fn [[n tsym]] 
                                                          (Java-symbol->Type 
                                                            tsym 
                                                            (method-nilable-param? msym nparams n)))
                                                      (map-indexed vector
                                                                   (if (:varargs flags)
                                                                     (butlast parameter-types)
                                                                     parameter-types))))
                                          (Java-symbol->Type 
                                            return-type 
                                            (not (method-nonnilable-return? msym nparams)))
                                          (when (:varargs flags)
                                            (Java-symbol->Type 
                                              (last parameter-types) 
                                              (method-nilable-param? msym nparams (dec nparams))))))))

;[clojure.reflect.Constructor -> Type]
(defn Constructor->Function [{:keys [declaring-class parameter-types] :as ctor}]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(r/FnIntersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (err/tc-delayed-error (str "Constructor for unresolvable class " (:class ctor))))]
    (r/make-FnIntersection (r/make-Function (doall (map #(Java-symbol->Type % false) parameter-types))
                                            (c/RClass-of-with-unknown-params cls)
                                            nil nil 
                                            ;always a true value. Cannot construct nil
                                            ; or primitive false
                                            :filter (fo/-true-filter)))))

;[(Seqable Expr) (Option Expr) FnIntersection -> (Seqable Function)]
(defn relevant-Fns
  "Given a set of required-param exprs, rest-param expr, and a FnIntersection,
  returns a seq of Functions containing Function types
  whos arities could be a subtype to the method with the fixed and rest parameters given"
  [required-params rest-param fin]
  {:pre [(r/FnIntersection? fin)]
   :post [(every? r/Function? %)]}
  (let [nreq (count required-params)]
    ;(prn "nreq" nreq)
    ;(prn "rest-param" rest-param)
    (filter (fn [{:keys [dom rest drest kws]}]
              (let [ndom (count dom)]
                (if rest-param 
                  (or ; required parameters can flow into the rest type
                      (when (or rest drest)
                        (<= nreq ndom))
                      ; kw functions must have exact fixed domain match
                      (when kws
                        (= nreq ndom)))
                  (and (not rest) (= nreq ndom)))))
            (:types fin))))

(defn default-defmethod? [var dispatch-val]
  {:pre [(var? var)]}
  (let [^MultiFn multifn @var
        _ (assert (instance? clojure.lang.MultiFn multifn))
        default-val (.defaultDispatchVal multifn)]
    (= default-val dispatch-val)))

(t/ann checked-ns! [t/Sym -> nil])
(defn- checked-ns! [nsym]
  (t/when-let-fail [a vs/*already-checked*]
    (swap! a conj nsym))
  nil)

(t/ann already-checked? [t/Sym -> Boolean])
(defn already-checked? [nsym]
  (t/when-let-fail [a vs/*already-checked*]
    (boolean (@a nsym))))

(defn check-ns-and-deps*
  "Type check a namespace and its dependencies.
  Assumes type annotations in each namespace
  has already been collected."
  ([nsym {:keys [ast-for-ns
                 check-asts
                 check-ns]}]
   {:pre [(symbol? nsym)]
    :post [(nil? %)]}
   (u/p :check/check-ns-and-deps
   (let []
     (cond 
       (already-checked? nsym) (do
                                 ;(println (str "Already checked " nsym ", skipping"))
                                 ;(flush)
                                 nil)
       :else
       ; check deps
       (let [deps (u/p :check/ns-immediate-deps 
                    (ns-deps/typed-deps nsym))]
         (checked-ns! nsym)
         ;check deps added with typed-deps
         (doseq [dep deps]
           (check-ns dep))
         ;check normal dependencies
         (doseq [dep (ns-depsu/deps-for-ns nsym)
                 :when (ns-depsu/should-check-ns? nsym)]
           (check-ns dep))
         ; ignore ns declaration
         (let [ns-form (ns-depsu/ns-form-for-ns nsym)
               check? (boolean (some-> ns-form ns-depsu/should-check-ns-form?))]
           (if-not check?
             (do (println (str "Not checking " nsym 
                               (cond
                                 (not ns-form) " (ns form missing)"
                                 (ns-depsu/collect-only-ns? ns-form) " (tagged :collect-only in ns metadata)"
                                 (not (ns-depsu/requires-tc? ns-form)) " (does not depend on clojure.core.typed)")))
                 (flush))
             (let [start (. System (nanoTime))
                   asts (u/p :check/gen-analysis (ast-for-ns nsym))
                   _ (println "Start checking" nsym)
                   _ (flush)
                   casts (check-asts asts)
                   _ (assert (== (count casts) (count asts)))
                   _ (when-let [checked-asts vs/*checked-asts*]
                       (swap! checked-asts assoc nsym casts))
                   _ (println "Checked" nsym "in" (/ (double (- (. System (nanoTime)) start)) 1000000.0) "msecs")
                   _ (flush)
                   ]
         nil)))))))))

(defn find-updated-locals [env1 env2]
  {:pre [(map? env1)
         (map? env2)]}
  (set
    (filter identity
            (for [[k v1] env1]
              (when-let [v2 (get env2 k)]
                (when (and (sub/subtype? v1 v2)
                           (not (sub/subtype? v2 v1)))
                  k))))))
