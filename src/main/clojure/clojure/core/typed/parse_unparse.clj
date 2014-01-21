(ns ^:skip-wiki clojure.core.typed.parse-unparse
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as orep]
            [clojure.core.typed.path-rep :as pthrep]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.dvar-env :as dvar]
            [clojure.core.typed.filter-rep :as f]
            [clojure.core.typed.filter-ops :as fl]
            [clojure.core.typed.constant-type :as const]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.name-env :as name-env]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result AnyValue
                                        HeterogeneousSeq KwArgsSeq TCError Extends NumberCLJS BooleanCLJS
                                        IntegerCLJS ArrayCLJS JSNominal StringCLJS TCResult AssocType
                                        GetType)
           (clojure.core.typed.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE CountPE ClassPE KeysPE ValsPE)
           (clojure.lang ISeq Cons IPersistentList Symbol IPersistentVector PersistentHashMap)))

(alter-meta! *ns* assoc :skip-wiki true)

(defonce ^:dynamic *parse-type-in-ns* nil)
(set-validator! #'*parse-type-in-ns* (some-fn nil? symbol?))

(declare unparse-type unparse-filter unparse-filter-set unparse-flow-set)

; Types print by unparsing them
(do (defmethod print-method clojure.core.typed.impl_protocols.TCType [s writer]
      (print-method (unparse-type s) writer))
    (prefer-method print-method clojure.core.typed.impl_protocols.TCType clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.impl_protocols.TCType java.util.Map)
    (prefer-method print-method clojure.core.typed.impl_protocols.TCType clojure.lang.IPersistentMap)

    (defmethod print-method clojure.core.typed.impl_protocols.TCAnyType [s writer]
      (print-method (unparse-type s) writer))
    (prefer-method print-method clojure.core.typed.impl_protocols.TCAnyType clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.impl_protocols.TCAnyType java.util.Map)
    (prefer-method print-method clojure.core.typed.impl_protocols.TCAnyType clojure.lang.IPersistentMap)

    (defmethod print-method clojure.core.typed.impl_protocols.IFilter [s writer]
      (cond 
        (f/FilterSet? s) (print-method (unparse-filter-set s) writer)
        (r/FlowSet? s) (print-method (unparse-flow-set s) writer)
        :else (print-method (unparse-filter s) writer)))
    (prefer-method print-method clojure.core.typed.impl_protocols.IFilter clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.impl_protocols.IFilter java.util.Map)
    (prefer-method print-method clojure.core.typed.impl_protocols.IFilter clojure.lang.IPersistentMap))

(defmacro with-parse-ns [sym & body]
  `(binding [*parse-type-in-ns* ~sym]
     ~@body))

(declare parse-type)

(defn parse-clj [s]
  (impl/with-clojure-impl
    (parse-type s)))

(defn parse-cljs [s]
  (impl/with-cljs-impl
    (parse-type s)))

(defmulti parse-type class)
(defmulti parse-type-list first)

(def parsed-free-map? (u/hmap-c? :fname symbol?
                                 :bnd r/Bounds?
                                 :variance r/variance?))

; parsing TFn, protocol, RClass binders
(defn ^:private parse-free-with-variance [f]
  {:post [(parsed-free-map? %)]}
  (if (symbol? f)
    {:fname f
     :bnd r/no-bounds
     :variance :invariant}
    (let [[n & {:keys [< > variance] :as opts}] f]
      (when (contains? opts :kind)
        (prn "DEPRECATED: kind annotation for TFn parameters"))
      (when-not (r/variance? variance)
        (u/int-error (str "Invalid variance " (pr-str variance) " in free binder: " f)))
      {:fname n 
       :bnd (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))
       :variance variance})))

(defn parse-free-binder-with-variance [binder]
  {:post [(every? parsed-free-map? %)]}
  (reduce (fn [fs fsyn]
            {:pre [(every? parsed-free-map? fs)]
             :post [(every? parsed-free-map? %)]}
            ;(prn "parse-free-binder-with-variance" (map :fname fs))
            (conj fs
                  (free-ops/with-bounded-frees 
                    (zipmap (map (comp r/make-F :fname) fs)
                            (map :bnd fs))
                    (parse-free-with-variance fsyn))))
          [] binder))

; parsing All binders
;return a vector of [name bnds]
(defn parse-free [f]
  {:post [((u/hvector-c? symbol? r/Bounds?) %)]}
  (if (symbol? f)
    [f r/no-bounds]
    (let [[n & {:keys [< >] :as opts}] f]
      (when (contains? opts :kind)
        (prn "DEPRECATED: kind annotation for TFn parameters"))
      (when (:variance opts) 
        (u/int-error "Variance not supported for variables introduced with All"))
      [n (let [upper-or-nil (when (contains? opts :<)
                              (parse-type <))
               lower-or-nil (when (contains? opts :>)
                              (parse-type >))]
           (c/infer-bounds upper-or-nil lower-or-nil))])))

(defn check-forbidden-rec [rec tbody]
  (letfn [(well-formed? [t]
            (and (not= rec t)
                 (if ((some-fn r/Intersection? r/Union?) t)
                   (every? well-formed? (:types t))
                   true)))]
    (when-not (well-formed? tbody)
      (u/int-error (str "Recursive type not allowed here")))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [Mu* @(Mu*-var)
        _ (when-not (= 1 (count bnder)) 
            (u/int-error "Only one variable allowed: Rec"))
        f (r/make-F free-symbol)
        body (free-ops/with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

;(defmethod parse-type-list 'DottedPretype
;  [[_ psyn bsyn]]
;  (let [df (dvar/*dotted-scope* bsyn)]
;    (assert df bsyn)
;    (r/DottedPretype1-maker (free-ops/with-frees [df]
;                         (parse-type psyn))
;                       (:name (dvar/*dotted-scope* bsyn)))))

(defmethod parse-type-list 'CountRange
  [[_ n u]]
  (r/make-CountRange n u))

(defmethod parse-type-list 'ExactCount
  [[_ n]]
  (r/make-ExactCountRange n))

(defn- RClass-of-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'RClass-of)]
    (assert (var? v) "RClass-of unbound")
    v))

(defmethod parse-type-list 'predicate
  [[_ t-syn]]
  (let [RClass-of @(RClass-of-var)
        on-type (parse-type t-syn)]
    (r/make-FnIntersection
      (r/make-Function [r/-any] (RClass-of Boolean) nil nil
                       :filter (fl/-FS (fl/-filter on-type 0)
                                       (fl/-not-filter on-type 0))))))

(defmethod parse-type-list 'Not
  [[_ tsyn :as all]]
  (when-not (= (count all) 2) 
    (u/int-error (str "Wrong arguments to Not (expected 1): " all)))
  (r/NotType-maker (parse-type tsyn)))

(defmethod parse-type-list 'Rec
  [syn]
  (parse-rec-type syn))

(defmethod parse-type-list 'Assoc
  [[_ tsyn & entries :as all]]
  (when-not (and (<= 1 (count (next all)))
                 (even? (count entries)))
    (u/int-error (str "Wrong arguments to Assoc: " all)))
  (r/AssocType-maker (parse-type tsyn)
                     (doall (->> entries 
                                 (map parse-type)
                                 (partition 2)
                                 (map vec)))
                     nil))

(defmethod parse-type-list 'Get
  [[_ tsyn keysyn & not-foundsyn :as all]]
  (when-not (#{2 3} (count (next all)))
    (u/int-error (str "Wrong arguments to Get: " all)))
  (r/-get (parse-type tsyn)
          (parse-type keysyn)
          :not-found
          (when (#{3} (count (next all)))
            (parse-type not-foundsyn))))

;dispatch on last element of syntax in binder
(defmulti parse-all-type (fn [bnds type] (last bnds)))

;(All [a b ...] type)
(defmethod parse-all-type '...
  [bnds type]
  (let [frees-with-bnds (reduce (fn [fs fsyn]
                                  {:pre [(vector? fs)]
                                   :post [(every? (u/hvector-c? symbol? r/Bounds?) %)]}
                                  (conj fs
                                        (free-ops/with-bounded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) fs))
                                          (parse-free fsyn))))
                                [] (-> bnds butlast butlast))
        dvar (parse-free (-> bnds butlast last))]
    (free-ops/with-bounded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds))
      (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                   (map second (concat frees-with-bnds [dvar]))
                     (dvar/with-dotted [(r/make-F (first dvar))]
                       (parse-type type))))))

;(All [a b] type)
(defmethod parse-all-type :default
  [bnds type]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (u/hvector-c? symbol? r/Bounds?) %)]}
                  (conj fs
                        (free-ops/with-bounded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) fs))
                          (parse-free fsyn))))
                [] bnds)]
    (free-ops/with-bounded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds))
      (c/Poly* (map first frees-with-bnds)
               (map second frees-with-bnds)
               (parse-type type)))))

(defmethod parse-type-list 'Extends
  [[_ extends & {:keys [without] :as opts} :as syn]]
  (when-not (empty? (set/difference (set (keys opts)) #{:without}))
    (u/int-error (str "Invalid options to Extends:" (keys opts))))
  (when-not (vector? extends) 
    (u/int-error (str "Extends takes a vector of types: " (pr-str syn))))
  (c/-extends (doall (map parse-type extends))
              :without (doall (map parse-type without))))

(defmethod parse-type-list 'All
  [[All bnds syn & more :as all]]
  ;(prn "All syntax" all)
  (when-not (not more) 
    (u/int-error (str "Bad All syntax: " all)))
  (parse-all-type bnds syn))

(defn parse-union-type [[u & types]]
  (c/make-Union (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

; don't do any simplification of the intersection because some types might
; not be resolved
(defn parse-intersection-type [[i & types]]
  (c/make-Intersection (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defmethod parse-type-list 'Array
  [[_ syn & none]]
  (when-not (empty? none) 
    (u/int-error "Expected 1 argument to Array"))
  (let [t (parse-type syn)]
    (impl/impl-case
      :clojure (let [jtype (if (r/RClass? t)
                             (r/RClass->Class t)
                             Object)]
                 (r/PrimitiveArray-maker jtype t t))
      :cljs (r/ArrayCLJS-maker t t))))

(defmethod parse-type-list 'ReadOnlyArray
  [[_ osyn & none]]
  (when-not (empty? none) 
    (u/int-error "Expected 1 argument to ReadOnlyArray"))
  (let [o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object (r/Bottom) o)
      :cljs (r/ArrayCLJS-maker (r/Bottom) o))))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none]]
  (when-not (empty? none) 
    (u/int-error "Expected 2 arguments to Array2"))
  (let [i (parse-type isyn)
        o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object i o)
      :cljs (r/ArrayCLJS-maker i o))))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (when-not (empty? none) 
    (u/int-error "Expected 3 arguments to Array3"))
  (impl/assert-clojure)
  (let [jrclass (c/fully-resolve-type (parse-type jsyn))
        _ (when-not (r/RClass? jrclass) 
            (u/int-error "First argument to Array3 must be a Class"))]
    (r/PrimitiveArray-maker (r/RClass->Class jrclass) (parse-type isyn) (parse-type osyn))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply r/make-FnIntersection (mapv parse-function types)))

(defmethod parse-type-list 'Fn
  [[_ & types :as syn]]
  (when-not (seq types) 
    (u/int-error "Must pass at least one arity to Fn: " (pr-str syn)))
  (when-not (every? vector? types) 
    (u/int-error (str "Fn accepts vectors, given: " (pr-str syn))))
  (parse-fn-intersection-type syn))

(defn parse-free-binder [[nme & {:keys [variance < > kind] :as opts}]]
  (when-not (symbol? nme)
    (u/int-error "First entry in free binder should be a name symbol"))
  {:nme nme :variance (or variance :invariant)
   :bound (r/Bounds-maker
            ;upper
            (when-not kind
              (if (contains? opts :<)
                (parse-type <)
                r/-any))
            ;lower
            (when-not kind
              (if (contains? opts :>)
                (parse-type >)
                r/-nothing))
            ;kind
            (when kind
              (parse-type kind)))})

(defn parse-tfn-binder [[nme & opts-flat :as all]]
  {:pre [(vector? all)]
   :post [((u/hmap-c? :nme symbol? :variance r/variance?
                      :bound r/Bounds?) %)]}
  (let [_ (when-not (even? (count opts-flat))
            (u/int-error (str "Uneven arguments passed to TFn binder: "
                              (pr-str all))))
        {:keys [variance < >] 
         :or {variance :inferred}
         :as opts} 
        (apply hash-map opts-flat)]
    (when-not (symbol? nme)
      (u/int-error "Must provide a name symbol to TFn"))
    (when (contains? opts :kind)
      (u/int-error "DEPRECATED: kind annotation for TFn parameters"))
    (when-not (r/variance? variance)
      (u/int-error (str "Invalid variance: " (pr-str variance))))
    {:nme nme :variance variance
     :bound (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))}))

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (when-not (= 3 (count tfn))
    (u/int-error "Wrong number of arguments to TFn: " (pr-str tfn)))
  (when-not (every? vector? binder)
    (u/int-error "TFn binder should be vector of vectors: " (pr-str tfn)))
  (let [; don't scope a free in its own bounds. Should review this decision
        free-maps (free-ops/with-free-symbols (map (fn [s]
                                                     {:pre [(vector? s)]
                                                      :post [(symbol? %)]}
                                                     (first s))
                                                   binder)
                    (mapv parse-tfn-binder binder))
        bodyt (free-ops/with-bounded-frees (into {}
                                                 (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
                                                      free-maps))
                (parse-type bodysyn))
        ; We check variances lazily in TypeFn-body*. This avoids any weird issues with calculating
        ; variances with potentially partially defined types.
        ;vs (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
        ;                                     free-maps)
        ;     (frees/fv-variances bodyt))
        ;_ (doseq [{:keys [nme variance]} free-maps]
        ;    (when-let [actual-v (vs nme)]
        ;      (when-not (= (vs nme) variance)
        ;        (u/int-error (str "Type variable " nme " appears in " (name actual-v) " position "
        ;                          "when declared " (name variance))))))
        ]
    (c/TypeFn* (map :nme free-maps) (map :variance free-maps)
               (map :bound free-maps) bodyt)))

(defmethod parse-type-list 'TFn
  [syn]
  (parse-type-fn syn))

(declare parse-quoted-hvec)

(defmethod parse-type-list 'Seq* [syn] (r/HeterogeneousSeq-maker (mapv parse-type (rest syn))))
(defmethod parse-type-list 'List* [syn] (r/HeterogeneousList-maker (mapv parse-type (rest syn))))
(defmethod parse-type-list 'Vector* [syn] (parse-quoted-hvec (rest syn)))

(declare parse-hvec-types parse-object parse-filter-set)

(defmethod parse-type-list 'HVec 
  [[_ syn & {:keys [filter-sets objects]}]]
  (let [{:keys [fixed drest rest]} (parse-hvec-types syn)]
    (r/-hvec fixed
             :filters (when filter-sets
                        (mapv parse-filter-set filter-sets))
             :objects (when objects
                        (mapv parse-object objects))
             :drest drest
             :rest rest)))

(defn parse-hvec-types [syns]
  (let [rest? (#{'*} (last syns))
        dotted? (#{'...} (-> syns butlast last))
        _ (when (and rest? dotted?)
            (u/int-error (str "Invalid heterogeneous vector syntax:" syns)))
        {:keys [fixed rest drest]}
        (cond
          rest?
          (let [fixed (mapv parse-type (drop-last 2 syns))
                rest (parse-type (-> syns butlast last))]
            {:fixed fixed
             :rest rest})
          dotted?
          (let [fixed (mapv parse-type (drop-last 3 syns))
                [drest-bnd _dots_ drest-type] (take-last 3 syns)
                bnd (dvar/*dotted-scope* drest-bnd)
                _ (when-not bnd 
                    (u/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
            {:fixed fixed
             :drest (r/DottedPretype1-maker
                      (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                        (parse-type drest-type))
                      (:name bnd))})
          :else {:fixed (mapv parse-type syns)})]
    {:fixed fixed
     :rest rest
     :drest drest}))



(defn- syn-to-hmap [mandatory optional absent-keys complete?]
  (when mandatory
    (when-not (map? mandatory)
      (u/int-error (str "Mandatory entries to HMap must be a map: " mandatory))))
  (when optional
    (when-not (map? optional)
      (u/int-error (str "Optional entries to HMap must be a map: " optional))))
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(r/-val k)
                        (parse-type v)])))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (u/int-error (str "HMap options contain duplicate key entries: "
                                "Mandatory: " (into {} mandatory) ", Optional: " (into {} optional) 
                                ", Absent: " (set absent-keys))))
          _ (when-not (every? keyword? (keys mandatory)) (u/int-error "HMap's mandatory keys must be keywords"))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (u/int-error "HMap's optional keys must be keywords"))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (u/int-error "HMap's absent keys must be keywords"))
          absent-keys (set (map r/-val absent-keys))]
      (c/make-HMap mandatory optional complete? :absent-keys absent-keys))))

(defn parse-quoted-hvec [syn]
  (let [{:keys [fixed drest rest]} (parse-hvec-types syn)]
    (r/-hvec fixed
             :drest drest
             :rest rest)))

(defmethod parse-type-list 'quote 
  [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol?) syn) (r/-val syn)
    (vector? syn) (parse-quoted-hvec syn)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false)
    :else (u/int-error (str "Invalid use of quote:" (pr-str syn)))))

(declare parse-in-ns)

(defn multi-frequencies 
  "Like frequencies, but only returns frequencies greater
  than one"
  [coll]
  (->> coll
       frequencies
       (filter (fn [[k freq]]
                 (when (< 1 freq)
                   true)))
       (into {})))

(defmethod parse-type-list 'HMap
  [[_HMap_ & flat-opts :as all]]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (println 
                                 (parse-in-ns)
                                 ": DEPRECATED: HMap syntax changed. Use :mandatory keyword argument instead of initial map")
                               (flush)
                               (first flat-opts))
        flat-opts (if deprecated-mandatory
                    (next flat-opts)
                    flat-opts)
        _ (when-not (even? (count flat-opts))
            (u/int-error (str "Uneven keyword arguments to HMap: " (pr-str all))))
        flat-keys (->> flat-opts
                       (partition 2)
                       (map first))
        _ (when-not (every? keyword? flat-keys)
            (u/int-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                              " in: " (pr-str all))))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (u/int-error (str "Repeated keyword argument to HMap: " (pr-str k)))))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (u/int-error (str "Unsupported HMap keyword argument: " (pr-str k))))
        _ (when (and deprecated-mandatory mandatory)
            (u/int-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete?)))

(defn- parse-in-ns []
  {:post [(symbol? %)]}
  (or *parse-type-in-ns* 
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (ucljs/cljs-ns))))

(defn- resolve-type-clj 
  "Returns a var, class or nil"
  [sym]
  {:pre [(symbol? sym)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-let [ns (find-ns nsym)]
      (ns-resolve ns sym)
      (u/int-error (str "Cannot find namespace: " sym)))))

(defn- resolve-type-cljs 
  "Returns a var map of {:ns sym :name sym} or nil"
  [sym]
  {:pre [(symbol? sym)]}
  (impl/assert-cljs)
  (let [nsym (parse-in-ns)]
    (ucljs/resolve-var nsym sym)))

(defn parse-RClass [cls-sym params-syn]
  (impl/assert-clojure)
  (let [RClass-of @(RClass-of-var)
        cls (resolve-type-clj cls-sym)
        _ (when-not (class? cls) (u/int-error (str (pr-str cls-sym) " cannot be resolved")))
        tparams (doall (map parse-type params-syn))]
    (RClass-of cls tparams)))

(defmethod parse-type-list 'Value
  [[_Value_ syn :as all]]
  (when-not (#{2} (count all))
    (u/int-error (str "Incorrect number of arguments to Value, " (count all)
                      ", expected 2: " all)))
  (impl/impl-case
    :clojure (const/constant-type syn)
    :cljs (assert nil "FIXME CLJS parse Value")))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (when-not (= #{}
               (set/intersection (set (keys optional))
                                 (set (keys mandatory))))
    (u/int-error (str "Optional and mandatory keyword arguments should be disjoint: "
                      (set/intersection (set (keys optional))
                                        (set (keys mandatory))))))
  (let [optional (into {} (for [[k v] optional]
                            (do (when-not (keyword? k) (u/int-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                              [(r/-val k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (when-not (keyword? k)) (u/int-error (str "Keyword argument keys must be keywords: " (pr-str k)))
                               [(r/-val k) (parse-type v)])))]
    (apply c/Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (r/HeterogeneousSeq-maker (mapcat #(find m %) ks)))))))))

(declare unparse-type deprecated-list)

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (if-let [d (deprecated-list syn)]
    d
    (let [op (parse-type n)]
      (when-not ((some-fn r/Name? r/TypeFn? r/F? r/B? r/Poly?) op)
        (u/int-error (str "Invalid operator to type application: " syn)))
      (with-meta (r/TApp-maker op (mapv parse-type args))
                 {:syn syn
                  :env vs/*current-env*}))))

(defmethod parse-type Cons [l] (parse-type-list l))
(defmethod parse-type IPersistentList [l] (parse-type-list l))

(defmulti parse-type-symbol identity)
(defmethod parse-type-symbol 'Any [_] r/-any)
(defmethod parse-type-symbol 'Nothing [_] (r/Bottom))
(defmethod parse-type-symbol 'AnyFunction [_] (r/TopFunction-maker))

(defn clj-primitives-fn []
  (let [RClass-of @(RClass-of-var)]
    {'byte (RClass-of 'byte)
     'short (RClass-of 'short)
     'int (RClass-of 'int)
     'long (RClass-of 'long)
     'float (RClass-of 'float)
     'double (RClass-of 'double)
     'boolean (RClass-of 'boolean)
     'char (RClass-of 'char)
     'void r/-nil}))

(defn cljs-primitives-fn []
  {'number (r/NumberCLJS-maker)
   'int (r/IntegerCLJS-maker)
   'boolean (r/BooleanCLJS-maker)
   'object (r/ObjectCLJS-maker)
   'string (r/StringCLJS-maker)})

;[Any -> (U nil Type)]
(defmulti deprecated-clj-symbol identity)

(defmethod deprecated-clj-symbol :default [_] nil)

;[Any -> (U nil Type)]
(defn deprecated-symbol [sym]
  {:post [((some-fn nil? r/Type?) %)]}
  (impl/impl-case
    :clojure (deprecated-clj-symbol sym)
    :cljs nil))

;[Any -> (U nil Type)]
(defmulti deprecated-clj-list 
  (fn [[op]]
    (when (symbol? op)
      ((some-fn
         (every-pred
           class? u/Class->symbol)
         (every-pred
           var? u/var->symbol))
       (resolve-type-clj op)))))

(defmethod deprecated-clj-list :default [_] nil)

;[Any -> (U nil Type)]
(defn deprecated-list [lst]
  {:post [((some-fn nil? r/Type?) %)]}
  (impl/impl-case
    :clojure (deprecated-clj-list lst)
    :cljs nil))

(defmethod parse-type-symbol :default
  [sym]
  (let [primitives (impl/impl-case
                     :clojure (clj-primitives-fn)
                     :cljs (cljs-primitives-fn))
        rsym (impl/impl-case
               :clojure (when-let [res (when (symbol? sym)
                                         (resolve-type-clj sym))]
                          (cond 
                            (class? res) (u/Class->symbol res)
                            (var? res) (u/var->symbol res)))
               :cljs (when-let [res (when (symbol? sym)
                                      (resolve-type-cljs sym))]
                       (:name res)))
        free (when (symbol? sym) 
               (free-ops/free-in-scope sym))
        _ (assert ((some-fn symbol? nil?) rsym))]
    (cond
      free free
      (primitives sym) (primitives sym)
      rsym ((some-fn deprecated-symbol r/Name-maker) rsym)
      :else (u/tc-error (str "Cannot resolve type: " (pr-str sym)
                             "\nHint: Is " (pr-str sym) " in scope?"
                             "\nHint: Has " (pr-str sym) "'s annotation been"
                             " found via check-ns, cf or typed-deps?")))))

(defmethod parse-type Symbol [l] (parse-type-symbol l))
(defmethod parse-type Boolean [v] (if v r/-true r/-false)) 
(defmethod parse-type nil [_] r/-nil)

(declare parse-path-elem parse-filter*)

(defn parse-filter [f]
  (cond
    (= 'tt f) f/-top
    (= 'ff f) f/-bot
    (not ((some-fn seq? list?) f)) (u/int-error (str "Malformed filter expression: " (pr-str f)))
    :else (parse-filter* f)))

(defn parse-object [{:keys [id path]}]
  (when-not (f/name-ref? id)
    (u/int-error (str "Must pass natural number or symbol as id: " (pr-str id))))
  (orep/->Path (when path (mapv parse-path-elem path)) id))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (fl/-FS (if then
            (parse-filter then)
            f/-top)
          (if else
            (parse-filter else)
            f/-top)))

(defmulti parse-filter* 
  (every-pred coll? first))

(defmethod parse-filter* :default
  [syn]
  (u/int-error (str "Malformed filter expression: " (pr-str syn))))

(defmethod parse-filter* 'is
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (u/int-error (str "Wrong number of arguments to is")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-filter t nme p)))

(defmethod parse-filter* '!
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (u/int-error (str "Wrong number of arguments to !")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-not-filter t nme p)))

(defmethod parse-filter* '|
  [[_ & fsyns]]
  (apply fl/-or (mapv parse-filter fsyns)))

(defmethod parse-filter* '&
  [[_ & fsyns]]
  (apply fl/-and (mapv parse-filter fsyns)))

(defmethod parse-filter* 'when
  [[_ & [a c :as args] :as all]]
  (when-not (#{2} (count args))
    (u/int-error (str "Wrong number of arguments to when: " all)))
  (fl/-imp (parse-filter a) (parse-filter c)))

;FIXME clean up the magic. eg. handle (Class foo bar) as an error
(defmulti parse-path-elem 
  #(cond
     (symbol? %) %
     (coll? %) (first %)
     :else 
       (u/int-error (str "Malformed path element: " (pr-str %)))))

(defmethod parse-path-elem :default [syn]
  (u/int-error (str "Malformed path element: " (pr-str syn))))

(defmethod parse-path-elem 'Class [_] (pthrep/->ClassPE))
(defmethod parse-path-elem 'Count [_] (pthrep/->CountPE))

(defmethod parse-path-elem 'Keys [_] (pthrep/->KeysPE))
(defmethod parse-path-elem 'Vals [_] (pthrep/->ValsPE))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (when-not (= 1 (count all))
    (u/int-error "Wrong arguments to Key"))
  (pthrep/->KeyPE ksyn))

(defn- parse-kw-map [m]
  {:post [((u/hash-c? r/Value? r/Type?) %)]}
  (into {} (for [[k v] m]
             [(r/-val k) (parse-type v)])))

(defn parse-function [f]
  {:post [(r/Function? %)]}
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts-flat :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
        _ (when-not (<= 2 (count chk)) 
            (u/int-error (str "Incorrect function syntax: " f)))

        _ (when-not (even? (count opts-flat)) 
            (u/int-error (str "Incorrect function syntax, must have even number of keyword parameters: " f)))

        opts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*
         ampersand-pos '&}
        (zipmap all-dom (range))

        _ (when-not (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos])))
            (u/int-error "Can only provide one rest argument option: & ... or *"))

        _ (when-let [ks (seq (remove #{:filters :object :flow} (keys opts)))]
            (u/int-error (str "Invalid function keyword option/s: " ks)))

        filters (when-let [[_ fsyn] (find opts :filters)]
                  (parse-filter-set fsyn))

        object (when-let [[_ obj] (find opts :object)]
                 (parse-object obj))

        flow (when-let [[_ obj] (find opts :flow)]
               (r/-flow (parse-filter obj)))

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    ampersand-pos (take ampersand-pos all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        _ (when-not (or (not asterix-pos)
                        (= (count all-dom) (inc asterix-pos)))
            (u/int-error (str "Trailing syntax after rest parameter: " (pr-str (drop (inc asterix-pos) all-dom)))))
        [drest-type _ drest-bnd :as drest-seq] (when ellipsis-pos
                                                 (drop (dec ellipsis-pos) all-dom))
        _ (when-not (or (not ellipsis-pos) (= 3 (count drest-seq))) 
            (u/int-error "Dotted rest entry must be 3 entries"))
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (u/int-error "Dotted bound must be symbol"))
        [& {optional-kws :optional mandatory-kws :mandatory} :as kws-seq]
        (let [kwsyn (when ampersand-pos
                      (drop (inc ampersand-pos) all-dom))]
          ; support deprecated syntax [& {} -> ] to be equivalent to [& :optional {} -> ]
          (if (and kwsyn
                   (map? (first kwsyn)))
            (do (prn "DEPRECATED: implicit optional parameters for Fn arity. Use :optional keyword argument beteween & and ->.")
                (cons :optional kwsyn))
            kwsyn))

        _ (when-not (or (not ampersand-pos) (seq kws-seq)) 
            (u/int-error "Must provide syntax after &"))]
    (r/make-Function (mapv parse-type fixed-dom)
                     (parse-type rng)
                     (when asterix-pos
                       (parse-type rest-type))
                     (when ellipsis-pos
                       (let [bnd (dvar/*dotted-scope* drest-bnd)
                             _ (when-not bnd 
                                 (u/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type drest-type))
                           (:name bnd))))
                     :filter filters
                     :object object
                     :flow flow
                     :optional-kws (when optional-kws
                                     (parse-kw-map optional-kws))
                     :mandatory-kws (when mandatory-kws
                                      (parse-kw-map mandatory-kws)))))

(defmethod parse-type IPersistentVector
  [f]
  (apply r/make-FnIntersection [(parse-function f)]))

(defmethod parse-type :default
  [k]
  (u/tc-error (str "Bad type syntax: " (pr-str k)
                   (when ((some-fn symbol? keyword?) k)
                     (str "\n\nHint: Value types should be preceded by a quote or wrapped in the Value constructor."  
                          " eg. '" (pr-str k) " or (Value " (pr-str k)")")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse

(defonce ^:dynamic *unparse-type-in-ns* nil)
(set-validator! #'*unparse-type-in-ns* (some-fn nil? symbol?))

(defmacro with-unparse-ns [sym & body]
  `(binding [*unparse-type-in-ns* ~sym]
     ~@body))

(defn alias-in-ns
  "Returns an alias for namespace sym in ns, or nil if none."
  [nsym ns]
  (impl/assert-clojure)
  (some (fn [[alias ans]]
          (when (= (str nsym) (str (ns-name ans)))
            alias))
        (ns-aliases ns)))

(defn core-lang-Class-sym [clsym]
  (when (.startsWith (str clsym) "clojure.lang.")
    (symbol (.getSimpleName (Class/forName (str clsym))))))

(defn Class-symbol-intern [clsym ns]
  {:pre [(u/namespace? ns)]}
  (some (fn [[isym cls]]
          (when (= (str clsym) (str (u/Class->symbol cls)))
            isym))
        (ns-imports ns)))

(defn var-symbol-intern 
  "Returns a symbol interned in ns for var symbol, or nil if none.

  (var-symbol-intern 'symbol (find-ns 'clojure.core))
  ;=> 'symbol
  (var-symbol-intern 'bar (find-ns 'clojure.core))
  ;=> nil"
  [sym ns]
  {:pre [(symbol? sym)
         (u/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (some (fn [[isym var]]
          (when (= (str sym) (str (u/var->symbol var)))
            isym))
        (merge (ns-interns ns)
               (ns-refers ns))))

(defn unparse-Class-symbol-in-ns [sym]
  (if-let [ns (and (not clojure.core.typed/*verbose-types*)
                   (when-let [nsym *unparse-type-in-ns*]
                     (find-ns *unparse-type-in-ns*)))]
        ; use an import name
    (or (Class-symbol-intern sym ns)
        ; core.lang classes are special
        (core-lang-Class-sym sym)
        ; otherwise use fully qualified name
        sym)
    sym))

(defn unparse-var-symbol-in-ns [sym]
  {:pre [(namespace sym)]}
  (if-let [ns (and (not clojure.core.typed/*verbose-types*)
                   (when-let [nsym *unparse-type-in-ns*]
                     (find-ns nsym)))]
        ; use unqualified name if interned
    (or (var-symbol-intern sym ns)
        ; use aliased ns if not interned, but ns is aliased
        (when-let [alias (alias-in-ns (namespace sym) ns)]
          (symbol (str alias) (name sym)))
        ; otherwise use fully qualified name
        sym)
    sym))

(declare unparse-type* unparse-object unparse-filter-set unparse-filter)

(defn unparse-type [t]
  ; quick way of giving a Name that the user is familiar with
  (if-let [nsym (-> t meta :source-Name)]
    nsym
    (unparse-type* t)))

(defmulti unparse-type* class)
(defn unp [t] (prn (unparse-type t)))

(defmethod unparse-type* Top [_] 'Any)
(defmethod unparse-type* TCError [_] 'Error)
(defmethod unparse-type* Name [{:keys [id]}] (if (namespace id)
                                               (unparse-var-symbol-in-ns id)
                                               id))
(defmethod unparse-type* AnyValue [_] 'AnyValue)

(defmethod unparse-type* DottedPretype
  [{:keys [pre-type name]}]
  (list 'DottedPretype (unparse-type pre-type) name))

(defmethod unparse-type* CountRange [{:keys [lower upper]}]
  (cond
    (= lower upper) (list 'ExactCount lower)
    :else (list* 'CountRange lower (when upper [upper]))))

(defmethod unparse-type* App 
  [{:keys [rator rands]}]
  (list* (unparse-type rator) (mapv unparse-type rands)))

(defmethod unparse-type* TApp 
  [{:keys [rator rands] :as tapp}]
  (cond 
    ;perform substitution if obvious
    ;(TypeFn? rator) (unparse-type (resolve-tapp tapp))
    :else
    (list* (unparse-type rator) (mapv unparse-type rands))))

(defmethod unparse-type* Result
  [{:keys [t]}]
  (unparse-type t))

(defmethod unparse-type* F
  [{:keys [] :as f}]
  ; Note: don't print f here, results in infinite recursion
  ;(prn (-> f :name) (-> f :name meta))
  (r/F-original-name f))

(defmethod unparse-type* PrimitiveArray
  [{:keys [jtype input-type output-type]}]
  (cond 
    (and (= input-type output-type)
         (= Object jtype))
    (list 'Array (unparse-type input-type))

    (= Object jtype)
    (list 'Array2 (unparse-type input-type) (unparse-type output-type))

    :else
    (list 'Array3 (u/Class->symbol jtype)
          (unparse-type input-type) (unparse-type output-type))))

(defmethod unparse-type* B
  [{:keys [idx]}]
  (list 'B idx))

(defmethod unparse-type* Union
  [{types :types :as u}]
  (cond
    ; Prefer the user provided Name for this type. Needs more thinking?
    ;(-> u meta :from-name) (-> u meta :from-name)
    (seq types) (list* 'U (doall (map unparse-type types)))
    :else 'Nothing))

(defmethod unparse-type* FnIntersection
  [{types :types}]
  (list* 'Fn (doall (map unparse-type types))))

(defmethod unparse-type* Intersection
  [{types :types}]
  (list* 'I (doall (map unparse-type types))))

(defmethod unparse-type* NotType
  [{:keys [type]}]
  (list 'Not (unparse-type type)))

(defmethod unparse-type* TopFunction [_] 'AnyFunction)

(defn- unparse-kw-map [m]
  {:pre [((u/hash-c? r/Value? r/Type?) m)]}
  (into {} (for [[^Value k v] m] 
             [(.val k) (unparse-type v)])))

(defn unparse-result [{:keys [t fl o] :as rng}]
  {:pre [(r/Result? rng)]}
  (concat [(unparse-type t)]
          (when (not (and ((some-fn f/TopFilter? f/BotFilter?) (:then fl))
                          ((some-fn f/TopFilter? f/BotFilter?) (:else fl))))
            [:filters (unparse-filter-set fl)])
          (when (not ((some-fn orep/NoObject? orep/EmptyObject?) o))
            [:object (unparse-object o)])))

(defmethod unparse-type* Function
  [{:keys [dom rng kws rest drest]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 [(unparse-type rest) '*])
               (when drest
                 (let [{:keys [pre-type name]} drest]
                   [(unparse-type pre-type) 
                    '... 
                    (-> name r/make-F r/F-original-name)]))
               (when kws
                 (let [{:keys [optional mandatory]} kws]
                   (list* '& 
                          (unparse-kw-map optional)
                          (when (seq mandatory) 
                            [:mandatory (unparse-kw-map mandatory)]))))
               ['->]
               (unparse-result rng))))

(defn unparse-flow-set [flow]
  {:pre [(r/FlowSet? flow)]}
  (unparse-filter (r/flow-normal flow)))

(defmethod unparse-type* Protocol
  [{:keys [the-var poly?]}]
  (let [s (impl/impl-case :clojure (unparse-var-symbol-in-ns the-var)
                          :cljs the-var)]
    (if poly?
      (list* s (mapv unparse-type poly?))
      s)))

(defmethod unparse-type* DataType
  [{:keys [the-class poly?]}]
  (if poly?
    (list* (unparse-Class-symbol-in-ns the-class) (mapv unparse-type poly?))
    (unparse-Class-symbol-in-ns the-class)))

(defmulti unparse-RClass :the-class)

(defmethod unparse-RClass 'clojure.lang.Atom
  [{:keys [the-class poly?]}]
  (let [[w r] poly?]
    (list* (unparse-Class-symbol-in-ns the-class) (map unparse-type (concat [w]
                                               (when (not= w r)
                                                 [r]))))))

(defmethod unparse-RClass :default
  [{:keys [the-class poly?]}]
  (list* (unparse-Class-symbol-in-ns the-class) (doall (map unparse-type poly?))))

(defmethod unparse-type* RClass
  [{:keys [the-class poly?] :as r}]
  (if (empty? poly?)
    (unparse-Class-symbol-in-ns the-class)
    (unparse-RClass r)))

(defmethod unparse-type* Mu
  [m]
  (let [nme (-> (c/Mu-fresh-symbol* m) r/make-F r/F-original-name)
        body (c/Mu-body* nme m)]
    (list 'Rec [nme] (unparse-type body))))

(defn unparse-poly-bounds-entry [name {:keys [upper-bound lower-bound higher-kind] :as bnds}]
  (let [name (-> name r/make-F r/F-original-name)
        u (when upper-bound 
            (unparse-type upper-bound))
        l (when lower-bound 
            (unparse-type lower-bound))
        h (when higher-kind
            (unparse-type higher-kind))]
    (or (when higher-kind
          [name :kind h])
        (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
          [name :< u :> l])
        (when-not (r/Top? upper-bound) 
          [name :< u])
        (when-not (r/Bottom? lower-bound)
          [name :> l])
        name)))

(defmethod unparse-type* PolyDots
  [{:keys [nbound] :as p}]
  (let [free-and-dotted-names (vec (c/PolyDots-fresh-symbols* p))
        ; ignore dotted bound for now
        bbnds (butlast (c/PolyDots-bbnds* free-and-dotted-names p))
        binder (vec (concat (map unparse-poly-bounds-entry 
                                 (butlast free-and-dotted-names) 
                                 bbnds)
                            [(last free-and-dotted-names) '...]))
        body (c/PolyDots-body* free-and-dotted-names p)]
    (list 'All binder (unparse-type body))))

(defmethod unparse-type* Extends
  [{:keys [extends without]}]
  (list* 'Extends
         (mapv unparse-type extends)
         (when (seq without)
           [:without (mapv unparse-type without)])))

(defmethod unparse-type* Poly
  [{:keys [nbound] :as p}]
  (let [free-names (c/Poly-fresh-symbols* p)
        ;_ (prn "Poly unparse" free-names (map meta free-names))
        bbnds (c/Poly-bbnds* free-names p)
        binder (mapv unparse-poly-bounds-entry free-names bbnds)
        body (c/Poly-body* free-names p)]
    (list 'All binder (unparse-type body))))

;(ann unparse-typefn-bounds-entry [Symbol Bounds Variance -> Any])
(defn unparse-typefn-bounds-entry [name {:keys [upper-bound lower-bound higher-kind]} v]
  (let [name (-> name r/make-F r/F-original-name)
        u (when upper-bound 
            (unparse-type upper-bound))
        l (when lower-bound 
            (unparse-type lower-bound))
        h (when higher-kind
            (unparse-type higher-kind))]
    (or (when higher-kind
          [name :variance v :kind h])
        (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
          [name :variance v :< u :> l])
        (when-not (r/Top? upper-bound) 
          [name :variance v :< u])
        (when-not (r/Bottom? lower-bound)
          [name :variance v :> l])
        [name :variance v])))

(defmethod unparse-type* TypeFn
  [{:keys [nbound] :as p}]
  (let [free-names (c/TypeFn-fresh-symbols* p)
        bbnds (c/TypeFn-bbnds* free-names p)
        binder (mapv unparse-typefn-bounds-entry free-names bbnds (:variances p))
        body (c/TypeFn-body* free-names p)]
    (list 'TFn binder (unparse-type body))))

(defmethod unparse-type* Value
  [v]
  (if ((some-fn r/Nil? r/True? r/False?) v)
    (:val v)
    (list 'Value (:val v))))

(defn- unparse-map-of-types [m]
  (into {} (map (fn [[k v]]
                  (assert (r/Value? k) k)
                  (vector (:val k) (unparse-type v)))
                m)))

(defmethod unparse-type* HeterogeneousMap
  [^HeterogeneousMap v]
  (list* 'HMap 
         (concat
           [:mandatory (unparse-map-of-types (.types v))]
           (when-let [ks (and (not (c/complete-hmap? v))
                              (seq (.absent-keys v)))]
             [:absent-keys (set (map :val ks))])
           (when (c/complete-hmap? v)
             [:complete? true]))))

(defmethod unparse-type* HeterogeneousSeq
  [v]
  (list* 'Seq* (doall (map unparse-type (:types v)))))

(defmethod unparse-type* KwArgsSeq
  [^KwArgsSeq v]
  (list* 'KwArgsSeq 
         (concat
           (when (seq (.optional v))
             [:optional (unparse-map-of-types (.optional v))])
           (when (seq (.mandatory v))
             [:mandatory (unparse-map-of-types (.mandatory v))]))))

(defmethod unparse-type* HeterogeneousVector
  [{:keys [types rest drest fs objects] :as v}]
  (list* 'HVec 
         (vec
           (concat
             (map unparse-type (:types v))
             (when rest [(unparse-type rest) '*])
             (when drest [(:name drest) '... (unparse-type (:pre-type drest))])))
         (concat
           (when-not (every? #{(fl/-FS f/-top f/-top)} fs)
             [:filter-sets (mapv unparse-filter-set fs)])
           (when-not (every? #{orep/-empty} objects)
             [:objects (mapv unparse-object objects)]))))

(defmethod unparse-type* HeterogeneousList
  [v]
  (list* 'List* (doall (map unparse-type (:types v)))))

(defmethod unparse-type* AssocType
  [{:keys [target entries dentries]}]
  (assert (not dentries) "dentries for Assoc NYI")
  (list* 'Assoc (unparse-type target) 
         (doall (map unparse-type (apply concat entries)))))

(defmethod unparse-type* GetType
  [{:keys [target key not-found]}]
  (list* 'Get (unparse-type target) 
         (unparse-type key)
         (when (not= r/-nil not-found)
           [(unparse-type not-found)])))

; CLJS Types

(defmethod unparse-type* NumberCLJS [_] 'number)
(defmethod unparse-type* BooleanCLJS [_] 'boolean)
(defmethod unparse-type* IntegerCLJS [_] 'int)
(defmethod unparse-type* StringCLJS [_] 'string)

(defmethod unparse-type* ArrayCLJS
  [{:keys [input-type output-type]}]
  (cond 
    (= input-type output-type) (list 'Array (unparse-type input-type))
    :else (list 'Array2 (unparse-type input-type) (unparse-type output-type))))

(defmethod unparse-type* JSNominal
  [{:keys [name poly?]}]
  (let [sym (symbol name)]
    (if (seq poly?)
      (list* sym (map unparse-type poly?))
      sym)))

; Objects

(declare unparse-path-elem)

(defmulti unparse-object class)
(defmethod unparse-object EmptyObject [_] 'empty-object)
(defmethod unparse-object NoObject [_] 'no-object)
(defmethod unparse-object Path [{:keys [path id]}] (conj {:id id} (when (seq path) [:path (mapv unparse-path-elem path)])))

; Path elems

(defmulti unparse-path-elem class)
(defmethod unparse-path-elem KeyPE [t] (list 'Key (:val t)))
(defmethod unparse-path-elem CountPE [t] 'Count)
(defmethod unparse-path-elem ClassPE [t] 'Class)
(defmethod unparse-path-elem KeysPE [t] 'Keys)
(defmethod unparse-path-elem ValsPE [t] 'Vals)

; Filters

(defmulti unparse-filter* class)

(declare unparse-filter)

(defn unparse-filter-set [{:keys [then else] :as fs}]
  {:pre [(f/FilterSet? fs)]}
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(defn unparse-filter [f]
  (unparse-filter* f))

(defmethod unparse-filter* TopFilter [f] 'tt)
(defmethod unparse-filter* BotFilter [f] 'ff)

(declare unparse-type)

(defmethod unparse-filter* TypeFilter
  [{:keys [type path id]}]
  (concat (list 'is (unparse-type type) id)
          (when (seq path)
            [(mapv unparse-path-elem path)])))

(defmethod unparse-filter* NotTypeFilter
  [{:keys [type path id]}]
  (concat (list '! (unparse-type type) id)
          (when (seq path)
            [(mapv unparse-path-elem path)])))

(defmethod unparse-filter* AndFilter [{:keys [fs]}] (apply list '& (map unparse-filter fs)))
(defmethod unparse-filter* OrFilter [{:keys [fs]}] (apply list '| (map unparse-filter fs)))

(defmethod unparse-filter* ImpFilter
  [{:keys [a c]}]
  (list 'when (unparse-filter a) (unparse-filter c)))

;[TCResult -> Any]
(defn unparse-TCResult [r]
  (let [t (unparse-type (r/ret-t r))
        fs (unparse-filter-set (r/ret-f r))
        o (unparse-object (r/ret-o r))]
    (if (and (= (fl/-FS f/-top f/-top) (r/ret-f r))
             (= (r/ret-o r) orep/-empty))
      t
      (if (= (r/ret-o r) orep/-empty)
        [t fs]
        [t fs o]))))

(defn unparse-TCResult-in-ns [r ns]
  {:pre [((some-fn u/namespace? symbol?) ns)]}
  (binding [*unparse-type-in-ns* (if (symbol? ns)
                                   ns
                                   (ns-name ns))]
    (unparse-TCResult r)))

(defmethod unparse-type* TCResult
  [v]
  (unparse-TCResult v))
