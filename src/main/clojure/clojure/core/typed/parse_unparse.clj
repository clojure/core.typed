(ns ^:skip-wiki clojure.core.typed.parse-unparse
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as orep]
            [clojure.core.typed.path-rep :as pthrep]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.dvar-env :as dvar]
            [clojure.core.typed.filter-rep :as f]
            [clojure.core.typed.filter-ops :as fl]
            [clojure.core.typed.constant-type :as const]
            [clojure.core.typed.datatype-env :as dtenv]
            [clojure.core.typed.protocol-env :as prenv]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.core.typed.name-env :as nmenv]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb]
            [cljs.analyzer :as cljs-ana])
  (:import (clojure.core.typed.type_rep NotType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousVector HeterogeneousList HeterogeneousMap
                                        CountRange Name Value Top TopFunction B F Result AnyValue
                                        HeterogeneousSeq KwArgsSeq TCError Extends NumberCLJS BooleanCLJS
                                        IntegerCLJS ArrayCLJS JSNominal StringCLJS)
           (clojure.core.typed.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)
           (clojure.core.typed.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.path_rep KeyPE CountPE ClassPE KeysPE ValsPE)
           (clojure.lang ISeq Cons IPersistentList Symbol IPersistentVector PersistentHashMap)))

(def ^:dynamic *parse-type-in-ns* nil)
(set-validator! #'*parse-type-in-ns* (some-fn nil? symbol?))

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

(declare find-lower-bound find-upper-bound infer-bounds)

; parsing TFn, protocol, RCLass binders
(defn parse-free-with-variance [f]
  {:post [(u/hvector-c? symbol? r/Bounds?)]}
  (if (symbol? f)
    [f r/no-bounds]
    (let [[n & {:keys [< > variance] :as opts}] f]
      (when (contains? opts :kind)
        (prn "DEPRECATED: kind annotation for TFn parameters"))
      {:fname n 
       :bnd (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (infer-bounds upper-or-nil lower-or-nil))
       :variance variance})))

; parsing All binders
;return a vector of [name bnds]
(defn parse-free [f]
  {:post [(u/hvector-c? symbol? r/Bounds?)]}
  (if (symbol? f)
    [f r/no-bounds]
    (let [[n & {:keys [< >] :as opts}] f]
      (when (contains? opts :kind)
        (prn "DEPRECATED: kind annotation for TFn parameters"))
      (assert (not (:variance opts)) "Variance not supported for variables introduced with All")
      [n (let [upper-or-nil (when (contains? opts :<)
                              (parse-type <))
               lower-or-nil (when (contains? opts :>)
                              (parse-type >))]
           (infer-bounds upper-or-nil lower-or-nil))])))

(defn check-forbidden-rec [rec tbody]
  (when (or (= rec tbody) 
            (and (r/Intersection? tbody)
                 (contains? (set (:types tbody)) rec))
            (and (r/Union? tbody)
                 (contains? (set (:types tbody)) rec)))
    (throw (Exception. "Recursive type not allowed here"))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [Mu* @(Mu*-var)
        _ (assert (= 1 (count bnder)) "Only one variable in allowed: Rec")
        f (r/make-F free-symbol)
        body (free-ops/with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

;(defmethod parse-type-list 'DottedPretype
;  [[_ psyn bsyn]]
;  (let [df (dvar/*dotted-scope* bsyn)]
;    (assert df bsyn)
;    (r/DottedPretype-maker (free-ops/with-frees [df]
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
      (r/make-Function [r/-any] (RClass-of 'boolean) nil nil
                       :filter (fl/-FS (fl/-filter on-type 0)
                                       (fl/-not-filter on-type 0))))))

(defmethod parse-type-list 'Not
  [[_ tsyn :as all]]
  (assert (= (count all) 2) "Wrong arguments to Not (expected 1)")
  (r/NotType-maker (parse-type tsyn)))

(defmethod parse-type-list 'Rec
  [syn]
  (parse-rec-type syn))

;dispatch on last element of syntax in binder
(defmulti parse-all-type (fn [bnds type] (last bnds)))

;(All [a b ...] type)
(defmethod parse-all-type '...
  [bnds type]
  (let [frees-with-bnds (reduce (fn [fs fsyn]
                                  {:pre [(vector? fs)]
                                   :post [(every? (u/hvector-c? symbol? r/Bounds?) %)]}
                                  (conj fs
                                        (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) fs)
                                          (parse-free fsyn))))
                                [] (-> bnds butlast butlast))
        dvar (parse-free (-> bnds butlast last))]
    (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                 (map second (concat frees-with-bnds [dvar]))
                 (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds)
                   (dvar/with-dotted [(r/make-F (first dvar))]
                     (parse-type type)))
                 (concat (map first frees-with-bnds) [(first dvar)]))))

;(All [a b] type)
(defmethod parse-all-type :default
  [bnds type]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (u/hvector-c? symbol? r/Bounds?) %)]}
                  (conj fs
                        (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) fs)
                          (parse-free fsyn))))
                [] bnds)]
    (c/Poly* (map first frees-with-bnds)
           (map second frees-with-bnds)
           (free-ops/with-bounded-frees (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds)
             (parse-type type))
           (map first frees-with-bnds))))

(defmethod parse-type-list 'Extends
  [[_ extends & {:keys [without] :as opts} :as syn]]
  (assert (empty? (set/difference (set (keys opts)) #{:without}))
          (str "Invalid options to Extends:" (keys opts)))
  (assert (vector? extends) (str "Extends takes a vector of types: " (pr-str syn)))
  (c/-extends (doall (map parse-type extends))
              :without (doall (map parse-type without))))

(defmethod parse-type-list 'All
  [[All bnds syn & more :as all]]
  ;(prn "All syntax" all)
  (assert (not more) "Bad All syntax")
  (parse-all-type bnds syn))

(defn parse-union-type [[u & types]]
  (apply c/Un (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (apply c/In (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(defmethod parse-type-list 'Array
  [[_ syn & none]]
  (assert (empty? none) "Expected 1 argument to Array")
  (let [t (parse-type syn)]
    (impl/impl-case
      :clojure (let [jtype (if (r/RClass? t)
                             (r/RClass->Class t)
                             Object)]
                 (r/PrimitiveArray-maker jtype t t))
      :cljs (r/ArrayCLJS-maker t t))))

(defmethod parse-type-list 'ReadOnlyArray
  [[_ osyn & none]]
  (assert (empty? none) "Expected 1 argument to ReadOnlyArray")
  (let [o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object (r/Bottom) o)
      :cljs (r/ArrayCLJS-maker (r/Bottom) o))))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none]]
  (assert (empty? none) "Expected 2 arguments to Array2")
  (let [i (parse-type isyn)
        o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object i o)
      :cljs (r/ArrayCLJS-maker i o))))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (assert (empty? none) "Expected 3 arguments to Array3")
  (impl/assert-clojure)
  (let [jrclass (parse-type jsyn)
        _ (assert (r/RClass? jrclass) "First argument to Array3 must be a Class")]
    (r/PrimitiveArray-maker (r/RClass->Class jrclass) (parse-type isyn) (parse-type osyn))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply r/make-FnIntersection (mapv parse-function types)))

(defmethod parse-type-list 'Fn
  [syn]
  (parse-fn-intersection-type syn))

(defn parse-free-binder [[nme & {:keys [variance < > kind] :as opts}]]
  (assert nme)
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

(defn find-bound* 
  "Find upper bound if polarity is true, otherwise lower bound"
  [t* polarity]
  {:pre [(r/Type? t*)]}
  (let [fnd-bnd #(find-bound* % polarity)
        t (c/fully-resolve-type t*)]
    (cond
      (r/Poly? t) (fnd-bnd (c/Poly-body* (repeatedly (:nbound t) gensym) t))
      (r/TypeFn? t) (let [names (repeatedly (:nbound t) gensym)
                          body (c/TypeFn-body* names t)
                          bbnds (c/TypeFn-bbnds* names t)]
                      (c/TypeFn* names
                                 (:variances t)
                                 bbnds
                                 (fnd-bnd body)))
      :else (if polarity
              r/-any
              r/-nothing))))

(defn find-upper-bound [t]
  {:pre [(r/Type? t)]}
  (find-bound* t true))

(defn find-lower-bound [t]
  {:pre [(r/Type? t)]}
  (find-bound* t false))

(defn infer-bounds
  "Returns a Bounds that attempts to fill in meaningful
  upper/lower bounds of the same rank"
  [upper-or-nil lower-or-nil]
  {:pre [(every? (some-fn nil? r/AnyType?) [upper-or-nil lower-or-nil])]
   :post [(r/Bounds? %)]}
  (let [{:keys [upper lower]} (cond 
                                ;both bounds provided
                                (and upper-or-nil lower-or-nil) {:upper upper-or-nil :lower lower-or-nil}
                                ;only upper
                                upper-or-nil {:upper upper-or-nil :lower (find-lower-bound upper-or-nil)}
                                ;only lower
                                lower-or-nil {:upper (find-upper-bound lower-or-nil) :lower lower-or-nil}
                                ;no bounds provided, default to Nothing <: Any
                                :else {:upper r/-any :lower r/-nothing})]
    (r/Bounds-maker upper lower nil)))

(defn parse-tfn-binder [[nme & {:keys [variance < >] :as opts}]]
  {:post [((u/hmap-c? :nme symbol? :variance r/variance?
                      :bound r/Bounds?) %)]}
  (assert nme)
  (when (contains? opts :kind)
    (prn "DEPRECATED: kind annotation for TFn parameters"))
  {:nme nme :variance (or variance :invariant)
   :bound (let [upper-or-nil (when (contains? opts :<)
                               (parse-type <))
                lower-or-nil (when (contains? opts :>)
                               (parse-type >))]
            (infer-bounds upper-or-nil lower-or-nil))})

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (assert (= 3 (count tfn)))
  (assert (every? vector? binder))
  (let [; don't bound frees because mutually dependent bounds are problematic
        free-maps (free-ops/with-free-symbols (map (fn [s]
                                                     {:pre [(vector? s)]
                                                      :post [(symbol? %)]}
                                                     (first s))
                                                   binder)
                    (mapv parse-tfn-binder binder))
        bodyt (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
                                                free-maps)
                (parse-type bodysyn))
        vs (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
                                             free-maps)
             (frees/fv-variances bodyt))
        _ (doseq [{:keys [nme variance]} free-maps]
            (when-let [actual-v (vs nme)]
              (assert (= (vs nme) variance)
                      (u/error-msg "Type variable " nme " appears in " (name actual-v) " position "
                                   "when declared " (name variance)))))]
    (with-meta (c/TypeFn* (map :nme free-maps) (map :variance free-maps)
                          (map :bound free-maps) bodyt)
               {:actual-frees (map :nme free-maps)})))

(defmethod parse-type-list 'TFn
  [syn]
  (parse-type-fn syn))

(defmethod parse-type-list 'Seq* [syn] (r/HeterogeneousSeq-maker (mapv parse-type (rest syn))))
(defmethod parse-type-list 'List* [syn] (r/HeterogeneousList-maker (mapv parse-type (rest syn))))
(defmethod parse-type-list 'Vector* [syn] (r/-hvec (mapv parse-type (rest syn))))

(defn- syn-to-hmap [mandatory optional absent-keys complete?]
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(r/-val k)
                        (parse-type v)])))]
    (let [_ (assert (every? empty? [(set/intersection (set (keys mandatory))
                                                      (set (keys optional)))
                                    (set/intersection (set (keys mandatory))
                                                      (set absent-keys))
                                    (set/intersection (set (keys optional))
                                                      (set absent-keys))])
                    (str "HMap options contain duplicate key entries: "
                         "Mandatory: " (into {} mandatory) ", Optional: " (into {} optional) 
                         ", Absent: " (set absent-keys)))
          _ (assert (every? keyword? (keys mandatory)) "HMap's mandatory keys must be keywords")
          mandatory (mapt mandatory)
          _ (assert (every? keyword? (keys optional)) "HMap's optional keys must be keywords")
          optional (mapt optional)
          _ (assert (every? keyword? absent-keys) "HMap's absent keys must be keywords")
          absent-keys (set (map r/-val absent-keys))]
      (c/make-HMap mandatory optional complete? :absent-keys absent-keys))))

(defmethod parse-type-list 'quote 
  [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol?) syn) (r/-val syn)
    (vector? syn) (r/-hvec (mapv parse-type syn))
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false)
    :else (throw (Exception. (str "Invalid use of quote:" syn)))))

(declare parse-in-ns)

(defmethod parse-type-list 'HMap
  [[_HMap_ & flat-opts]]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (println 
                                 (parse-in-ns)
                                 ": DEPRECATED: HMap syntax changed. Use :mandatory keyword argument instead of initial map")
                               (flush)
                               (first flat-opts))
        ^ISeq flat-opts (if deprecated-mandatory
                          (next flat-opts)
                          flat-opts)
        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (PersistentHashMap/createWithCheck flat-opts)
        _ (when-let [more (seq (set/difference (set (keys others)) supported-options))]
            (println "WARNING: Unsupported HMap options:" (vec more))
            (flush))
        _ (when (and deprecated-mandatory mandatory)
            (throw (Exception. "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete?)))

(defn- parse-in-ns []
  {:post [(symbol? %)]}
  (or *parse-type-in-ns* 
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs cljs-ana/*cljs-ns*)))

(defn- resolve-type-clj 
  "Returns a qualified symbol, class or nil"
  [sym]
  {:pre [(symbol? sym)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-let [ns (find-ns nsym)]
      (ns-resolve ns sym)
      (assert nil (str "Cannot find namespace: " sym)))))

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
        _ (assert (class? cls) (str cls-sym " cannot be resolved"))
        tparams (doall (map parse-type params-syn))]
    (RClass-of cls tparams)))

(defmethod parse-type-list 'Value
  [[_Value_ syn]]
  (impl/impl-case
    :clojure (const/constant-type syn)
    :cljs (assert nil "FIX parse Value")))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (assert (= #{}
             (set/intersection (set (keys optional))
                               (set (keys mandatory)))))
  (let [optional (into {} (for [[k v] optional]
                            (do (assert (keyword? k))
                              [(r/-val k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (assert (keyword? k))
                               [(r/-val k) (parse-type v)])))]
    (apply c/Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (r/HeterogeneousSeq-maker (mapcat #(find m %) ks)))))))))

(declare unparse-type)

;TODO should probably just be (r/TApp-maker (parse-type n) (mapv parse-type args))
(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (let [RClass-of @(RClass-of-var)
        current-nstr (-> (parse-in-ns) name)]
    (let [rsym (impl/impl-case
                 :clojure (when-let [res (when (symbol? n)
                                           (resolve-type-clj n))]
                            (cond 
                              (class? res) (u/Class->symbol res)
                              (var? res) (u/var->symbol res)))
                 :cljs (when-let [res (when (symbol? n)
                                        (resolve-type-cljs n))]
                         (:name res)))
          _ (assert ((some-fn symbol? nil?) rsym))]
    (if-let [free (and (symbol? n) (free-ops/free-in-scope n))]
      (r/TApp-maker free (mapv parse-type args))
      (if-let [t ((some-fn dtenv/get-datatype prenv/get-protocol nmenv/get-type-name) rsym)]
        ;don't resolve if operator is declared
        (r/TApp-maker (r/Name-maker rsym) (mapv parse-type args))
        (if (and (impl/checking-clojure?)
                 (class? (when (symbol? n)
                           (resolve-type-clj n))))
          ;a Class that's not a datatype
          (r/TApp-maker (r/Name-maker rsym) (mapv parse-type args))
          (cond
            (symbol? n)
            ;unqualified declared protocols and datatypes
            (if-let [s (let [svar (symbol current-nstr (name n))
                             scls (symbol (munge (str current-nstr \. (name n))))]
                         (some #(and (nmenv/get-type-name %)
                                     %)
                               [svar scls]))]
              (r/TApp-maker (r/Name-maker s) (mapv parse-type args))
              (u/tc-error (str "Cannot parse type: " (pr-str syn)
                               (when (seq syn)
                                 (str "\nHint: Does " (first syn) " accept parameters and is it in scope?")))))
            :else (r/TApp-maker (parse-type n) (mapv parse-type args)))))))))

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

(defmethod parse-type-symbol :default
  [sym]
  (let [primitives (impl/impl-case
                     :clojure (clj-primitives-fn)
                     :cljs (cljs-primitives-fn))]
    (letfn [(resolve-symbol [qsym clssym]
              (cond
                (primitives sym) (primitives sym)
                (nmenv/get-type-name qsym) (r/Name-maker qsym)
                (nmenv/get-type-name clssym) (r/Name-maker clssym)
                ;Datatypes that are annotated in this namespace, but not yet defined
                (dtenv/get-datatype clssym) (dtenv/resolve-datatype clssym)
                (prenv/get-protocol qsym) (prenv/resolve-protocol qsym)))]
      (if-let [f (free-ops/free-in-scope sym)]
        f
        (let []
          (or (impl/impl-case
                :clojure (let [current-nstr (-> (parse-in-ns) name)
                               qsym (if (namespace sym)
                                      sym
                                      (symbol current-nstr (name sym)))
                               clssym (if (some #(= \. %) (str sym))
                                        sym
                                        (symbol (str (munge current-nstr) \. (name sym))))
                               res (resolve-type-clj sym)
                               qsym (when (var? res)
                                      (u/var->symbol res))
                               clssym (when (class? res)
                                        (u/Class->symbol res))]
                           (or (resolve-symbol qsym clssym)
                               (when qsym
                                 (println (str "WARNING: Assuming unannotated var " qsym
                                               " is a protocol."))
                                 (flush)
                                 (r/Name-maker qsym))
                               (when clssym
                                 (c/RClass-of clssym))))
                :cljs (let [res (resolve-type-cljs sym)
                            ressym (:name res)]
                        (or (when (= "js" (namespace ressym)) 
                              (c/JSNominal-of ressym))
                            (when (and (some #{\.} (str sym))
                                       (not-any? #{\/} (str sym)))
                              ; sometimes we have a class-like symbol, foo.Bar
                              ; but resolve-type-cljs depends on the current namespace
                              ; scope. We might have this fully qualified name bound somewhere in our type
                              ; environent instead.
                              (cond
                                (jsnom/contains-jsnominal? sym)
                                (c/JSNominal-of sym)))

                            (resolve-symbol ressym ressym)
                            (when ressym
                              (println (str "WARNING: Assuming unannotated var " ressym
                                            " is a protocol."))
                              (flush)
                              (r/Name-maker ressym)))))
              (u/tc-error (str "Cannot resolve type: " (pr-str sym)
                               "\nHint: Is " (pr-str sym) " in scope?"
                               "\nHint: Has " (pr-str sym) "'s annotation been"
                               " found via check-ns, cf or typed-deps?"))))))))

(defmethod parse-type Symbol [l] (parse-type-symbol l))
(defmethod parse-type Boolean [v] (if v r/-true r/-false)) 
(defmethod parse-type nil [_] r/-nil)

(declare parse-path-elem parse-filter*)

(defn parse-filter [f]
  (cond
    (= 'tt f) f/-top
    (= 'ff f) f/-bot
    (not ((some-fn seq? list?) f)) (assert nil (str "Malformed filter expression: " f))
    :else (parse-filter* f)))

(defn parse-object [{:keys [id path]}]
  (orep/->Path (when path (mapv parse-path-elem path)) id))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (fl/-FS (if then
            (parse-filter then)
            f/-top)
          (if else
            (parse-filter else)
            f/-top)))

(defmulti parse-filter* first)

(defmethod parse-filter* 'is
  [[_ & [tsyn nme psyns :as all]]]
  (assert (#{2 3} (count all)))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-filter t nme p)))

(defmethod parse-filter* '!
  [[_ & [tsyn nme psyns :as all]]]
  (assert (#{2 3} (count all)))
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

(defmulti parse-path-elem #(cond
                             (symbol? %) %
                             :else (first %)))

(defmethod parse-path-elem 'Class [_] (pthrep/->ClassPE))
(defmethod parse-path-elem 'Count [_] (pthrep/->CountPE))

(defmethod parse-path-elem 'Keys [_] (pthrep/->KeysPE))
(defmethod parse-path-elem 'Vals [_] (pthrep/->ValsPE))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (assert (= 1 (count all)))
  (pthrep/->KeyPE ksyn))

(defn- parse-kw-map [m]
  {:post [((u/hash-c? r/Value? r/Type?) %)]}
  (into {} (for [[k v] m]
             [(r/-val k) (parse-type v)])))

(defn parse-function [f]
  {:post [(r/Function? %)]}
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts-flat :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
        _ (assert (<= 2 (count chk)) (str "Missing range in " f))

        opts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*
         ampersand-pos '&}
        (zipmap all-dom (range))

        _ (assert (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos])))
                  "Can only provide one rest argument option: & ... or *")

        _ (when-let [ks (seq (remove #{:filters :object :flow} (keys opts)))]
            (throw (Exception. (str "Invalid option/s: " ks))))

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
        _ (assert (or (not asterix-pos)
                      (= (count all-dom) (inc asterix-pos)))
                  (str "Trailing syntax after rest parameter: " (pr-str (drop (inc asterix-pos) all-dom))))
        [drest-type _ drest-bnd :as drest-seq] (when ellipsis-pos
                                                 (drop (dec ellipsis-pos) all-dom))
        _ (assert (or (not ellipsis-pos) (= 3 (count drest-seq))) 
                  "Dotted rest entry must be 3 entries")
        _ (assert (or (not ellipsis-pos) (symbol? drest-bnd))
                  "Dotted bound must be symbol")
        [& {optional-kws :optional mandatory-kws :mandatory} :as kws-seq]
        (let [kwsyn (when ampersand-pos
                      (drop (inc ampersand-pos) all-dom))]
          ; support deprecated syntax [& {} -> ] to be equivalent to [& :optional {} -> ]
          (if (and kwsyn
                   (map? (first kwsyn)))
            (do (prn "DEPRECATED: implicit optional parameters for Fn arity. Use :optional keyword argument beteween & and ->.")
                (cons :optional kwsyn))
            kwsyn))

        _ (assert (or (not ampersand-pos) (seq kws-seq)) 
                  "Must provide syntax after &")]
    (r/make-Function (mapv parse-type fixed-dom)
                     (parse-type rng)
                     (when asterix-pos
                       (parse-type rest-type))
                     (when ellipsis-pos
                       (let [bnd (dvar/*dotted-scope* drest-bnd)
                             _ (assert bnd (str (pr-str drest-bnd) " is not in scope as a dotted variable"))]
                         (r/DottedPretype-maker
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

(def ^:dynamic *next-nme* 0) ;stupid readable variables

(def ^:dynamic *unparse-type-in-ns* nil)
(set-validator! #'*unparse-type-in-ns* (some-fn nil? symbol?))

(defmacro with-unparse-ns [sym & body]
  `(binding [*unparse-type-in-ns* ~sym]
     ~@body))

(defn alias-in-ns
  "Returns an alias for namespace sym in ns, or nil if none."
  [nsym ns]
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
  [{:keys [name]}]
  (or (some (fn [[sym {{fname :name} :F}]]
              (when (= name fname)
                sym))
            free-ops/*free-scope*)
      name))

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
                   [(unparse-type pre-type) '... name]))
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
  (if poly?
    (list* (unparse-var-symbol-in-ns the-var) (mapv unparse-type poly?))
    the-var))

(defmethod unparse-type* DataType
  [{:keys [the-class poly?]}]
  (if poly?
    (list* (unparse-Class-symbol-in-ns the-class) (mapv unparse-type poly?))
    the-class))

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
  (let [nme (gensym "Mu")
        body (c/Mu-body* nme m)]
    (list 'Rec [nme] (unparse-type body))))

(defmethod unparse-type* PolyDots
  [{:keys [nbound] :as p}]
  (let [{:keys [actual-frees dvar-name]} (meta p)
        free-names actual-frees
        given-names? (and free-names dvar-name)
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs (if given-names?
             (vec (concat free-names [dvar-name]))
             (vec 
               (for [x (range *next-nme* end-nme)]
                 (symbol (str "v" x)))))
        body (c/PolyDots-body* fs p)]
    (binding [*next-nme* end-nme]
      (list 'All (vec (concat (butlast fs) [(last fs) '...])) (unparse-type body)))))

(defmethod unparse-type* Extends
  [{:keys [extends without]}]
  (list* 'Extends
         (mapv unparse-type extends)
         (when (seq without)
           [:without (mapv unparse-type without)])))

(defmethod unparse-type* Poly
  [{:keys [nbound] :as p}]
  (let [free-names (c/Poly-free-names* p)
        given-names? free-names
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs-names (or (and given-names? free-names)
                     (vec
                       (for [x (range *next-nme* end-nme)]
                         (symbol (str "v" x)))))
        bbnds (c/Poly-bbnds* fs-names p)
        fs (if given-names?
             (vec
               (for [[name {:keys [upper-bound lower-bound higher-kind]}] (map vector free-names bbnds)]
                 (let [u (when upper-bound 
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
                       name))))
             fs-names)
        body (c/Poly-body* fs-names p)]
    (binding [*next-nme* end-nme]
      (list 'All fs (unparse-type body)))))

(defmethod unparse-type* TypeFn
  [{:keys [nbound] :as p}]
  (let [free-names (-> p meta :actual-frees)
        given-names? free-names
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs-names (or (and given-names? free-names)
                     (vec
                       (for [x (range *next-nme* end-nme)]
                         (symbol (str "v" x)))))
        bbnds (c/TypeFn-bbnds* fs-names p)
        fs (vec
             (for [[name {:keys [upper-bound lower-bound higher-kind]} v] (map vector 
                                                                               fs-names
                                                                               bbnds
                                                                               (:variances p))]
               (let [u (when upper-bound 
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
                     [name :variance v]))))
        body (c/TypeFn-body* fs-names p)]
    (binding [*next-nme* end-nme]
      (list 'TFn fs (unparse-type body)))))

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
  [v]
  (mapv unparse-type (:types v)))

(defmethod unparse-type* HeterogeneousList
  [v]
  (list* 'List* (doall (map unparse-type (:types v)))))

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
  (let [sym (symbol (str "js/" name))]
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

