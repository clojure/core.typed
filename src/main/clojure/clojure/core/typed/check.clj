(ns ^:skip-wiki 
  ^{:core.typed {:collect-only true}}
  clojure.core.typed.check
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed :as t :refer [*already-checked* letfn>]]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.type-rep :as r :refer [ret-t ret-f ret-o ret TCResult? Type?]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.filter-protocols :as fprotocol]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.constant-type :as const]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.fold-rep :as fold]
            [clojure.core.typed.cs-rep :as crep]
            [clojure.core.typed.cs-gen :as cgen]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.tvar-env :as tvar-env]
            [clojure.core.typed.tvar-bnds :as tvar-bnds]
            [clojure.core.typed.dvar-env :as dvar-env]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.array-ops :as arr-ops]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.mm-env :as mm]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.protocol-env :as pcl-env]
            [clojure.core.typed.method-param-nilables :as mtd-param-nil]
            [clojure.core.typed.method-return-nilables :as mtd-ret-nil]
            [clojure.core.typed.method-override-env :as mth-override]
            [clojure.core.typed.ctor-override-env :as ctor-override]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.ns-deps :as ns-deps]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.jvm.tools.analyzer.hygienic :as hygienic]
            [clojure.pprint :as pprint]
            [clojure.math.combinatorics :as comb]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.reflect :as reflect])
  (:import (clojure.core.typed.lex_env PropEnv)
           (clojure.core.typed.type_rep Function FnIntersection RClass Poly DottedPretype HeterogeneousSeq
                                        Value KwArgs HeterogeneousMap DataType TCResult HeterogeneousVector
                                        FlowSet Union)
           (clojure.core.typed.object_rep Path)
           (clojure.core.typed.filter_rep NotTypeFilter TypeFilter FilterSet AndFilter OrFilter)
           (clojure.lang APersistentMap IPersistentMap IPersistentSet Var Seqable ISeq IPersistentVector
                         Reflector PersistentHashSet Symbol)))

(alter-meta! *ns* assoc :skip-wiki true)

(set! *warn-on-reflection* true)

(t/ann ^:no-check clojure.core.typed.parse-unparse/*unparse-type-in-ns* (U nil Symbol))
(t/ann ^:no-check clojure.core.typed/*already-checked* (U nil (t/Atom1 (t/Vec Symbol))))

;==========================================================
; # Type Checker
;
; The type checker is implemented here.

(t/ann tc-warning [Any * -> nil])
(defn tc-warning [& ss]
  (let [env vs/*current-env*]
    (binding [*out* *err*]
      (apply println "WARNING: Type Checker: "
             (str "(" (-> env :ns :name) ":" (:line env) 
                  (when-let [col (:column env)]
                    (str ":"col))
                  ") ")
             ss)
      (flush))))

(t/ann error-ret [(U nil TCResult) -> TCResult])
(defn error-ret 
  "Return a TCResult appropriate for when a type
  error occurs, with expected type expected.
  
  Use *only* in case of a type error."
  [expected]
  (ret (or (when expected
             (ret-t expected))
           (r/TCError-maker))))

(declare expr-ns)

;(t/ann expected-error [r/Type r/Type -> nil])
(defn expected-error [actual expected]
  (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                           (when vs/*current-expr*
                             (expr-ns vs/*current-expr*)))
    (u/tc-delayed-error (str "Type mismatch:"
                             "\n\nExpected: \t" (pr-str (prs/unparse-type expected))
                             "\n\nActual: \t" (pr-str (prs/unparse-type actual))))))

(declare check-expr)

(t/ann checked-ns! [Symbol -> nil])
(defn- checked-ns! [nsym]
  (t/when-let-fail [a *already-checked*]
    (swap! a conj nsym))
  nil)

(t/ann already-checked? [Symbol -> Boolean])
(defn- already-checked? [nsym]
  (t/when-let-fail [a *already-checked*]
    (boolean (@a nsym))))


(t/ann check-ns-and-deps [Symbol -> nil])
(defn check-ns-and-deps 
  "Type check a namespace and its dependencies.
  Assumes type annotations in each namespace
  has already been collected."
  ([nsym]
   (u/p :check/check-ns-and-deps
   (let [ns (find-ns nsym)
         _ (assert ns (str "Namespace " nsym " not found during type checking"))]
     (cond 
       (already-checked? nsym) (do
                                 ;(println (str "Already checked " nsym ", skipping"))
                                 ;(flush)
                                 nil)
       :else
       ; check deps
       (let [deps (u/p :check/ns-immediate-deps 
                    (ns-deps/immediate-deps nsym))]
         (checked-ns! nsym)
         (doseq [dep deps]
           (check-ns-and-deps dep))
         ; ignore ns declaration
         (let [check? (not (-> ns meta :core.typed :collect-only))]
           (if-not check?
             (do (println (str "Not checking " nsym " (tagged :collect-only in ns metadata)"))
                 (flush))
             (let [start (. System (nanoTime))
                   [_ns-decl_ & asts] (u/p :check/gen-analysis (ana-clj/ast-for-ns nsym))]
               (println "Start checking" nsym)
               (flush)
               (doseq [ast asts]
                 (check-expr ast))
               (println "Checked" nsym "in" (/ (double (- (. System (nanoTime)) start)) 1000000.0) "msecs"))))
         (flush)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(t/ann expr-ns [Any -> Symbol])
(defn expr-ns [expr]
  {:post [(symbol? %)]}
  (impl/impl-case
    :clojure (let [nsym (when-let [s (get-in expr [:env :ns :name])]
                          (symbol s))
                   _ (assert nsym (str "Bug! " (:op expr) " expr has no associated namespace"))
                   ns (find-ns nsym)
                   _ (assert ns)]
               (ns-name ns))
    :cljs (-> expr :env :ns :name)))

(def expr-type ::expr-type)

(defmulti check (fn [expr & [expected]]
                  {:pre [((some-fn nil? TCResult?) expected)]}
                  (:op expr)))

(u/add-defmethod-generator check)

(defn check-expr [expr & [expected]]
  (when t/*trace-checker*
    (println "Checking line:" (-> expr :env :line))
    (flush))
  (u/p :check/check-expr
  (check expr expected)))

(defmacro tc-t [form]
  `(let [{delayed-errors# :delayed-errors ret# :ret}
         (impl/with-clojure-impl
           (t/check-form-info '~form))]
     (if-let [errors# (seq delayed-errors#)]
       (t/print-errors! errors#)
       ret#)))

(defmacro tc [form]
  `(impl/with-clojure-impl
     (t/check-form* '~form)))

(defn flow-for-value []
  (let [props (.props ^PropEnv lex/*lexical-env*)
        flow (r/-flow (apply fo/-and fl/-top props))]
    flow))

(defn check-value
  [{:keys [val] :as expr} & [expected]]
  (let [actual-type (const/constant-type val)
        _ (when (and expected (not (sub/subtype? actual-type (ret-t expected))))
            (binding [vs/*current-expr* expr]
              (expected-error actual-type (ret-t expected))))
        flow (flow-for-value)]
    (assoc expr
           expr-type (if val
                       (ret actual-type
                            (fo/-FS fl/-top fl/-bot)
                            obj/-empty
                            flow)
                       (ret actual-type
                            (fo/-FS fl/-bot fl/-top)
                            obj/-empty
                            flow)))))

(add-check-method :constant [& args] (apply check-value args))
(add-check-method :number [& args] (apply check-value args))
(add-check-method :string [& args] (apply check-value args))
(add-check-method :keyword [& args] (apply check-value args))
(add-check-method :boolean [& args] (apply check-value args))
(add-check-method :nil [& args] (apply check-value args))

;(ann expected-vals [(Coll Type) (Nilable TCResult) -> (Coll (Nilable TCResult))])
(defn expected-vals
  "Returns a sequence of (Nilable TCResults) to use as expected types for type
  checking the values of a literal map expression"
  [key-types expected]
  {:pre [(every? r/Type? key-types)
         ((some-fn r/TCResult? nil?) expected)]
   :post [(every? (some-fn nil? r/TCResult?) %)
          (= (count %)
             (count key-types))]}
  (let [expected (when expected 
                   (c/fully-resolve-type (ret-t expected)))
        flat-expecteds (when expected
                         (mapv c/fully-resolve-type
                               (if (r/Union? expected)
                                 (:types expected)
                                 [expected])))
        no-expecteds (repeat (count key-types) nil)]
    (cond 
      ; If every key in this map expression is a Value, let's try and
      ; make use of our expected type for each individual entry. This is
      ; most useful for `extend`, where we have HMaps of functions, and
      ; we want to forward the expected types to each val, a function.
      ;
      ; The expected type also needs to be an expected shape:
      ; - a HMap or union of just HMaps
      ; - each key must have exactly one possible type if it is
      ;   present
      ; - if a key is absent in a HMap type, it must be explicitly
      ;   absent
      (and expected 
           (every? r/Value? key-types)
           (every? r/HeterogeneousMap? flat-expecteds))
        (let [hmaps flat-expecteds]
          (reduce (fn [val-expecteds ktype]
                    ; also includes this contract wrapped in a Reduced
                    ; The post-condition for `expected-vals` catches this anyway
                    ;{:post [(every? (some-fn nil? r/TCResult?) %)]}

                    (let [; find the ktype key in each hmap.
                          ; - If ktype is present in :types then we either use the entry's val type 
                          ; - if ktype is explicitly forbidden via :absent-keys or :other-keys
                          ;   we skip the entry.
                          ; - otherwise we give up and don't check this as a hmap, return nil
                          ;   that gets propagated up
                          corresponding-vals 
                          (reduce (fn [corresponding-vals {:keys [types absent-keys other-keys?] :as hmap}]
                                    (if-let [v (get types ktype)]
                                      (conj corresponding-vals v)
                                      (cond
                                        (or (contains? absent-keys ktype)
                                            (not other-keys?)) 
                                        corresponding-vals
                                        :else (reduced nil))))
                                  #{} hmaps)
                          val-expect (when (= 1 (count corresponding-vals))
                                       (ret (first corresponding-vals)))]
                      (if val-expect
                        (conj val-expecteds val-expect)
                        (reduced no-expecteds))))
                  [] key-types))

      ; If we expect an (IPersistentMap k v), just use the v as the expected val types
      (and (r/RClass? expected)
           (= (:the-class expected) 'clojure.lang.IPersistentMap))
        (let [{[_ vt] :poly?} expected]
          (map ret (repeat (count key-types) vt)))
      ; otherwise we don't give expected types
      :else no-expecteds)))

(add-check-method :map
  [{:keys [keyvals] :as expr} & [expected]]
  (let [[keyexprs valexprs] (let [s (partition 2 keyvals)]
                              [(map first s) (map second s)])
        ckeyexprs (mapv check keyexprs)
        key-types (map (comp ret-t expr-type) ckeyexprs)

        val-rets
        (expected-vals key-types expected)

        cvalexprs (mapv check valexprs val-rets)
        val-types (map (comp ret-t expr-type) cvalexprs)

        ts (zipmap key-types val-types)
        actual (if (every? c/keyword-value? (keys ts))
                 (c/-complete-hmap ts)
                 (c/RClass-of APersistentMap [(apply c/Un (keys ts))
                                              (apply c/Un (vals ts))]))
        _ (when expected
            (when-not (sub/subtype? actual (ret-t expected))
              (expected-error actual (ret-t expected))))]
    (assoc expr
           expr-type (ret actual (fo/-true-filter)))))

(add-check-method :set
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)
        res-type (c/RClass-of PersistentHashSet [(apply c/Un (mapv (comp ret-t expr-type) cargs))])
        _ (when (and expected (not (sub/subtype? res-type (ret-t expected))))
            (expected-error res-type (ret-t expected)))]
    (assoc expr
           expr-type (ret res-type (fo/-true-filter)))))

(add-check-method :vector
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)
        res-type (r/-hvec (mapv (comp ret-t expr-type) cargs)
                          :filters (mapv (comp ret-f expr-type) cargs)
                          :objects (mapv (comp ret-o expr-type) cargs))
        _ (when (and expected (not (sub/subtype? res-type (ret-t expected))))
            (expected-error res-type (ret-t expected)))]
    (assoc expr
           expr-type (ret res-type (fo/-true-filter)))))

(add-check-method :empty-expr 
  [{coll :coll :as expr} & [expected]]
  (let [actual (const/constant-type coll)
        _ (when (and expected (not (sub/subtype? actual (ret-t expected))))
            (expected-error actual (ret-t expected)))]
    (assoc expr
           expr-type (ret actual (fo/-true-filter)))))

;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [((some-fn TCResult? Type?) tr1)
         ((some-fn TCResult? Type?) expected)]
   :post [((some-fn TCResult? Type?) %)]}
  (letfn [(filter-better? [{f1+ :then f1- :else :as f1}
                           {f2+ :then f2- :else :as f2}]
            {:pre [(fl/Filter? f1)
                   (fl/Filter? f2)]
             :post [(u/boolean? %)]}
            (cond
              (= f1 f2) true
              (and (fo/implied-atomic? f2+ f1+)
                   (fo/implied-atomic? f2- f1-)) true
              :else false))
          (object-better? [o1 o2]
            {:pre [(obj/RObject? o1)
                   (obj/RObject? o2)]
             :post [(u/boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn obj/NoObject? obj/EmptyObject?) o2) true
              :else false))]
    ;tr1 = arg
    ;expected = dom
    ; Omitted some cases dealing with multiple return values
    (cond
      (and (TCResult? tr1)
           (TCResult? expected)
           (= (c/Un) (ret-t tr1))
           (fl/NoFilter? (ret-f expected))
           (obj/NoObject? (ret-o expected)))
      (let [ts2 (:t tr1)]
        (ret ts2))

      (and (TCResult? tr1)
           (= (c/Un) (ret-t tr1)))
      expected

      (and (TCResult? tr1)
           (TCResult? expected)
           (= (fo/-FS fl/-top fl/-top)
              (ret-f expected))
           (obj/EmptyObject? (ret-o expected)))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t} expected]
        (when-not (sub/subtype? t1 t2)
          (expected-error t1 t2))
        expected)

      (and (TCResult? tr1)
           (TCResult? expected))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t f2 :fl o2 :o} expected]
        (cond
          (not (sub/subtype? t1 t2)) (expected-error t1 t2)

          (and (not (filter-better? f1 f2))
               (object-better? o1 o2))
          (u/tc-delayed-error (str "Expected result with filter " f2 ", got filter"  f1))

          (and (filter-better? f1 f2)
               (not (object-better? o1 o2)))
          (u/tc-delayed-error (str "Expected result with object " o2 ", got object"  o1))

          (and (not (filter-better? f1 f2))
               (not (object-better? o1 o2)))
          (u/tc-delayed-error (str "Expected result with object " o2 ", got object"  o1 " and filter "
                                   f2 " got filter " f1)))
        expected)

      (and (TCResult? tr1)
           (Type? expected))
      (let [{t1 :t f :fl o :o} tr1
            t2 expected]
        (when-not (sub/subtype? t1 t2)
          (expected-error t1 t2))
        (ret t2 f o))

      ;FIXME
      ;; erm.. ? What is (FilterSet: (list) (list))
      ;; TODO this case goes here, but not sure what it means 
      ;
      ;[((? Type? t1) (tc-result1: t2 (FilterSet: (list) (list)) (Empty:)))
      ; (unless (sub/subtype t1 t2)
      ;   (tc-error/expr "Expected ~a, but got ~a" t2 t1))
      ; t1]

      (and (Type? tr1)
           (TCResult? expected))
      (let [t1 tr1
            {t2 :t f :fl o :o} expected]
        (if (sub/subtype? t1 t2)
          (u/tc-delayed-error (str "Expected result with filter " f " and " o ", got " t1))
          (expected-error t1 t2))
        t1)

      (and (Type? tr1)
           (Type? expected))
      (let [t1 tr1
            t2 expected]
        (when-not (sub/subtype? t1 t2)
          (expected-error t1 t2))
        expected)

      :else (let [a tr1
                  b expected]
              (u/int-error (str "Unexpected input for check-below " a b))))))

(derive ::free-in-for-object fold/fold-rhs-default)

(fold/add-fold-case ::free-in-for-object
                    Path
                    (fn [{p :path i :id :as o} {{:keys [free-in? k]} :locals}]
                      (if (= i k)
                        (reset! free-in? true)
                        o)))

(derive ::free-in-for-filter fold/fold-rhs-default)

(fold/add-fold-case ::free-in-for-filter
                    NotTypeFilter
                    (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                      (if (= i k)
                        (reset! free-in? true)
                        t)))

(fold/add-fold-case ::free-in-for-filter
                    TypeFilter
                    (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                      (if (= i k)
                        (reset! free-in? true)
                        t)))

(derive ::free-in-for-type fold/fold-rhs-default)

(declare index-free-in?)

(fold/add-fold-case ::free-in-for-type
                    Function
                    (fn [{:keys [dom rng rest drest kws]} {{:keys [k free-in? for-type]} :locals}]
                      ;; here we have to increment the count for the domain, where the new bindings are in scope
                      (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (concat (:mandatory kws)
                                                                                                 (:optional kws))))
                            st* (fn [t] (index-free-in? (if (number? k) (+ arg-count k) k) t))]
                        (doseq [d dom]
                          (for-type d))
                        (st* rng)
                        (and rest (for-type rest))
                        (and rest (for-type (:pre-type drest)))
                        (doseq [[_ v] (concat (:mandatory kws)
                                              (:optional kws))]
                          (for-type v))
                        ;dummy return value
                        (r/make-Function [] r/-any))))

;[AnyInteger Type -> Boolean]
(defn index-free-in? [k type]
  (let [free-in? (atom false :validator u/boolean?)]
    (letfn [(for-object [o]
              (fold/fold-rhs ::free-in-for-object
                             {:type-rec for-type
                              :locals {:free-in? free-in?
                                       :k k}}
                             o))
            (for-filter [o]
              (fold/fold-rhs ::free-in-for-filter
                             {:type-rec for-type
                              :filter-rec for-filter
                              :locals {:free-in? free-in?
                                       :k k}}
                             o))
            (for-type [t]
              (fold/fold-rhs ::free-in-for-type
                             {:type-rec for-type
                              :filter-rec for-filter
                              :object-rec for-object
                              :locals {:free-in? free-in?
                                       :k k
                                       :for-type for-type}}
                             t))]
      (for-type type)
      @free-in?)))

(declare subst-type)

;[Filter (U Number Symbol) RObject Boolean -> Filter]
(defn subst-filter [f k o polarity]
  {:pre [(fl/Filter? f)
         (fl/name-ref? k)
         (obj/RObject? o)
         (u/boolean? polarity)]
   :post [(fl/Filter? %)]}
  (letfn [(ap [f] (subst-filter f k o polarity))
          (tf-matcher [t p i k o polarity maker]
            {:pre [(Type? t)
                   ((some-fn obj/EmptyObject? obj/NoObject? obj/Path?) o)]
             :post [(fl/Filter? %)]}
            (cond
              ((some-fn obj/EmptyObject? obj/NoObject?)
               o)
              (cond 
                (= i k) (if polarity fl/-top fl/-bot)
                (index-free-in? k t) (if polarity fl/-top fl/-bot)
                :else f)

              (obj/Path? o) (let [{p* :path i* :id} o]
                              (cond
                                (= i k) (maker 
                                          (subst-type t k o polarity)
                                          i*
                                          (concat p p*))
                                (index-free-in? k t) (if polarity fl/-top fl/-bot)
                                :else f))
              :else (u/int-error (str "what is this? " o))))]
    (cond
      (fl/ImpFilter? f) (let [{ant :a consq :c} f]
                          (fo/-imp (subst-filter ant k o (not polarity)) (ap consq)))
      (fl/AndFilter? f) (let [fs (:fs f)] 
                          (apply fo/-and (map ap fs)))
      (fl/OrFilter? f) (let [fs (:fs f)]
                         (apply fo/-or (map ap fs)))
      (fl/BotFilter? f) fl/-bot
      (fl/TopFilter? f) fl/-top

      (fl/TypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity fo/-filter))

      (fl/NotTypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity fo/-not-filter)))))

(defn- add-extra-filter
  "If provided a type t, then add the filter (is t k).
  Helper function."
  [fl k t]
  {:pre [(fl/Filter? fl)
         (fl/name-ref? k)
         ((some-fn false? nil? Type?) t)]
   :post [(fl/Filter? %)]}
  (let [extra-filter (if t (fl/->TypeFilter t nil k) fl/-top)]
    (letfn [(add-extra-filter [f]
              {:pre [(fl/Filter? f)]
               :post [(fl/Filter? %)]}
              (let [f* (fo/-and extra-filter f)]
                (if (fl/BotFilter? f*)
                  f*
                  f)))]
      (add-extra-filter fl))))

(defn subst-flow-set [^FlowSet fs k o polarity & [t]]
  {:pre [(r/FlowSet? fs)
         (fl/name-ref? k)
         (obj/RObject? o)
         ((some-fn false? nil? Type?) t)]
   :post [(r/FlowSet? %)]}
  (r/-flow (subst-filter (add-extra-filter (.normal fs) k t) k o polarity)))

;[FilterSet Number RObject Boolean (Option Type) -> FilterSet]
(defn subst-filter-set [fs k o polarity & [t]]
  {:pre [((some-fn fl/FilterSet? fl/NoFilter?) fs)
         (fl/name-ref? k)
         (obj/RObject? o)
         ((some-fn false? nil? Type?) t)]
   :post [(fl/FilterSet? %)]}
  ;  (prn "subst-filter-set")
  ;  (prn "fs" (prs/unparse-filter-set fs))
  ;  (prn "k" k) 
  ;  (prn "o" o)
  ;  (prn "polarity" polarity) 
  ;  (prn "t" (when t (prs/unparse-type t)))
  (cond
    (fl/FilterSet? fs) (let [^FilterSet fs fs]
                         (fo/-FS (subst-filter (add-extra-filter (.then fs) k t) k o polarity)
                                 (subst-filter (add-extra-filter (.else fs) k t) k o polarity)))
    :else (fo/-FS fl/-top fl/-top)))

;[Type Number RObject Boolean -> RObject]
(defn subst-object [t k o polarity]
  {:pre [(obj/RObject? t)
         (fl/name-ref? k)
         (obj/RObject? o)
         (u/boolean? polarity)]
   :post [(obj/RObject? %)]}
  (cond
    ((some-fn obj/NoObject? obj/EmptyObject?) t) t
    (obj/Path? t) (let [{p :path i :id} t]
                    (if (= i k)
                      (cond
                        (obj/EmptyObject? o) (obj/->EmptyObject)
                        ;; the result is not from an annotation, so it isn't a NoObject
                        (obj/NoObject? o) (obj/->EmptyObject)
                        (obj/Path? o) (let [{p* :path i* :id} o]
                                        (obj/->Path (seq (concat p p*)) i*)))
                      t))))

(derive ::subst-type fold/fold-rhs-default)

(fold/add-fold-case ::subst-type
                    Function
                    (fn [{:keys [dom rng rest drest kws] :as ty} {{:keys [st k o polarity]} :locals}]
                      ;; here we have to increment the count for the domain, where the new bindings are in scope
                      (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (:mandatory kws)) (count (:optional kws)))
                            st* (if (integer? k)
                                  (fn [t] 
                                    {:pre [(r/AnyType? t)]}
                                    (subst-type t (if (number? k) (+ arg-count k) k) o polarity))
                                  st)]
                        (r/Function-maker (map st dom)
                                      (st* rng)
                                      (and rest (st rest))
                                      (when drest
                                        (-> drest
                                            (update-in [:pre-type] st)))
                                      (when kws
                                        (-> kws
                                            (update-in [:mandatory] #(into {} (for [[k v] %]
                                                                                [(st k) (st v)])))
                                            (update-in [:optional] #(into {} (for [[k v] %]
                                                                               [(st k) (st v)])))))))))


;[Type (U Symbol Number) RObject Boolean -> Type]
(defn subst-type [t k o polarity]
  {:pre [(r/AnyType? t)
         (fl/name-ref? k)
         (obj/RObject? o)
         (u/boolean? polarity)]
   :post [(r/AnyType? %)]}
  ;(prn "subst-type" (prs/unparse-type t))
  (letfn [(st [t*]
            (subst-type t* k o polarity))
          (sf [fs] 
            {:pre [((some-fn fl/FilterSet? r/FlowSet?) fs)] 
             :post [((some-fn fl/FilterSet? r/FlowSet?) %)]}
            ((if (fl/FilterSet? fs) subst-filter-set subst-flow-set) 
             fs k o polarity))]
    (fold/fold-rhs ::subst-type
      {:type-rec st
       :filter-rec sf
       :object-rec (fn [f] (subst-object f k o polarity))
       :locals {:st st
                :k k
                :o o
                :polarity polarity}}
      t)))

;; Used to "instantiate" a Result from a function call.
;; eg. (let [a (ann-form [1] (U nil (Seqable Number)))]
;;       (if (seq a)
;;         ...
;;
;; Here we want to instantiate the result of (seq a).
;; objs is each of the arguments' objects, ie. [obj/-empty]
;; ts is each of the arugments' types, ie. [(U nil (Seqable Number))]
;;
;; The latent result:
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) 0)
;           :else (| (is nil 0)
;                    (is (ExactCount 0) 0))}]))
;; instantiates to 
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) a)
;           :else (| (is nil a)
;                    (is (ExactCount 0) a))}]))
;;
;; Notice the objects are instantiated from 0 -> a
;
;[Result (Seqable RObject) (Option (Seqable Type)) 
;  -> '[Type FilterSet RObject]]
(defn open-Result 
  "Substitute ids for objs in Result t"
  [{t :t fs :fl old-obj :o :keys [flow] :as r} objs & [ts]]
  {:pre [(r/Result? r)
         (every? obj/RObject? objs)
         ((some-fn fl/FilterSet? fl/NoFilter?) fs)
         (obj/RObject? old-obj)
         (r/FlowSet? flow)
         ((some-fn nil? (u/every-c? Type?)) ts)]
   :post [((u/hvector-c? Type? fl/FilterSet? obj/RObject? r/FlowSet?) %)]}
  ;  (prn "open-result")
  ;  (prn "result type" (prs/unparse-type t))
  ;  (prn "result filterset" (prs/unparse-filter-set fs))
  ;  (prn "result (old) object" old-obj)
  ;  (prn "objs" objs)
  ;  (prn "ts" (mapv prs/unparse-type ts))
  (reduce (fn [[t fs old-obj flow] [[o k] arg-ty]]
            {:pre [(Type? t)
                   ((some-fn fl/FilterSet? fl/NoFilter?) fs)
                   (obj/RObject? old-obj)
                   (integer? k)
                   (obj/RObject? o)
                   (r/FlowSet? flow)
                   ((some-fn false? Type?) arg-ty)]
             :post [((u/hvector-c? Type? fl/FilterSet? obj/RObject? r/FlowSet?) %)]}
            (let [r [(subst-type t k o true)
                     (subst-filter-set fs k o true arg-ty)
                     (subst-object old-obj k o true)
                     (subst-flow-set flow k o true arg-ty)]]
              ;              (prn [(prs/unparse-type t) (prs/unparse-filter-set fs) old-obj])
              ;              (prn "r" r)
              r))
          [t fs old-obj flow]
          ; this is just a sequence of pairs of [nat? RObject] and Type?
          ; Represents the object and type of each argument, and its position
          (map vector 
               (map-indexed #(vector %2 %1) ;racket's is opposite..
                            objs)
               (if ts
                 ts
                 (repeat false)))))


;Function TCResult^n (or nil TCResult) -> TCResult
(defn check-funapp1 [fexpr arg-exprs {{optional-kw :optional mandatory-kw :mandatory :as kws} :kws
                                      :keys [dom rng rest drest] :as ftype0}
                     argtys expected & {:keys [check?] :or {check? true}}]
  {:pre [(r/Function? ftype0)
         (every? TCResult? argtys)
         ((some-fn nil? TCResult?) expected)
         (u/boolean? check?)]
   :post [(TCResult? %)]}
  (when drest 
    (u/nyi-error "funapp with drest args NYI"))
  ;  (prn "check-funapp1")
  ;  (prn "argtys objects" (map ret-o argtys))
  ;checking
  (when check?
    (let [nactual (count argtys)]
      (when-not (or (when (and (not rest)
                               (empty? optional-kw)
                               (empty? mandatory-kw))
                      (= (count dom) (count argtys)))
                    (when rest
                      (<= (count dom) nactual))
                    (when kws
                      (let [nexpected (+ (count dom)
                                         (* 2 (count mandatory-kw)))]
                        (and (even? (- nactual (count dom)))
                             ((if (seq optional-kw) <= =)
                              nexpected
                              nactual)))))
        (u/tc-delayed-error (str "Wrong number of arguments, expected " (count dom) " fixed parameters"
                                 (cond
                                   rest " and a rest parameter "
                                   drest " and a dotted rest parameter "
                                   kws (cond
                                         (and (seq mandatory-kw) (seq optional-kw))
                                         (str ", some optional keyword arguments and " (count mandatory-kw) 
                                              " mandatory keyword arguments")

                                         (seq mandatory-kw) (str "and " (count mandatory-kw) "  mandatory keyword arguments")
                                         (seq optional-kw) " and some optional keyword arguments"))
                                 ", and got " nactual
                                 " for function " (pr-str (prs/unparse-type ftype0))
                                 " and arguments " (pr-str (mapv (comp prs/unparse-type ret-t) argtys)))))
      (cond
        ; case for regular rest argument, or no rest parameter
        (or rest (empty? (remove nil? [rest drest kws])))
        (doseq [[arg-t dom-t] (map vector 
                                   (map ret-t argtys) 
                                   (concat dom (when rest (repeat rest))))]
          (check-below arg-t dom-t))
        
        ; case for mandatory or optional keyword arguments
        kws
        (do
          ;check regular args
          (doseq [[arg-t dom-t] (map vector (map ret-t (take (count dom) argtys)) dom)]
            (check-below arg-t dom-t))
          ;check keyword args
          (let [flat-kw-argtys (drop (count dom) argtys)]
            (when-not (even? (count flat-kw-argtys))
              (u/tc-delayed-error  
                (str "Uneven number of arguments to function expecting keyword arguments")))
            (let [kw-args-paired-t (apply hash-map (map ret-t flat-kw-argtys))]
              ;make sure all mandatory keys are present
              (when-let [missing-ks (seq 
                                      (set/difference (set (keys mandatory-kw))
                                                      (set (keys kw-args-paired-t))))]
                (u/tc-delayed-error (str "Missing mandatory keyword keys: "
                                         (pr-str (interpose ", " (map prs/unparse-type missing-ks))))))
              ;check each keyword argument is correctly typed
              (doseq [[kw-key-t kw-val-t] kw-args-paired-t]
                (when-not (r/Value? kw-key-t)
                  (u/tc-delayed-error (str "Can only check keyword arguments with Value keys, found"
                                           (pr-str (prs/unparse-type kw-key-t)))))
                (let [expected-val-t ((some-fn optional-kw mandatory-kw) kw-key-t)]
                  (if expected-val-t
                    (check-below kw-val-t expected-val-t)
                    ; It is an error to use an undeclared keyword arg because we want to treat the rest parameter
                    ; as a complete hash-map.
                    (u/tc-delayed-error (str "Undeclared keyword parameter " 
                                             (pr-str (prs/unparse-type kw-key-t)))))))))))))
  (let [dom-count (count dom)
        arg-count (+ dom-count (if rest 1 0) (count optional-kw))
        o-a (map ret-o argtys)
        _ (assert (every? obj/RObject? o-a))
        t-a (map ret-t argtys)
        _ (assert (every? Type? t-a))
        [o-a t-a] (let [rs (for [[nm oa ta] (map vector 
                                                 (range arg-count) 
                                                 (concat o-a (repeatedly obj/->EmptyObject))
                                                 (concat t-a (repeatedly c/Un)))]
                             [(if (>= nm dom-count) (obj/->EmptyObject) oa)
                              ta])]
                    [(map first rs) (map second rs)])
        [t-r f-r o-r flow-r] (open-Result rng o-a t-a)]
    (ret t-r f-r o-r flow-r)))

(declare MethodExpr->qualsym)

;[Expr (Seqable Expr) (Seqable TCResult) (Option TCResult) Boolean
; -> Any]
(defn ^String app-type-error [fexpr args ^FnIntersection fin arg-ret-types expected poly?]
  {:pre [(r/FnIntersection? fin)
         (or (not poly?)
             ((some-fn r/Poly? r/PolyDots?) poly?))]
   :post [(r/TCResult? %)]}
  (let [static-method? (= :static-method (:op fexpr))
        instance-method? (= :instance-method (:op fexpr))
        method-sym (when (or static-method? instance-method?)
                     (MethodExpr->qualsym fexpr))]
    (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                             (or (when fexpr
                                   (expr-ns fexpr))
                                 (when vs/*current-expr*
                                   (expr-ns vs/*current-expr*))))
      (u/tc-delayed-error
        (str
          (if poly?
            (str "Polymorphic " 
                 (cond static-method? "static method "
                       instance-method? "instance method "
                       :else "function "))
            (cond static-method? "Static method "
                  instance-method? "Instance method "
                  :else "Function "))
          (if (or static-method?
                  instance-method?)  
            (or method-sym
                (:method-name fexpr))
            (if fexpr
              (u/emit-form-fn fexpr)
              "<NO FORM>"))
          " could not be applied to arguments:\n"
          (when poly?
            (let [names (cond 
                          (r/Poly? poly?) (c/Poly-fresh-symbols* poly?)
                          ;PolyDots
                          :else (c/PolyDots-fresh-symbols* poly?))
                  bnds (if (r/Poly? poly?)
                         (c/Poly-bbnds* names poly?)
                         (c/PolyDots-bbnds* names poly?))
                  dotted (when (r/PolyDots? poly?)
                           (last names))]
              (str "Polymorphic Variables:\n\t"
                   (clojure.string/join "\n\t" 
                                        (map (partial apply pr-str)
                                             (map (fn [{:keys [lower-bound upper-bound] :as bnd} nme]
                                                    {:pre [(r/Bounds? bnd)
                                                           (symbol? nme)]}
                                                    (cond
                                                      (= nme dotted) [nme '...]
                                                      :else (concat [nme]
                                                                    (when-not (= r/-nothing lower-bound)
                                                                      [:> (prs/unparse-type lower-bound)])
                                                                    (when-not (= r/-any upper-bound)
                                                                      [:< (prs/unparse-type upper-bound)]))))
                                                  bnds (map (comp r/F-original-name r/make-F) names)))))))
          "\n\nDomains:\n\t" 
          (clojure.string/join "\n\t" 
                               (map (partial apply pr-str) 
                                    (map (fn [{:keys [dom rest drest kws]}]
                                           (concat (map prs/unparse-type dom)
                                                   (when rest
                                                     [(prs/unparse-type rest) '*])
                                                   (when-let [{:keys [pre-type name]} drest]
                                                     [(prs/unparse-type pre-type) 
                                                      '... 
                                                      (-> name r/make-F r/F-original-name)])
                                                   (letfn [(readable-kw-map [m]
                                                             (into {} (for [[k v] m]
                                                                        (do (assert (r/Value? k))
                                                                            [(:val k) (prs/unparse-type v)]))))]
                                                     (when-let [{:keys [mandatory optional]} kws]
                                                       (concat ['&]
                                                               (when (seq mandatory)
                                                                 [:mandatory (readable-kw-map mandatory)])
                                                               (when (seq optional)
                                                                 [:optional (readable-kw-map optional)]))))))
                                         (:types fin))))
          "\n\n"
          "Arguments:\n\t" (apply prn-str (map (comp prs/unparse-type ret-t) arg-ret-types))
          "\n"
          "Ranges:\n\t"
          (clojure.string/join "\n\t" 
                               (map (partial apply pr-str) (map (comp prs/unparse-result :rng) (:types fin))))
          "\n\n"
          (when expected (str "with expected type:\n\t" (prs/unparse-type (ret-t expected)) "\n\n"))
          "in: " (if fexpr
                   (if (or static-method? instance-method?)
                     (u/emit-form-fn fexpr)
                     (list* (u/emit-form-fn fexpr)
                            (map u/emit-form-fn args)))
                   "<NO FORM>"))
        :return (or expected (ret r/Err))))))

(defn ^String polyapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [((some-fn r/Poly? r/PolyDots?) fexpr-type)]
   :post [(r/TCResult? %)]}
  (let [fin (if (r/Poly? fexpr-type)
              (c/Poly-body* (c/Poly-fresh-symbols* fexpr-type) fexpr-type)
              (c/PolyDots-body* (c/PolyDots-fresh-symbols* fexpr-type) fexpr-type))]
    (app-type-error fexpr args fin arg-ret-types expected fexpr-type)))

(defn ^String plainapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [(r/FnIntersection? fexpr-type)]
   :post [(r/TCResult? %)]}
  (app-type-error fexpr args fexpr-type arg-ret-types expected false))

(declare invoke-keyword)

(defn ifn-ancestor 
  "If this type can be treated like a function, return one of its
  possibly polymorphic function ancestors.
  
  Assumes the type is not a union"
  [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [t (c/fully-resolve-type t)]
    (cond
      (r/RClass? t)
      (first (filter (some-fn r/Poly? r/FnIntersection?) (c/RClass-supers* t)))
      ;handle other types here
      )))

; Expr Expr^n TCResult TCResult^n (U nil TCResult) -> TCResult
(defn check-funapp [fexpr args fexpr-ret-type arg-ret-types expected]
  {:pre [(TCResult? fexpr-ret-type)
         (every? TCResult? arg-ret-types)
         ((some-fn nil? TCResult?) expected)]
   :post [(TCResult? %)]}
  (u/p :check/check-funapp
  (let [fexpr-type (c/fully-resolve-type (ret-t fexpr-ret-type))
        arg-types (mapv ret-t arg-ret-types)]
    (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                             (when fexpr
                               (expr-ns fexpr)))
    ;(prn "check-funapp" (prs/unparse-type fexpr-type) (map prs/unparse-type arg-types))
    (cond
      ;; a union of functions can be applied if we can apply all of the elements
      (r/Union? fexpr-type)
      (ret (reduce (fn [t ftype]
                     {:pre [(r/Type? t)
                            (r/Type? ftype)]
                      :post [(r/Type? %)]}
                     (c/Un t (ret-t (check-funapp fexpr args (ret ftype) arg-ret-types expected))))
                   (c/Un)
                   (:types fexpr-type)))

      (ifn-ancestor fexpr-type)
      (check-funapp fexpr args (ret (ifn-ancestor fexpr-type)) arg-ret-types expected)

      ;keyword function
      (c/keyword-value? fexpr-type)
      (let [[target-ret default-ret & more-args] arg-ret-types]
        (assert (empty? more-args))
        (invoke-keyword fexpr-ret-type target-ret default-ret expected))

      ;set function
      ;FIXME yuck. Also this is wrong, should be APersistentSet or something that *actually* extends IFn
      (and (r/RClass? fexpr-type)
           (isa? (u/symbol->Class (.the-class ^RClass fexpr-type)) IPersistentSet))
      (do
        (when-not (#{1} (count args))
          (u/tc-delayed-error "Wrong number of arguments to set function (" (count args)")"))
        (ret r/-any))

      ;FIXME same as IPersistentSet case
      (and (r/RClass? fexpr-type)
           (isa? (u/symbol->Class (.the-class ^RClass fexpr-type)) IPersistentMap))
      ;rewrite ({..} x) as (f {..} x), where f is some dummy fn
      (let [mapfn (prs/parse-type '(All [x] [(clojure.lang.IPersistentMap Any x) Any -> (U nil x)]))]
        (check-funapp fexpr args (ret mapfn) (concat [fexpr-ret-type] arg-ret-types) expected))

      ;Symbol function
      (and (r/RClass? fexpr-type)
           ('#{clojure.lang.Symbol} (.the-class ^RClass fexpr-type)))
      (let [symfn (prs/parse-type '(All [x] [(U (clojure.lang.IPersistentMap Any x) Any) -> (U x nil)]))]
        (check-funapp fexpr args (ret symfn) arg-ret-types expected))
      
      ;Var function
      (and (r/RClass? fexpr-type)
           ('#{clojure.lang.Var} (:the-class ^RClass fexpr-type)))
      (let [{[_ ftype :as poly?] :poly?} fexpr-type
            _ (assert (#{2} (count poly?))
                      "Assuming clojure.lang.Var only takes 1 argument")]
        (check-funapp fexpr args (ret ftype) arg-ret-types expected))

      ;Error is perfectly good fn type
      (r/TCError? fexpr-type)
      (ret r/Err)

      ;ordinary Function, single case, special cased for improved error msgs
      (and (r/FnIntersection? fexpr-type)
           (let [[{:keys [drest] :as ft} :as ts] (:types fexpr-type)]
             (and (= 1 (count ts))
                  (not drest))))
      (u/p :check/funapp-single-arity-nopoly-nodots
      (let [argtys arg-ret-types
            {[t] :types} fexpr-type]
        (check-funapp1 fexpr args t argtys expected)))

      ;ordinary Function, multiple cases
      (r/FnIntersection? fexpr-type)
      (u/p :check/funapp-nopoly-nodots
      (let [ftypes (:types fexpr-type)
            matching-fns (filter (fn [{:keys [dom rest] :as f}]
                                   {:pre [(r/Function? f)]}
                                   (sub/subtypes-varargs? arg-types dom rest))
                                 ftypes)
            success-ret-type (when-let [f (first matching-fns)]
                               (check-funapp1 fexpr args f arg-ret-types expected :check? false))]
        (if success-ret-type
          success-ret-type
          (plainapp-type-error fexpr args fexpr-type arg-ret-types expected))))

      ;ordinary polymorphic function without dotted rest
      (when (r/Poly? fexpr-type)
        (let [names (c/Poly-fresh-symbols* fexpr-type)
              body (c/Poly-body* names fexpr-type)]
          (when (r/FnIntersection? body)
            (every? (complement :drest) (:types body)))))
      (u/p :check/funapp-poly-nodots
      (let [fs-names (c/Poly-fresh-symbols* fexpr-type)
            _ (assert (every? symbol? fs-names))
            ^FnIntersection fin (c/Poly-body* fs-names fexpr-type)
            bbnds (c/Poly-bbnds* fs-names fexpr-type)
            _ (assert (r/FnIntersection? fin))
            ;; Only infer free variables in the return type
            ret-type 
            (free-ops/with-bounded-frees (zipmap (map r/F-maker fs-names) bbnds)
                     (loop [[{:keys [dom rng rest drest kws] :as ftype} & ftypes] (.types fin)]
                       (when ftype
                         #_(prn "infer poly fn" (prs/unparse-type ftype) (map prs/unparse-type arg-types)
                                (count dom) (count arg-types))
                         #_(prn ftype)
                         #_(when rest (prn "rest" (prs/unparse-type rest)))
                         ;; only try inference if argument types are appropriate
                         (if-let 
                           [substitution 
                            (cgen/handle-failure
                              (cond
                                ;possibly present rest argument, or no rest parameter
                                (and (not (or drest kws))
                                     ((if rest <= =) (count dom) (count arg-types)))
                                (cgen/infer-vararg (zipmap fs-names bbnds) {} 
                                                   arg-types dom rest (r/Result-type* rng)
                                                   (and expected (ret-t expected)))

                                ;keyword parameters
                                kws
                                (let [{:keys [mandatory optional]} kws
                                      [normal-argtys flat-kw-argtys] (split-at (count dom) arg-types)
                                      _ (when-not (even? (count flat-kw-argtys))
                                          (u/int-error (str "Uneven number of keyword arguments "
                                                            "provided to polymorphic function "
                                                            "with keyword parameters.")))
                                      paired-kw-argtys (apply hash-map flat-kw-argtys)

                                      ;generate two vectors identical in length with actual kw val types
                                      ;on the left, and expected kw val types on the right.

                                      [kw-val-actual-tys kw-val-expected-tys]
                                      (reduce (fn [[kw-val-actual-tys kw-val-expected-tys]
                                                   [kw-key-t kw-val-t]]
                                                {:pre [(vector? kw-val-actual-tys)
                                                       (vector? kw-val-expected-tys)
                                                       (Type? kw-key-t)
                                                       (Type? kw-val-t)]
                                                 :post [((u/hvector-c? (every-pred vector? (u/every-c? Type?)) 
                                                                       (every-pred vector? (u/every-c? Type?)))
                                                         %)]}
                                                (when-not (r/Value? kw-key-t)
                                                  (u/int-error 
                                                    (str "Can only check keyword arguments with Value keys, found"
                                                         (pr-str (prs/unparse-type kw-key-t)))))
                                                (let [expected-val-t ((some-fn optional mandatory) kw-key-t)]
                                                  (if expected-val-t
                                                    [(conj kw-val-actual-tys kw-val-t)
                                                     (conj kw-val-expected-tys expected-val-t)]
                                                    (do 
                                                      ; Using undeclared keyword keys is an error because we want to treat
                                                      ; the rest param as a complete hash map when checking 
                                                      ; fn bodies.
                                                      (u/tc-delayed-error (str "Undeclared keyword parameter " 
                                                                               (pr-str (prs/unparse-type kw-key-t))))
                                                      [(conj kw-val-actual-tys kw-val-t)
                                                       (conj kw-val-expected-tys r/-any)]))))
                                              [[] []]
                                              paired-kw-argtys)]
                                  ;make sure all mandatory keys are present
                                  (when-let [missing-ks (seq 
                                                          (set/difference (set (keys mandatory))
                                                                          (set (keys paired-kw-argtys))))]
                                    ; move to next arity
                                    (cgen/fail! nil nil))
                                    ;(u/tc-delayed-error (str "Missing mandatory keyword keys: "
                                    ;                         (pr-str (vec (interpose ", "
                                    ;                                                 (map prs/unparse-type missing-ks))))))
                                  ;; it's probably a bug to not infer for unused optional args, revisit this
                                  ;(when-let [missing-optional-ks (seq
                                  ;                                 (set/difference (set (keys optional))
                                  ;                                                 (set (keys paired-kw-argtys))))]
                                  ;  (u/nyi-error (str "NYI POSSIBLE BUG?! Unused optional parameters"
                                  ;                    (pr-str (interpose ", " (map prs/unparse-type missing-optional-ks)))))
                                  ;  )
                                  ; infer keyword and fixed parameters all at once
                                  (cgen/infer (zipmap fs-names bbnds) {}
                                              (concat normal-argtys kw-val-actual-tys)
                                              (concat dom kw-val-expected-tys) 
                                              (r/Result-type* rng)
                                              (and expected (ret-t expected))))))]
                           (do #_(prn "subst:" substitution)
                               (check-funapp1 fexpr args (subst/subst-all substitution ftype)
                                              arg-ret-types expected :check? false))
                           (if drest
                             (do (u/tc-delayed-error (str "Cannot infer arguments to polymorphic functions with dotted rest"))
                                 nil)
                             (recur ftypes))))))]
        (if ret-type
          ret-type
          (polyapp-type-error fexpr args fexpr-type arg-ret-types expected))))

      :else ;; any kind of dotted polymorphic function without mandatory or optional keyword args
      (if-let [[pbody fixed-vars fixed-bnds dotted-var dotted-bnd]
               (and (r/PolyDots? fexpr-type)
                    (let [vars (vec (c/PolyDots-fresh-symbols* fexpr-type))
                          bbnds (c/PolyDots-bbnds* vars fexpr-type)
                          [fixed-bnds dotted-bnd] [(butlast bbnds) (last bbnds)]
                          [fixed-vars dotted-var] [(butlast vars) (last vars)]
                          pbody (c/PolyDots-body* vars fexpr-type)]
                      (and (r/FnIntersection? pbody)
                           (seq (:types pbody))
                           (not (some :kws (:types pbody)))
                           [pbody fixed-vars fixed-bnds dotted-var dotted-bnd])))]
        (let [;_ (prn "polydots, no kw args")
              inferred-rng 
              (free-ops/with-bounded-frees (zipmap (map r/F-maker fixed-vars) fixed-bnds)
                           (some identity
                                 (for [{:keys [dom rest ^DottedPretype drest rng] :as ftype} (:types pbody)
                                       ;only try inference if argument types match
                                       :when (cond
                                               rest (<= (count dom) (count arg-types))
                                               drest (and (<= (count dom) (count arg-types))
                                                          (= dotted-var (-> drest :name)))
                                               :else (= (count dom) (count arg-types)))]
                                   (cgen/handle-failure
                                     ;(prn "Inferring dotted fn" (prs/unparse-type ftype))
                                     ;; Only try to infer the free vars of the rng (which includes the vars
                                     ;; in filters/objects).
                                     (let [substitution (cond
                                                          drest (cgen/infer-dots (zipmap fixed-vars fixed-bnds) dotted-var dotted-bnd
                                                                                 arg-types dom (.pre-type drest) (r/Result-type* rng) 
                                                                                 (frees/fv rng)
                                                                                 :expected (and expected (ret-t expected)))
                                                          rest (cgen/infer-vararg (zipmap fixed-vars fixed-bnds) {dotted-var dotted-bnd}
                                                                                  arg-types dom rest (r/Result-type* rng)
                                                                                  (and expected (ret-t expected)))
                                                          :else (cgen/infer (zipmap fixed-vars fixed-bnds) {dotted-var dotted-bnd} 
                                                                            arg-types dom (r/Result-type* rng)
                                                                            (and expected (ret-t expected))))
                                           ;_ (prn "substitution:" substitution)
                                           substituted-type (subst/subst-all substitution ftype)
                                           ;_ (prn "substituted-type" (prs/unparse-type substituted-type))
                                           ;_ (prn "args" (map prs/unparse-type arg-types))
                                           ]
                                       (or (and substitution
                                                (check-funapp1 fexpr args 
                                                               substituted-type arg-ret-types expected :check? false))
                                           (u/tc-delayed-error "Error applying dotted type")
                                           nil))))))]
          ;(prn "inferred-rng"inferred-rng)
          (if inferred-rng
            inferred-rng
            (polyapp-type-error fexpr args fexpr-type arg-ret-types expected)))

        (u/tc-delayed-error (str
                              "Cannot invoke type: " (pr-str (prs/unparse-type fexpr-type)))
                            :return (or expected (ret (c/Un))))))))))

(add-check-method :var
  [{:keys [var] :as expr} & [expected]]
  (binding [vs/*current-expr* expr]
    (let [id (u/var->symbol var)
          _ (when-not (var-env/used-var? id)
              (var-env/add-used-var id))
          t (var-env/lookup-Var-nofail (u/var->symbol var))]
      (if t
        (assoc expr
               expr-type (ret t (fo/-FS fl/-top fl/-top) obj/-empty))
        (u/tc-delayed-error
          (str "Unannotated var " id
               "\nHint: Add the annotation for " id
               " via check-ns or cf")
          :return (assoc expr
                         expr-type 
                         (ret (or (when expected
                                    (ret-t expected))
                                  (r/TCError-maker))
                              (fo/-FS fl/-top fl/-top) 
                              obj/-empty)))))))

(add-check-method :the-var
  [{:keys [^Var var env] :as expr} & [expected]]
  {:pre [(var? var)]}
  (let [id (u/var->symbol var)
        macro? (.isMacro var)
        _ (when-not (or macro?
                        (var-env/used-var? id))
            (var-env/add-used-var id))
        t (var-env/lookup-Var-nofail id)
        t (cond
            t t
            macro? r/-any
            :else (u/tc-delayed-error (str "Untyped var reference: " id
                                           "\nHint: Add the annotation for " id
                                           " via check-ns or cf")
                                      :form (u/emit-form-fn expr)
                                      :return (r/TCError-maker)))]
    (assoc expr
           expr-type (ret (c/RClass-of Var [t t])
                          (fo/-true-filter)
                          obj/-empty))))

;[Any TCResult * -> TCResult]
(defn tc-equiv [comparator & vs]
  {:pre [(every? TCResult? vs)]
   :post [(TCResult? %)]}
  (assert (#{:=} comparator))
  (assert (seq vs))
  (let [; TODO sequence behaviour is subtle
        ; conservative for now
        equiv-able (fn [t]
                     (boolean
                       (when (r/Value? t)
                         ((some-fn number? symbol? keyword? nil? true? false? class?) (.val ^Value t)))))
        vs-combinations (comb/combinations vs 2)
        ;_ (prn vs-combinations)
        then-filter (apply fo/-and (apply concat
                                          (for [[{t1 :t fl1 :fl o1 :o}
                                                 {t2 :t fl2 :fl o2 :o}] vs-combinations]
                                            (concat
                                              (when (equiv-able t1) 
                                                [(fo/-filter-at t1 o2)])
                                              (when (equiv-able t2) 
                                                [(fo/-filter-at t2 o1)])))))
        ;_ (prn then-filter)
        else-filter (apply fo/-or 
                           (if-let [fs (seq (apply concat
                                                   (for [[{t1 :t fl1 :fl o1 :o}
                                                          {t2 :t fl2 :fl o2 :o}] vs-combinations]
                                                     (concat
                                                       (when (equiv-able t1) 
                                                         [(fo/-not-filter-at t1 o2)])
                                                       (when (equiv-able t2) 
                                                         [(fo/-not-filter-at t2 o1)])))))]
                             fs
                             ; ensure we don't simplify to ff if we have more than one
                             ; argument to = (1 arg is always a true value)
                             (when (< 1 (count vs))
                               [fl/-top])))
        ;_ (prn else-filter)
        ]
    (ret (c/Un r/-false r/-true)
         (fo/-FS then-filter else-filter))))

(comment
  (tc-equiv (ret (r/-val :if))
            (ret (prs/parse-type '(U ':if ':case))
                 ))

  )

(defmulti invoke-special (fn [expr & args] 
                           (when-let [var (-> expr :fexpr :var)]
                             (u/var->symbol var))))
(u/add-defmethod-generator invoke-special)

(defmulti invoke-apply (fn [expr & args]
                         (when-let [var (-> expr :fexpr :var)]
                           (u/var->symbol var))))
(u/add-defmethod-generator invoke-apply)

(defmulti static-method-special (fn [{{:keys [declaring-class name]} :method} & args]
                                  (symbol (str declaring-class) (str name))))
(defmulti instance-method-special (fn [{{:keys [declaring-class name]} :method} & args]
                                    (symbol (str declaring-class) (str name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups

;[Type TCResult -> Type]
(defn- extend-method-expected 
  "Returns the expected type with target-type intersected with the first argument"
  [target-type expected]
  {:pre [(Type? target-type)
         (Type? expected)]
   :post [(Type? %)]}
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
    :else (u/int-error (str "Expected Function type, found " (prs/unparse-type expected)))))

(declare normal-invoke)

(add-invoke-special-method 'clojure.core.typed/var>*
  [{[sym-expr] :args :keys [args fexpr] :as expr} & [expected]]
  (when-not (#{1} (count args))
    (u/int-error (str "Wrong number of arguments to clojure.core.typed/var>,"
                      " expected 1, given " (count args))))
  (assert (#{:constant} (:op sym-expr)))
  (let [sym (-> sym-expr :val)
        _ (assert (symbol? sym))
        t (var-env/lookup-Var-nofail sym)
        _ (when-not t
            (u/tc-delayed-error (str "Unannotated var: " sym)))
        _ (when (and t expected)
            (when-not (sub/subtype? t (ret-t expected))
              (expected-error t (ret-t expected))))]
    (assoc expr
           expr-type (ret (or t (r/TCError-maker))))))

; ignore some keyword argument related intersections
(add-invoke-special-method 'clojure.core/seq?
  [{:keys [args fexpr] :as expr} & [expected]]
  (when-not (#{1} (count args))
    (u/int-error (str "Wrong number of arguments to clojure.core/seq?,"
                      " expected 1, given " (count args))))
  (let [[ctarget :as cargs] (map check args)]
    (cond 
      ; handle keyword args macroexpansion
      (r/KwArgsSeq? (-> ctarget expr-type ret-t))
      (assoc expr
             expr-type (ret r/-true (fo/-true-filter)))
      ; records never extend ISeq
      (r/Record? (-> ctarget expr-type ret-t c/fully-resolve-type))
      (assoc expr
             expr-type (ret r/-false (fo/-false-filter)))
      :else (normal-invoke expr fexpr args expected
                           :cargs cargs))))

(add-invoke-special-method 'clojure.core/extend
  [{[atype & protos :as args] :args :as expr} & [expected]]
  (when-not (and atype (even? (count protos))) 
    (u/int-error "Wrong number of arguments to extend, expected at least one with an even "
                 "number of variable arguments, given " (count args)))
  (let [catype (check atype)
        ret-expr (assoc expr
                        expr-type (ret r/-any))
        ; this is a Value type containing a java.lang.Class instance representing
        ; the type extending the protocol, or (Value nil) if extending to nil
        target-literal-class (ret-t (expr-type catype))]
    (cond
      (not (and (r/Value? target-literal-class)
                ((some-fn class? nil?) (:val target-literal-class))))
      (u/tc-delayed-error
        (str "Must provide a Class or nil as first argument to extend, "
             "got " (pr-str (prs/unparse-type target-literal-class)))
        :return ret-expr)

      (and expected (not (sub/subtype? r/-any (ret-t expected))))
      (do (expected-error r/-any (ret-t expected))
          ret-expr)
      :else
      (let [; this is the actual core.typed type of the thing extending the protocol
            target-type (let [v (:val target-literal-class)]
                          (if (nil? v)
                            r/-nil
                            (c/RClass-of-with-unknown-params v)))

            ; build expected types for each method map
            extends (into {}
                          (for [[prcl-expr mmap-expr] (apply hash-map protos)]
                            (let [protocol (do (when-not (= :var (:op prcl-expr))
                                                 (u/int-error  "Must reference protocol directly with var in extend"))
                                               (ptl-env/resolve-protocol (u/var->symbol (:var prcl-expr))))
                                  expected-mmap (c/make-HMap {}
                                                             ;get all combinations
                                                             (into {}
                                                                   (for [[msym mtype] (:methods protocol)]
                                                                     [(r/-val (keyword (name msym))) 
                                                                      (extend-method-expected target-type mtype)])))]
                              [protocol [mmap-expr expected-mmap]])))
            _ (doseq [[protocol [mmap-expr expected-hmap]] extends]
                (let [cmmap-expr (check mmap-expr (ret expected-hmap))
                      actual (-> cmmap-expr expr-type ret-t)]))]
        ret-expr))))

;into-array>
;
; Usage: (into-array> javat cljt coll)
;        (into-array> cljt coll)
(add-invoke-special-method 'clojure.core.typed/into-array>*
  [{:keys [args] :as expr} & [expected]]
  (when-not (#{2 3 4} (count args)) 
    (u/int-error "Wrong number of args to into-array>*"))
  (let [has-java-syn? (#{3 4} (count args))
        [javat-syn cljt-syn coll-expr]
        (cond 
          (= 3 (count args)) args
          (= 4 (count args)) (next args) ;handle temporary hacky case
          :else (cons nil args))

        javat (let [syn (or (when has-java-syn? (:val javat-syn))  ; generalise javat-syn if provided, otherwise cljt-syn
                            (:val cljt-syn))
                    c (-> 
                        (binding [prs/*parse-type-in-ns* (expr-ns expr)]
                          (prs/parse-type syn))
                        arr-ops/Type->array-member-Class)]
                (assert (class? c))
                c)
        cljt (binding [prs/*parse-type-in-ns* (expr-ns expr)]
               (prs/parse-type (:val cljt-syn)))
        ccoll (check coll-expr (ret (c/Un r/-nil (c/RClass-of Seqable [cljt]))))]
    (assoc expr
           expr-type (ret (r/PrimitiveArray-maker javat cljt cljt)))))

;not
(add-invoke-special-method 'clojure.core/not
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (= 1 (count args)) (u/error-msg "Wrong number of args to clojure.core/not"))
  (let [ctarget (check (first args))
        {fs+ :then fs- :else} (-> ctarget expr-type ret-f)]
    (assoc expr
           expr-type (ret (prs/parse-type 'boolean) 
                          ;flip filters
                          (fo/-FS fs- fs+)
                          obj/-empty))))

(defn invoke-get [{:keys [args] :as expr} expected & {:keys [cargs]}]
  {:post [((some-fn 
             #(-> % expr-type TCResult?)
             #{::not-special})
           %)]}
  (assert cargs)
  (assert (#{2 3} (count args)) "Wrong number of args to clojure.core/get")
  (let [[ctarget ckw cdefault] cargs
        kwr (expr-type ckw)]
    (cond
      (c/keyword-value? (ret-t kwr))
      (assoc expr
             expr-type (invoke-keyword kwr
                                       (expr-type ctarget)
                                       (when cdefault
                                         (expr-type cdefault))
                                       expected))

      ((every-pred r/Value? (comp integer? :val)) (ret-t kwr))
      (u/nyi-error (str "get lookup of vector (like nth) NYI"))

      :else ::not-special)))

;get
(add-invoke-special-method 'clojure.core/get
  [{:keys [args fexpr] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [cargs (mapv check args)
        r (invoke-get expr expected :cargs cargs)]
    (if-not (#{::not-special} r)
      r
      (normal-invoke expr fexpr args expected
                     :cargs cargs))))

(declare check-invoke-method)

(defmethod static-method-special 'clojure.lang.RT/get
  [{:keys [args target] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [cargs (mapv check args)
        r (invoke-get expr expected :cargs cargs)]
    (if-not (#{::not-special} r)
      r
      (check-invoke-method expr expected false
                           :cargs cargs))))

;FIXME should be the same as (apply hash-map ..) in invoke-apply
(defmethod static-method-special 'clojure.lang.PersistentHashMap/create
  [{:keys [args] :as expr} & [expected]]
  (binding [vs/*current-expr* expr]
    (let [_ (assert (#{1} (count args)) (u/error-msg "Incorrect number of arguments to clojure.lang.PersistentHashMap/create"))
          targett (-> (first args) check expr-type ret-t)]
      (cond
        (r/KwArgsSeq? targett)
        (assoc expr
               expr-type (ret (c/KwArgsSeq->HMap targett)))
        (not (r/HeterogeneousSeq? targett))
        (u/tc-delayed-error (str "Must pass HeterogeneousSeq to clojure.lang.PersistentHashMap/create given "
                                 (prs/unparse-type targett)
                                 "\n\nHint: Check the expected type of the function and the actual argument list for any differences. eg. extra undeclared arguments"
                                 "\n\nForm:\n\t"
                                 (u/emit-form-fn expr))
                            :return (assoc expr
                                           expr-type (error-ret expected)))
        :else
        (let [res (reduce (fn [t [kt vt]]
                            {:pre [(Type? t)]}
                            ;preserve bottom
                            (if (= (c/Un) vt)
                              vt
                              (do (assert (r/HeterogeneousMap? t))
                                  (assoc-in [:types kt] vt))))
                          (c/-complete-hmap {}) (.types ^HeterogeneousSeq targett))]
          (assoc expr
                 expr-type (ret res)))))))

(add-check-method :keyword-invoke
  [{:keys [kw target] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (assoc expr
         expr-type (invoke-keyword (expr-type (check kw))
                                   (expr-type (check target))
                                   nil 
                                   expected)))

;TODO pass fexpr and args for better errors
;[Type Type (Option Type) -> Type]
(defn find-val-type [t k default]
  {:pre [(Type? t)
         (Type? k)
         ((some-fn nil? Type?) default)]
   :post [(Type? %)]}
  (let [t (c/fully-resolve-type t)]
    (cond
      ; propagate the error
      (r/TCError? t) t
      (r/Nil? t) (or default r/-nil)
      (r/AssocType? t) (let [t* (apply c/assoc-pairs-noret (:target t) (:entries t))]
                         (cond
                           (:dentries t) (do
                                           (prn "dentries NYI")
                                           r/-any)
                           (r/HeterogeneousMap? t*) (find-val-type t* k default)

                           (and (not t*)
                                (r/F? (:target t))
                                (every? c/keyword-value? (map first (:entries t))))
                           (let [hmap (apply c/assoc-pairs-noret (c/-partial-hmap {}) (:entries t))]
                             (if (r/HeterogeneousMap? hmap)
                               (find-val-type hmap k default)
                               r/-any))
                           :else r/-any))
      (r/HeterogeneousMap? t) (let [^HeterogeneousMap t t]
                                ; normal case, we have the key declared present
                                (if-let [v (get (.types t) k)]
                                  v
                                  ; if key is known absent, or we have a complete map, we know precisely the result.
                                  (if (or (contains? (.absent-keys t) k)
                                          (c/complete-hmap? t))
                                    (do
                                      #_(tc-warning
                                        "Looking up key " (prs/unparse-type k) 
                                        " in heterogeneous map type " (prs/unparse-type t)
                                        " that declares the key always absent.")
                                      (or default r/-nil))
                                    ; otherwise result is Any
                                    (do #_(tc-warning "Looking up key " (prs/unparse-type k)
                                                    " in heterogeneous map type " (prs/unparse-type t)
                                                    " which does not declare the key absent ")
                                        r/-any))))

      (r/Record? t) (find-val-type (c/Record->HMap t) k default)

      (r/Intersection? t) (apply c/In 
                               (for [t* (:types t)]
                                 (find-val-type t* k default)))
      (r/Union? t) (apply c/Un
                        (for [t* (:types t)]
                          (find-val-type t* k default)))
      (r/RClass? t)
      (->
        (check-funapp nil nil (ret (prs/parse-type 
                                     ;same as clojure.core/get
                                     '(All [x y]
                                           (Fn 
                                             ;no default
                                             [(clojure.lang.IPersistentSet x) Any -> (clojure.core.typed/Option x)]
                                             [nil Any -> nil]
                                             [(U nil (clojure.lang.ILookup Any x)) Any -> (U nil x)]
                                             [java.util.Map Any -> (U nil Any)]
                                             [String Any -> (U nil Character)]
                                             ;default
                                             [(clojure.lang.IPersistentSet x) Any y -> (U y x)]
                                             [nil Any y -> y]
                                             [(U nil (clojure.lang.ILookup Any x)) Any y -> (U y x)]
                                             [java.util.Map Any y -> (U y Any)]
                                             [String Any y -> (U y Character)]
                                             ))))
                      [(ret t) (ret (or default r/-nil))] nil)
        ret-t)
      :else r/-any)))

;[TCResult TCResult (Option TCResult) (Option TCResult) -> TCResult]
(defn invoke-keyword [kw-ret target-ret default-ret expected-ret]
  {:pre [(TCResult? kw-ret)
         (TCResult? target-ret)
         ((some-fn nil? TCResult?) default-ret)
         ((some-fn nil? TCResult?) expected-ret)]
   :post [(TCResult? %)]}
  (u/p :check/invoke-keyword
  (let [targett (c/-resolve (ret-t target-ret))
        kwt (ret-t kw-ret)
        defaultt (when default-ret
                   (ret-t default-ret))]
    (cond
      ;Keyword must be a singleton with no default
      (c/keyword-value? kwt)
      (let [{{path-hm :path id-hm :id :as o} :o} target-ret
            this-pelem (pe/->KeyPE (:val kwt))
            val-type (find-val-type targett kwt defaultt)]
        (if (not= (c/Un) val-type)
          (ret val-type
               (fo/-FS (if (obj/Path? o)
                         (fo/-filter val-type id-hm (concat path-hm [this-pelem]))
                         fl/-top)
                       (if (obj/Path? o)
                         (fo/-or (fo/-filter (c/-hmap {} #{kwt} true) id-hm path-hm) ; this map doesn't have a kwt key or...
                                 (fo/-filter (c/Un r/-nil r/-false) id-hm (concat path-hm [this-pelem]))) ; this map has a false kwt key
                         fl/-top))
               (if (obj/Path? o)
                 (update-in o [:path] #(seq (concat % [this-pelem])))
                 o))
          (do (tc-warning (str "Keyword lookup gave bottom type: "
                               (:val kwt) " " (prs/unparse-type targett)))
              (ret r/-any))))

      :else (u/int-error (str "keyword-invoke only supports keyword lookup, no default. Found " 
                              (prs/unparse-type kwt)))))))

;binding
;FIXME use `check-normal-def`
;FIXME record checked-var-def info
(add-invoke-special-method 'clojure.core/push-thread-bindings
  [{[bindings-expr & other-args] :args :as expr} & [expected]]
  (assert (empty? other-args))
  ; only support (push-thread-bindings (hash-map @~[var bnd ...]))
  ; like `binding`s expansion
  (assert (#{:invoke} (-> bindings-expr :op)))
  (assert (#{#'hash-map} (-> bindings-expr :fexpr :var)))
  (assert (even? (count (-> bindings-expr :args))))
  (let [new-bindings-exprs (apply hash-map (-> bindings-expr :args))
        _ (doseq [[{:keys [op var] :as var-expr} bnd-expr] new-bindings-exprs]
            (assert (#{:the-var} op))
            (let [expected (var-env/type-of (u/var->symbol var))
                  cexpr (check bnd-expr (ret expected))
                  actual (-> cexpr expr-type ret-t)]
              (when (not (sub/subtype? actual expected))
                (u/tc-delayed-error (str "Expected binding for "
                                         (u/var->symbol var)
                                         " to be: " (prs/unparse-type expected)
                                         ", Actual: " (prs/unparse-type actual))))))]
    (assoc expr
           expr-type (ret r/-any))))

(defn dummy-invoke-expr [fexpr args env]
  {:op :invoke
   :env env
   :fexpr fexpr
   :args args})

(defn dummy-fn-method-expr [body required-params rest-param env]
  {:op :fn-method
   :env env
   :body body
   :required-params required-params
   :rest-param rest-param})

(defn dummy-fn-expr [methods variadic-method env]
  {:op :fn-expr
   :env env
   :methods methods
   :variadic-method variadic-method})

(defn dummy-local-binding* [sym env]
  {:op :local-binding
   :env env
   :sym sym
   hygienic/hsym-key sym})

(defn dummy-local-binding-expr [sym env]
  {:op :local-binding-expr
   :env env
   :local-binding (dummy-local-binding* sym env)})

(defn dummy-var-expr [vsym env]
  (let [v (resolve vsym)]
    (assert (var? v))
    {:op :var
     :env env
     :var v}))

(defn dummy-constant-expr [c env]
  {:op :constant
   :env env
   :val c})

(defn swap!-dummy-arg-expr [env [target-expr & [f-expr & args]]]
  (assert f-expr)
  (assert target-expr)
  (let [; transform (swap! t f a ...) to (swap! t (fn [t'] (f t' a ...)))
        ;
        ;generate fresh symbol for function param
        sym (gensym 'swap-val)
        derefed-param (dummy-local-binding-expr sym env)
        ;
        ;dummy-fn is (fn [t'] (f t' a ...)) with hygienic bindings
        dummy-fn-expr (dummy-fn-expr
                        [; (fn [t'] (f t' a ...))
                         (dummy-fn-method-expr
                           ; (f t' a ...)
                           (dummy-invoke-expr f-expr
                                              (concat [derefed-param]
                                                      args)
                                              env)
                           [(dummy-local-binding* sym env)]
                           nil
                           env)]
                        nil
                        env)]
    dummy-fn-expr))

; Any Type Env -> Expr
(defn dummy-ann-form-expr [expr t env]
  (dummy-invoke-expr
    (dummy-var-expr
      'clojure.core.typed/ann-form*
      env)
    [expr
     (dummy-constant-expr
       (binding [t/*verbose-types* true]
         (prs/unparse-type t))
       env)]
    env))

;swap!
;
; attempt to rewrite a call to swap! to help type inference
(add-invoke-special-method 'clojure.core/swap!
  [{:keys [fexpr args env] :as expr} & [expected]]
  (let [target-expr (first args)
        ctarget-expr (check target-expr)
        target-t (-> ctarget-expr expr-type ret-t c/fully-resolve-type)
        deref-type (when (and (r/RClass? target-t)
                              (= 'clojure.lang.Atom (:the-class target-t)))
                     (when-not (= 2 (count (:poly? target-t)))
                       (u/int-error (str "Atom takes 2 arguments, found " (count (:poly? target-t)))))
                     (second (:poly? target-t)))
        ]
    (if deref-type
      (cond
        ; TODO if this is a lambda we can do better eg. (swap! (atom> Number 1) (fn [a] a))
        ;(#{:fn-expr} (:op (second args)))

        :else
          (let [dummy-arg (swap!-dummy-arg-expr env args)
                ;_ (prn (u/emit-form-fn dummy-arg) "\n" deref-type)
                expected-dummy-fn-type (r/make-FnIntersection
                                         (r/make-Function
                                           [deref-type]
                                           deref-type))
                delayed-errors (t/-init-delayed-errors)
                actual-dummy-fn-type 
                (binding [t/*delayed-errors* delayed-errors]
                  (-> (normal-invoke expr
                                     (dummy-var-expr
                                       'clojure.core/swap!
                                       env)
                                     [(first args)
                                      (dummy-ann-form-expr
                                        dummy-arg
                                        expected-dummy-fn-type
                                        env)]
                                     expected)
                      expr-type ret-t))]
            ;(prn "deref expected" deref-type)
            ;(prn "expected-dummy-fn-type" expected-dummy-fn-type)
            ;(prn "actual-dummy-fn-type" actual-dummy-fn-type)
            ;(prn "subtype?" (sub/subtype? actual-dummy-fn-type expected-dummy-fn-type))
            (if (seq @delayed-errors)
              :default
              (assoc expr
                     expr-type (ret deref-type)))))
      :default)))

;=
(add-invoke-special-method 'clojure.core/= 
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;identical
(defmethod static-method-special 'clojure.lang.Util/identical
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;equiv
(defmethod static-method-special 'clojure.lang.Util/equiv
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

(t/ann hvec->rets [HeterogeneousVector -> (Seqable TCResult)])
(defn hvec->rets [^HeterogeneousVector v]
  {:pre [(r/HeterogeneousVector? v)]
   :post [(every? TCResult? %)]}
  (map ret
       (.types v)
       (.fs v)
       (.objects v)))

(t/ann tc-isa? [TCResult TCResult -> TCResult])
(defn tc-isa? 
  "Type check a call to isa?. Assumes global hierarchy.
  Also supports the case where both elements are vectors, but not recursively."
  [child-ret parent-ret]
  {:pre [(TCResult? child-ret)
         (TCResult? parent-ret)]
   :post [(TCResult? %)]}
  (letfn> [fs :- [TCResult TCResult -> '{:then fprotocol/IFilter :else fprotocol/IFilter}]
           (fs [child1 parent1]
             {:pre [(TCResult? child1)
                    (TCResult? parent1)]
              :post [((u/hmap-c? :then fl/Filter? :else fl/Filter?) %)]}
             {:then (fo/-filter-at (ret-t parent1) (ret-o child1))
              :else (fo/-not-filter-at (ret-t parent1) (ret-o child1))})]
    (let [child-t (ret-t child-ret)
          parent-t (ret-t parent-ret)
          fs (cond
               ; interesting case with (isa? [...] [...])
               ; use each pairing between child and parent
               (and (r/HeterogeneousVector? child-t)
                    (r/HeterogeneousVector? parent-t))
               (let [individual-fs (map fs (hvec->rets child-t) (hvec->rets parent-t))]
                 (fo/-FS (apply fo/-and (map :then individual-fs))
                         (apply fo/-or (map :else individual-fs))))
               ; simple (isa? child parent) 
               :else (let [{:keys [then else]} (fs child-ret parent-ret)]
                       (fo/-FS then else)))]
      (ret (c/Un r/-true r/-false) fs obj/-empty))))


;isa? (2 arity is special)
(add-invoke-special-method 'clojure.core/isa?
  [{:keys [args] :as expr} & [expected]]
  (cond
    (#{2} (count args))
    (let [[cchild-expr cparent-expr :as cargs] (mapv check args)]
      (assoc expr
             expr-type (tc-isa? (expr-type cchild-expr)
                                (expr-type cparent-expr))))
    :else :default))

;FIXME need to review if any repeated "check"s happen between invoke-apply and specials
;apply
(add-invoke-special-method 'clojure.core/apply
  [expr & [expected]]
  ;(prn "special apply:")
  (let [e (invoke-apply expr expected)]
    (if (= e ::not-special)
      (u/tc-delayed-error (str "apply must be special: " (u/emit-form-fn expr))
                          :return (assoc expr
                                         expr-type (error-ret expected)))
      e)))

;manual instantiation
(add-invoke-special-method 'clojure.core.typed/inst-poly
  [{[pexpr targs-exprs :as args] :args :as expr} & [expected]]
  (assert (#{2} (count args)) "Wrong arguments to inst")
  (let [ptype (let [t (-> (check pexpr) expr-type ret-t)]
                (if (r/Name? t)
                  (c/resolve-Name t)
                  t))
        ; support (inst :kw ...)
        ptype (if (c/keyword-value? ptype)
                (c/KeywordValue->Fn ptype)
                ptype)]
    (if-not ((some-fn r/Poly? r/PolyDots?) ptype)
      (binding [vs/*current-expr* pexpr]
        (u/tc-delayed-error (str "Cannot instantiate non-polymorphic type: " (prs/unparse-type ptype))
                            :return (assoc expr
                                           expr-type (error-ret expected))))
      (let [targs (binding [prs/*parse-type-in-ns* (expr-ns expr)]
                    (doall (map prs/parse-type (:val targs-exprs))))]
        (assoc expr
               expr-type (ret (inst/manual-inst ptype targs)))))))

(defonce ^:dynamic *inst-ctor-types* nil)
(set-validator! #'*inst-ctor-types* (some-fn nil? (u/every-c? Type?)))

;manual instantiation for calls to polymorphic constructors
(add-invoke-special-method 'clojure.core.typed/inst-poly-ctor
  [{[ctor-expr targs-exprs] :args :as expr} & [expected]]
  (let [targs (binding [prs/*parse-type-in-ns* (expr-ns expr)]
                (mapv prs/parse-type (:val targs-exprs)))
        cexpr (binding [*inst-ctor-types* targs]
                (check ctor-expr))]
    (assoc expr
           expr-type (expr-type cexpr))))

(defn- print-lex-env [l]
  {:pre [(lex/lex-env? l)]}
  (prn (into {} (for [[k v] l]
                  [k (prs/unparse-type v)]))))

(defn- print-env*
  ([] (print-env* lex/*lexical-env*))
  ([e]
   {:pre [(lex/PropEnv? e)]}
   ;; DO NOT REMOVE
   (let [tvar-scope tvar-env/*current-tvars*
         tvar-bounds tvar-bnds/*current-tvar-bnds*
         scoped-names (keys tvar-scope)
         actual-names (map :name (vals tvar-scope))
         _ (every? symbol? actual-names)
         actual-bnds (map tvar-bounds actual-names)]
     (prn {:env (into {} (for [[k v] (:l e)]
                           [k (prs/unparse-type v)]))
           :props (map prs/unparse-filter (:props e))
           ;:frees (map (t/fn> 
           ;              [nme :- Symbol, bnd :- (U nil Bounds)]
           ;              {:pre [(symbol? nme)
           ;                     ((some-fn nil? r/Bounds?) bnd)]}
           ;              (if bnd
           ;                (prs/unparse-poly-bounds-entry nme bnd)
           ;                [nme 'NO-BOUNDS]))
           ;            scoped-names
           ;            actual-bnds)
           ;:tvar-scope tvar-scope
           ;:tvar-bnds tvar-bounds
           }))))

;debug printing
(add-invoke-special-method 'clojure.core.typed/print-env
  [{[debug-string :as args] :args :as expr} & [expected]]
  (assert (#{1} (count args)) (str "Wrong arguments to print-env, Expected 1, found " (count args)))
  (assert (= :string (:op debug-string)) "Must pass print-env a string literal")
  ;DO NOT REMOVE
  (println (:val debug-string))
  (flush)
  (print-env*)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (ret r/-nil (fo/-false-filter) obj/-empty)))

;filter printing
(add-invoke-special-method 'clojure.core.typed/print-filterset
  [{[debug-string form :as args] :args :as expr} & [expected]]
  (assert (#{2} (count args)) (str "Wrong arguments to print-filterset. Expected 2, found " (count args)))
  (assert (= :string (:op debug-string)) "Must pass print-filterset a string literal as the first argument.")
  (let [cform (check form expected)
        t (expr-type cform)]
    (assert (= :string (:op debug-string)))
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    ;(prn (:fl t))
    (if (fl/FilterSet? (:fl t))
      (do (pprint/pprint (prs/unparse-filter-set (:fl t)))
          (flush))
      (prn (:fl t)))
    (prn (prs/unparse-object (:o t)))
    (prn 'Flow (prs/unparse-filter (-> t :flow r/flow-normal)))
    ;DO NOT REMOVE
    (assoc expr
           expr-type t)))

;unsafe form annotation
(add-invoke-special-method 'clojure.core.typed/unsafe-ann-form*
  [{[frm {tsyn :val}] :args, :keys [env], :as expr} & [expected]]
  (let [parsed-ty (binding [vs/*current-env* env
                            prs/*parse-type-in-ns* (expr-ns expr)]
                    (prs/parse-type tsyn))]
    (assoc expr
           expr-type (ret parsed-ty))))

;form annotation
(add-invoke-special-method 'clojure.core.typed/ann-form*
  [{[frm {tsyn :val}] :args, :keys [env], :as expr} & [expected]]
  (let [parsed-ty (binding [vs/*current-env* env
                            prs/*parse-type-in-ns* (expr-ns expr)]
                    (prs/parse-type tsyn))
        cty (check frm (ret parsed-ty))
        checked-type (ret-t (expr-type cty))
        _ (binding [vs/*current-expr* frm]
            (when (not (sub/subtype? checked-type parsed-ty))
              (expected-error checked-type parsed-ty)))
        _ (when (and expected (not (sub/subtype? checked-type (ret-t expected))))
            (binding [vs/*current-expr* frm
                      vs/*current-env* env]
              (expected-error checked-type (ret-t expected))))]
    (assoc expr
           expr-type (ret parsed-ty))))

;fn literal
(add-invoke-special-method 'clojure.core.typed/fn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[fexpr {type-syns :val}] args
        expected
        (binding [prs/*parse-type-in-ns* (expr-ns expr)]
          (apply
            r/make-FnIntersection
            (doall
              (for [{:keys [dom-syntax has-rng? rng-syntax]} type-syns]
                (r/make-Function (mapv prs/parse-type dom-syntax)
                                 (if has-rng?
                                   (prs/parse-type rng-syntax)
                                   r/-any))))))]
    (check fexpr (ret expected))))

;polymorphic fn literal
(add-invoke-special-method 'clojure.core.typed/pfn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (assert false "pfn> NYI")
  #_(let [[fexpr {poly-decl :val} {method-types-syn :val}] args
        frees-with-bounds (map prs/parse-free poly-decl)
        method-types (free-ops/with-bounded-frees frees-with-bounds
                       (binding [prs/*parse-type-in-ns* (expr-ns expr)]
                         (doall 
                           (for [{:keys [dom-syntax has-rng? rng-syntax]} method-types-syn]
                             {:dom (doall (map prs/parse-type dom-syntax))
                              :rng (if has-rng?
                                     (prs/parse-type rng-syntax)
                                     r/-any)}))))
        cexpr (-> (check-anon-fn fexpr method-types :poly frees-with-bounds)
                  (update-in [expr-type :t] (fn [fin] (c/Poly* (map first frees-with-bounds) 
                                                             (map second frees-with-bounds)
                                                             fin))))]
    cexpr))

(defonce ^:dynamic *loop-bnd-anns* nil)
(set-validator! #'*loop-bnd-anns* #(or (nil? %)
                                       (every? Type? %)))

;loop
(add-invoke-special-method 'clojure.core.typed/loop>-ann
  [{:keys [args] :as expr} & [expected]]
  (let [[expr {expected-bnds-syn :val}] args
        expected-bnds (binding [prs/*parse-type-in-ns* (expr-ns expr)]
                        (mapv prs/parse-type expected-bnds-syn))]
    ;loop may be nested, type the first loop found
    (binding [*loop-bnd-anns* expected-bnds]
      (check expr expected))))

;don't type check
(add-invoke-special-method 'clojure.core.typed/tc-ignore-forms*
  [{:keys [fexpr args] :as expr} & [expected]]
  (assoc expr
         expr-type (ret r/-any)))

;seq
(add-invoke-special-method 'clojure.core/seq
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [_ (assert (#{1} (count args))
                  "Wrong number of arguments to seq")
        [ccoll :as cargs] (mapv check args)]
    ;(prn "special seq: ccoll type" (prs/unparse-type (ret-t (expr-type ccoll))))
    (cond
      ; for (apply hash-map (seq kws)) macroexpansion of keyword args
      (r/KwArgsSeq? (ret-t (expr-type ccoll)))
      (assoc expr
             expr-type (expr-type ccoll))

      ((some-fn r/HeterogeneousVector? 
                r/HeterogeneousList? 
                r/HeterogeneousSeq?)
       (expr-type ccoll))
      (assoc expr
             expr-type (ret (if-let [ts (seq (:types (expr-type ccoll)))]
                              (r/HeterogeneousSeq-maker ts)
                              r/-nil)))
      :else (normal-invoke expr fexpr args expected :cargs cargs))))

;make vector
(add-invoke-special-method 'clojure.core/vector
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (ret (r/-hvec (mapv (comp ret-t expr-type) cargs)
                                   :filters (mapv (comp ret-f expr-type) cargs)
                                   :objects (mapv (comp ret-o expr-type) cargs))))))

;make hash-map
(add-invoke-special-method 'clojure.core/hash-map
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (every? r/Value? (keys (apply hash-map (mapv (comp ret-t expr-type) cargs))))
      (assoc expr
             expr-type (ret (c/-complete-hmap
                              (apply hash-map (mapv (comp ret-t expr-type) cargs)))))
      :else (normal-invoke expr fexpr args expected :cargs cargs))))

;apply hash-map
(add-invoke-apply-method 'clojure.core/hash-map
  [{[_ & args] :args :as expr} & [expected]]
  (let [cargs (mapv check args)]
    #_(prn "apply special (hash-map): "
           (map (comp prs/unparse-type ret-t expr-type) cargs))
    (cond
      (and (#{1} (count cargs))
           (r/KwArgsSeq? (expr-type (last cargs))))
      (assoc expr
             expr-type (ret (c/KwArgsSeq->HMap (-> (expr-type (last cargs)) ret-t))))

      (and ((some-fn r/HeterogeneousVector? r/HeterogeneousList? r/HeterogeneousSeq?) 
            (expr-type (last cargs)))
           ;; every key must be a Value
           (every? r/Value? (keys (apply hash-map (concat (map expr-type (butlast cargs))
                                                        (mapcat vector (:types (expr-type (last cargs)))))))))
      (assoc expr
             expr-type (ret (c/-complete-hmap
                              (apply hash-map (concat (map expr-type (butlast cargs))
                                                      (mapcat vector (:types (expr-type (last cargs)))))))))
      :else ::not-special)))

(defn invoke-nth [{:keys [args] :as expr} expected & {:keys [cargs]}]
  (let [_ (assert (#{2 3} (count args)) (str "nth takes 2 or 3 arguments, actual " (count args)))
        [te ne de :as cargs] (or cargs (doall (map check args)))
        types (let [ts (c/fully-resolve-type (ret-t (expr-type te)))]
                (if (r/Union? ts)
                  (:types ts)
                  [ts]))
        num-t (ret-t (expr-type ne))
        default-t (when de
                    (ret-t (expr-type de)))]
    (cond
      (and (r/Value? num-t)
           (integer? (:val num-t))
           (every? (some-fn r/Nil?
                            r/HeterogeneousVector?
                            r/HeterogeneousList?
                            r/HeterogeneousSeq?)
                   types))
      (assoc expr
             expr-type (ret (apply c/Un
                                   (doall
                                     (for [t types]
                                       (if-let [res-t (cond
                                                        (r/Nil? t) (or default-t r/-nil)
                                                        ; nil on out-of-bounds and no default-t
                                                        :else (nth (:types t) (:val num-t) default-t))]
                                         res-t
                                         (u/int-error (str "Cannot get index " (:val num-t)
                                                           " from type " (prs/unparse-type t)))))))
                            (let [nnth (:val num-t)
                                  target-o (ret-o (expr-type te))
                                  default-o (when de
                                              (ret-o (expr-type de)))
                                  ;; We handle filters for both arities of nth here, with and without default
                                  ;;
                                  ;;With default:
                                  ;; if this is a true value either:
                                  ;;  * target is nil or seq and default is true
                                  ;;  * target is seqable, default is false
                                  ;;    and target is at least (inc nnth) count
                                  default-fs+ (fo/-or (fo/-and (fo/-filter-at (c/Un r/-nil (c/RClass-of ISeq [r/-any])) 
                                                                              target-o)
                                                               (fo/-not-filter-at (c/Un r/-false r/-nil) 
                                                                                  default-o))
                                                      (fo/-and (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                                                                    (r/make-CountRange (inc nnth)))
                                                                              target-o)
                                                               (fo/-filter-at (c/Un r/-false r/-nil) 
                                                                              default-o)))
                                  ;;Without default:
                                  ;; if this is a true value: 
                                  ;;  * target is seqable of at least nnth count
                                  nodefault-fs+ (fo/-filter-at (c/In (c/RClass-of Seqable [r/-any])
                                                                     (r/make-CountRange (inc nnth)))
                                                               target-o)]
                              (fo/-FS (if default-t
                                        default-fs+
                                        nodefault-fs+)
                                      ; not sure if there's anything worth encoding here
                                      fl/-top))))
      :else ::not-special)))

;nth
(defmethod static-method-special 'clojure.lang.RT/nth
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [cargs (doall (map check args))
        r (invoke-nth expr expected :cargs cargs)]
    (if-not (#{::not-special} r)
      r
      (check-invoke-method expr expected false
                           :cargs cargs))))

;assoc
(add-invoke-special-method 'clojure.core/assoc
  [{:keys [fexpr args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [[target & keyvals] args

        _ (when-not (<= 3 (count args))
            (u/tc-delayed-error (str "assoc accepts at least 3 arguments, found "
                                     (count args))))
        _ (when-not (even? (count keyvals))
            (u/tc-delayed-error "assoc accepts an even number of keyvals"))

        targetun (-> target check expr-type ret-t)
        ckeyvals (doall (map check keyvals))
        keypair-types (partition 2 (map expr-type ckeyvals))]
    (if-let [new-hmaps (apply c/assoc-type-pairs targetun keypair-types)]
      (assoc expr
        expr-type (ret new-hmaps
                       (fo/-true-filter) ;assoc never returns nil
                       obj/-empty))
      
      ;; to do: improve this error message
      (u/tc-delayed-error (str "Cannot assoc args `"
                               (clojure.string/join " "
                                 (map (comp prs/unparse-type expr-type) ckeyvals))
                               "` on "
                               (prs/unparse-type targetun))
                          :return (assoc expr
                                    expr-type (error-ret expected))))
    ))

(add-invoke-special-method 'clojure.core/dissoc
  [{:keys [fexpr args] :as expr} & [expected]]
  {:post [(or (= % :default) (-> % expr-type TCResult?))]}
  (let [[ctarget & cargs :as all-cargs] (doall (map check args))
        ttarget (-> ctarget expr-type ret-t)
        targs (map expr-type cargs)]
    (if-let [new-t (c/dissoc-keys ttarget targs)]
      (assoc expr expr-type (ret new-t))
      (normal-invoke expr fexpr args expected
                     :cargs all-cargs))))

; merge
(add-invoke-special-method 'clojure.core/merge
  [{:keys [fexpr args] :as expr} & [expected]]
  {:post [(or (= % :default) (-> % expr-type TCResult?))]}
  (let [[ctarget & cargs :as all-cargs] (doall (map check args))
        basemap (-> ctarget expr-type ret-t c/fully-resolve-type)
        targs (map expr-type cargs)]
    (if-let [merged (apply c/merge-types basemap targs)]
      (assoc expr expr-type (ret merged
                                 (fo/-true-filter) ;assoc never returns nil
                                 obj/-empty))
      (normal-invoke expr fexpr args expected
                     :cargs all-cargs))))

;conj
(add-invoke-special-method 'clojure.core/conj
  [{:keys [args fexpr] :as expr} & [expected]]
  (let [[ctarget & cargs :as all-cargs] (doall (map check args))
        ttarget (-> ctarget expr-type ret-t)
        targs (map expr-type cargs)]
    (if-let [conjed (apply c/conj-types ttarget targs)]
      (assoc expr expr-type (ret conjed
                                 (fo/-true-filter) ; conj never returns nil
                                 obj/-empty))
      (normal-invoke expr fexpr args expected
                     :cargs all-cargs))))

#_(add-invoke-special-method 'clojure.core/update-in
  [{:keys [fexpr args env] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [error-expr (assoc expr expr-type (ret (r/TCError-maker)))]
      (cond
        (not (< 3 (count args))) (u/tc-delayed-error (str "update-in takes at least 3 arguments"
                                                          ", actual " (count args))
                                                     :return error-expr)

        :else
        (let [[ctarget-expr cpath-expr cfn-expr & more-exprs] (doall (map check args))
              path-type (-> cpath-expr expr-type ret-t c/fully-resolve-type)]
          (if (not (HeterogeneousVector? path-type))
            (u/tc-delayed-error (str "Can only check update-in with vector as second argument")
                                :return error-expr)
            (let [path (:types path-type)
                  follow-path (reduce (fn [t pth]
                                        (when t
                                          ))
                                      (-> ctarget-expr expr-type ret-t)
                                      path)])))))))
        

(comment
  (method-expected-type (prs/parse-type '[Any -> Any])
                        (prs/parse-type '(Value :op))
                        (prs/parse-type '(Value :if)))
  ;=> ['{:if Any} -> Any]
  )

(defn- parse-fn-return-type [parse-fn-type]
  (let [subst-in (free-ops/with-free-symbols 
                   #{'a} (prs/parse-type '[String -> a]))] 
    (-> (cgen/cs-gen #{} {'a r/no-bounds} {} parse-fn-type subst-in) 
        (cgen/subst-gen #{} subst-in)
        (get-in ['a :type]))))

(defn vector-args [expr]
  (case (:op expr)
    :constant (when (vector? (:val expr))
                (map (fn [f] [f nil]) (:val expr)))
    :vector (doall
              (map (fn [arg-expr]
                     [(u/emit-form-fn arg-expr) arg-expr])
                   (:args expr)))
    nil))

; some code taken from tools.cli
; (All [x]
;   [CliSpec -> (U nil '[Value Type])])
(defn parse-cli-spec [spec-expr]
  (letfn [(opt? [^String x]
            (.startsWith x "-"))
          (name-for [k]
            (str/replace k #"^--no-|^--\[no-\]|^--|^-" "")) 
          (flag? [^String x]
              (.startsWith x "--[no-]"))]

  (let [; (U nil (Seqable '[Form (U nil Expr)]))
        raw-spec (vector-args spec-expr)]
    (cond
      (not raw-spec) (do
                       ;(prn "cli: not vector " spec-expr)
                       nil)
      :else
      (let [; each seq and map entry is a pair of [form expr]
            [switches raw-spec] (split-with (fn [[frm _]] (and (string? frm) (opt? frm))) raw-spec)
            [docs raw-spec]     (split-with (fn [[frm _]] (string? frm)) raw-spec)
            ; keys are [kw expr]
            options             (apply hash-map raw-spec)
            ; keys are keywords
            ; (Map Keyword [Form Expr])
            options             (into {}
                                      (for [[[kfrm _] v] options]
                                        [kfrm v]))
            ; (Seqable Form)
            aliases             (map (fn [[frm _]] (name-for frm)) switches)
            ; assume we fail later if there is anything ambiguous
            flag                (or (if (seq switches)
                                      (flag? (first (last switches)))
                                      :unknown)
                                    (when (contains? options :flag)
                                      (let [flg-form (first (:flag options))]
                                        (if (u/boolean? flg-form)
                                          flg-form
                                          :unknown)))
                                    false)]
        (cond
          ;not accurate enough, return nil
          (not-every? keyword? (keys options)) (do
                                                 ;(prn "cli: not every option key was keyword" options)
                                                 nil)
          (#{:unknown} flag) (do 
                               ;(prn "cli: flag unknown")
                               nil)
          (not
            (and (#{0 1} (count docs))
                 ((some-fn nil? string?) (-> docs first first)))) (do
                                                                    ;(prn "cli: docs" docs) 
                                                                    nil)
          (empty? aliases) (do
                             ;(prn "cli: empty aliases")
                             nil)
          :else
          (let [name (r/-val (keyword (last aliases)))
                default-type (when-let [[frm default-expr] (:default options)]
                               (if default-expr
                                 (-> (check default-expr)
                                     expr-type
                                     ret-t)
                                 (const/constant-type frm)))
                parse-fn-type (when-let [[pfrm parse-fn-expr] (:parse-fn options)]
                                (if parse-fn-expr
                                  (-> (check parse-fn-expr (ret (prs/parse-type
                                                                  '[String -> Any])))
                                      expr-type
                                      ret-t)
                                  (const/constant-type pfrm)))
                parse-fn-ret (when parse-fn-type
                               (parse-fn-return-type parse-fn-type))
                type (cond
                       (and parse-fn-type
                            (not parse-fn-ret)) (do
                                                  ;(prn "cli: parse-fn")
                                                  nil)
                       flag (c/RClass-of Boolean)
                       :else
                       (apply c/Un (concat (when default-type
                                             [default-type])
                                           (if parse-fn-type
                                             [parse-fn-ret]
                                             [(c/RClass-of String)]))))]
            (when type
              [name type]))))))))

; cli
(add-invoke-special-method 'clojure.tools.cli/cli
  [{[args-expr & specs-exprs] :args :keys [env] :as expr} & [expected]]
  (binding [vs/*current-env* env]
    (let [args-expected-ty (prs/parse-type '(U nil (clojure.lang.Seqable String)))
          cargs-expr (binding [vs/*current-env* (:env args-expr)]
                       (check args-expr))
          _ (when-not (sub/subtype? 
                        (-> cargs-expr expr-type ret-t)
                        args-expected-ty)
              (binding [vs/*current-env* (:env args-expr)]
                (expected-error (-> cargs-expr expr-type ret-t) args-expected-ty)))
          spec-map-ty (reduce (fn [t spec-expr]
                                (if-let [[keyt valt] (parse-cli-spec spec-expr)]
                                  (-> t
                                    (assoc-in [:types keyt] valt))
                                  ; resort to a general type
                                  (do
                                    ;(prn "cli: giving up because of" (u/emit-form-fn spec-expr)
                                         ;"\n" spec-expr)
                                    (reduced 
                                      (c/RClass-of IPersistentMap [(c/RClass-of clojure.lang.Keyword) r/-any])))))
                              (c/-complete-hmap {})
                              specs-exprs)

          actual (r/-hvec [spec-map-ty 
                           (prs/parse-type '(clojure.lang.Seqable String))
                           (prs/parse-type 'String)])
          _ (when expected
              (when-not (sub/subtype? actual (ret-t expected))
                (expected-error 
                  actual (ret-t expected))))]
      (assoc expr
             expr-type (ret actual)))))

(defonce ^:dynamic *current-mm* nil)
(set-validator! #'*current-mm* (some-fn nil? 
                                        (u/hmap-c? :dispatch-fn-type Type?
                                                   :dispatch-val-ret TCResult?)))

(defn default-defmethod? [var dispatch-val]
  {:pre [(var? var)]}
  (let [^clojure.lang.MultiFn multifn @var
        _ (assert (instance? clojure.lang.MultiFn multifn))
        default-val (.defaultDispatchVal multifn)]
    (= default-val dispatch-val)))

; FIXME this needs a line number from somewhere!
(defmethod instance-method-special 'clojure.lang.MultiFn/addMethod
  [{[dispatch-val-expr method-expr :as args] :args :keys [target env] :as expr} & [expected]]
  (assert (= 2 (count args)))
  (let [var (:var target)
        mmsym (u/var->symbol var)
        ret-expr (assoc expr
                        expr-type (ret (c/RClass-of clojure.lang.MultiFn)))
        default? (default-defmethod? var (u/emit-form-fn dispatch-val-expr))]
    (cond
      ;skip if warn-on-unannotated-vars is in effect
      (or (and (ns-opts/warn-on-unannotated-vars? (expr-ns expr))
               (not (var-env/lookup-Var-nofail mmsym)))
          (not (var-env/check-var? mmsym)))
      (do (println "<NO LINE NUMBER>: Not checking defmethod" mmsym "with dispatch value" (u/emit-form-fn dispatch-val-expr))
          (flush)
          ret-expr)
      :else
      (let [_ (assert (#{:var} (:op target)))
            _ (assert (#{:fn-expr} (:op method-expr))
                      "Method must be a fn")
            ctarget (check target)
            cdispatch-val-expr (check dispatch-val-expr)
            dispatch-type (mm/multimethod-dispatch-type mmsym)]
        (if-not dispatch-type
          (binding [vs/*current-env* env]
            (u/tc-delayed-error (str "Multimethod requires dispatch type: " mmsym
                                     "\n\nHint: defmulti must be checked before its defmethods")
                                :return ret-expr))
          (let [method-expected (var-env/type-of mmsym)
                cmethod-expr 
                (binding [*current-mm* (when-not default?
                                         {:dispatch-fn-type dispatch-type
                                          :dispatch-val-ret (expr-type cdispatch-val-expr)})]
                  (check method-expr (ret method-expected)))]
            ret-expr))))))

(add-invoke-special-method :default [& args] :default)
(defmethod static-method-special :default [& args] :default)
(defmethod instance-method-special :default [& args] :default)

(defn check-apply
  [{[fexpr & args] :args :as expr} expected]
  {:post [((some-fn TCResult? #{::not-special}) %)]}
  (let [ftype (ret-t (expr-type (check fexpr)))
        [fixed-args tail] [(butlast args) (last args)]]
    (cond
      ;apply of a simple function
      (r/FnIntersection? ftype)
      (do 
        (when (empty? (:types ftype))
          (u/int-error (str "Empty function intersection given as argument to apply")))
        (let [arg-tres (mapv check fixed-args)
              arg-tys (mapv (comp ret-t expr-type) arg-tres)
              tail-ty (ret-t (expr-type (check tail)))]
          (loop [[{:keys [dom rng rest drest]} :as fs] (:types ftype)]
            (cond
              ;we've run out of cases to try, so error out
              (empty? fs)
              (u/tc-delayed-error (str "Bad arguments to apply: "
                                       "\n\nTarget: \t" (prs/unparse-type ftype) 
                                       "\n\nArguments:\t" (str/join " " (mapv prs/unparse-type (concat arg-tys [tail-ty]))))
                                  :return (error-ret expected))

              ;this case of the function type has a rest argument
              (and rest
                   ;; check that the tail expression is a subtype of the rest argument
                   (sub/subtype? tail-ty (c/Un r/-nil (c/RClass-of Seqable [rest])))
                   (sub/subtypes-varargs? arg-tys dom rest))
              (ret (r/Result-type* rng)
                   (r/Result-filter* rng)
                   (r/Result-object* rng))

              ;other cases go here

              ;next case
              :else (recur (next fs))))))

      ;; apply of a simple polymorphic function
      (r/Poly? ftype)
      (let [vars (c/Poly-fresh-symbols* ftype)
            bbnds (c/Poly-bbnds* vars ftype)
            body (c/Poly-body* vars ftype)
            _ (assert (r/FnIntersection? body))
            arg-tres (mapv check fixed-args)
            arg-tys (mapv (comp ret-t expr-type) arg-tres)
            tail-bound nil
            tail-ty (ret-t (expr-type (check tail)))]
        (loop [[{:keys [dom rng rest drest] :as ftype0} :as fs] (:types body)]
          ;          (when (seq fs)
          ;            (prn "checking fn" (prs/unparse-type (first fs))
          ;                 (mapv prs/unparse-type arg-tys)))
          (cond
            (empty? fs) (u/tc-delayed-error (str "Bad arguments to polymorphic function in apply")
                                            :return (error-ret expected))
            ;the actual work, when we have a * function and a list final argument
            :else 
            (if-let [substitution (cgen/handle-failure
                                    (and rest (not tail-bound) 
                                         (<= (count dom)
                                             (count arg-tys))
                                         (cgen/infer-vararg (zipmap vars bbnds) {}
                                                            (cons tail-ty arg-tys)
                                                            (cons (c/Un r/-nil (c/RClass-of Seqable [rest])) dom)
                                                            rest
                                                            (r/Result-type* rng))))]
              (ret (subst/subst-all substitution (r/Result-type* rng)))
              (recur (next fs))))))

      :else ::not-special)))

;convert apply to normal function application
(add-invoke-apply-method :default 
  [expr & [expected]]
  (let [t (check-apply expr expected)]
    (if (= t ::not-special)
      t
      (assoc expr
             expr-type t))))

(defn normal-invoke [expr fexpr args expected & {:keys [cfexpr cargs]}]
  (u/p :check/normal-invoke
  (let [cfexpr (or cfexpr
                   (check fexpr))
        cargs (or cargs
                  (doall (map check args)))
        ftype (expr-type cfexpr)
        argtys (map expr-type cargs)
        actual (check-funapp fexpr args ftype argtys expected)]
    (assoc expr
           expr-type actual))))

(add-check-method :invoke
  [{:keys [fexpr args env] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  #_(prn "invoke:" ((some-fn :var :keyword :op) fexpr))
  (binding [vs/*current-env* env]
    (let [e (invoke-special expr expected)]
      (if (not= :default e) 
        e
        (let [cfexpr (check fexpr)]
          (cond
            (c/keyword-value? (ret-t (expr-type cfexpr)))
            (let [[target default] args]
              (assert (<= 1 (count args) 2))
              (assoc expr
                     expr-type (invoke-keyword (expr-type (check fexpr))
                                               (expr-type (check target))
                                               (when default
                                                 (expr-type (check default))) 
                                               expected)))

            :else (normal-invoke expr fexpr args expected :cfexpr cfexpr)))))))

;lam-result in TR
(u/defrecord FnResult [args kws rest drest body]
  "Results of checking a fn method"
  [(every? symbol? (map first args))
   (every? Type? (map second args))
   ((some-fn nil? (u/hvector-c? symbol? r/KwArgs?)) kws)
   ((some-fn nil? (u/hvector-c? symbol? Type?)) rest)
   (nil? drest)
   (TCResult? body)])

(defn- KwArgs-minimum-args [^KwArgs kws]
  {:pre [(r/KwArgs? kws)]
   :post [((u/hmap-c? :minimum (complement neg?)
                      :maximum (some-fn nil? (complement neg?)))
           %)]}
  {:minimum (count (.mandatory kws))
   ; if no optional parameters, must supply exactly the number of mandatory arguments
   :maximum (when (empty? (.optional kws))
              (count (.mandatory kws)))})

;[(Seqable Expr) (Option Expr) FnIntersection -> (Seqable Function)]
(defn relevant-Fns
  "Given a set of required-param exprs, rest-param expr, and a FnIntersection,
  returns a seq of Functions containing Function types
  whos arities could be a subtype to the method with the fixed and rest parameters given"
  [required-params rest-param fin]
  {:pre [(r/FnIntersection? fin)]
   :post [(every? r/Function? %)]}
  (assert (not (some :drest (:types fin))))
  (let [nreq (count required-params)]
    ;(prn "nreq" nreq)
    ;(prn "rest-param" rest-param)
    (filter (fn [{:keys [dom rest kws]}]
              (let [ndom (count dom)]
                (if rest-param 
                  (or ; required parameters can flow into the rest type
                      (when rest
                        (<= nreq ndom))
                      ; kw functions must have exact fixed domain match
                      (when kws
                        (= nreq ndom)))
                  (and (not rest) (= nreq ndom)))))
            (:types fin))))

(defonce ^:dynamic *check-fn-method1-checkfn* nil)
; [(U nil Type) (U nil DottedPretype) -> Type]
; takes the current rest or drest argument (only one is non-nil) and returns
; the type to assign the rest parameter
(defonce ^:dynamic *check-fn-method1-rest-type* nil)

(declare check-fn)

(add-check-method :fn-expr
  [{:keys [env] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (:line env))
  (binding [vs/*current-env* env
            vs/*current-expr* expr
            *check-fn-method1-checkfn* check
            *check-fn-method1-rest-type* (fn [rest drest kws]
                                           {:pre [(or (Type? rest)
                                                      (r/DottedPretype? drest)
                                                      (r/KwArgs? kws))
                                                  (#{1} (count (filter identity [rest drest kws])))]
                                            :post [(Type? %)]}
                                           ;(prn "rest" rest)
                                           ;(prn "drest" drest)
                                           ;(prn "kws" kws)
                                           (cond
                                             (or rest drest)
                                             (c/Un r/-nil 
                                                   (r/TApp-maker (r/Name-maker 'clojure.core.typed/NonEmptySeq)
                                                                 [(or rest (.pre-type ^DottedPretype drest))]))
                                             :else (c/KwArgs->Type kws)))]
    (let [type (check-fn expr (let [default-ret (ret (r/make-FnIntersection
                                                       (r/make-Function [] r/-any r/-any)))]
                                (cond (and expected (not= r/-any (ret-t expected))) expected
                                      :else default-ret)))
          _ (when expected
              (when-not (sub/subtype? (ret-t type) (ret-t expected))
               (expected-error (ret-t type) (ret-t expected))))]
      (assoc expr
             expr-type type))))

(declare abstract-object abstract-filter abstract-type abo)

; Difference from Typed Racket
;
; Here we also abstract types with abstract-type. We have types
; like HeterogeneousVector that contains Result's, but can also
; appear in arbitrary positions. The combination of these means
; we need to abstract and instantiate all types at function boundaries.

;[TCResult (Seqable Symbol) -> Result]
(defn abstract-result [result arg-names]
  {:pre [(TCResult? result)
         (every? symbol? arg-names)]
   :post [(r/Result? %)]}
  ;(prn "abstract result" result arg-names)
  (u/p :check/abstract-result
  (let [keys (range (count arg-names))]
    (r/make-Result
      (abstract-type   arg-names keys (ret-t result))
      (abstract-filter arg-names keys (ret-f result))
      (abstract-object arg-names keys (ret-o result))))))

;[Type (Seqable Symbol) -> Type]
(defn abstract-type [ids keys t]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (r/AnyType? t)]
   :post [(r/AnyType? %)]}
  ;(prn "abstract type" ids keys t)
  (letfn [(sb-t [t] (abstract-type ids keys t))
          (sb-f [f] (abo ids keys f))
          (sb-o [o] (abstract-object ids keys o))]
    (fold/fold-rhs ::abo
       {:type-rec sb-t
        :filter-rec sb-f
        :object-rec sb-o}
      t)))

;[(Seqable Symbol) (Seqable AnyInteger) RObject -> RObject]
(defn abstract-object [ids keys o]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (obj/RObject? o)]
   :post [(obj/RObject? %)]}
  ;(prn "abstract-object" ids keys o)
  (letfn [ ; Difference from Typed Racket:
            ;   because abstract-result calls abstract-type, we could have
            ;   already-abstracted filters at this point. We relax the contract
            ;   to allow naturals.
            ;
            ; eg. (ann-form (fn [] (fn [b] b)) [-> [Any -> Any]])
            ;
            ;    In this type the (fn [b] b) is already abstracted as 
            ;      [Any -> Any :filters {:then (! (U nil false) 0), :else (is (U nil false) 0)} :object {:id 0}]
            ;    by the time we call abstract-result.
          (lookup [y]
            {:pre [((some-fn symbol? u/nat?) y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector ids keys)))]
    (cond
      (and (obj/Path? o)
           (lookup (:id o))) (update-in o [:id] lookup)
      :else obj/-empty)))

;[(Seqable Symbol) (Seqable AnyInteger) (U NoFilter FilterSet) 
;  -> (U NoFilter FilterSet)]
(defn abstract-filter [ids keys fs]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         ((some-fn fl/NoFilter? fl/FilterSet?) fs)]
   :post [((some-fn fl/NoFilter? fl/FilterSet?) %)]}
  ;(prn "abstract filter" ids keys fs)
  (cond
    (fl/FilterSet? fs)
    (let [{fs+ :then fs- :else} fs]
      (fo/-FS (abo ids keys fs+)
              (abo ids keys fs-)))
    (fl/NoFilter? fs) (fo/-FS fl/-top fl/-top)))

(derive ::abo fold/fold-rhs-default)

(fold/add-fold-case ::abo
                    TypeFilter
                    (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                      ;if variable goes out of scope, replace filter with fl/-top
                      (if-let [scoped (lookup id)]
                        (fo/-filter type scoped path)
                        fl/-top)))

(fold/add-fold-case ::abo
                    NotTypeFilter
                    (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                      ;if variable goes out of scope, replace filter with fl/-top
                      (if-let [scoped (lookup id)]
                        (fo/-not-filter type scoped path)
                        fl/-top)))

;[(Seqable Symbol) (Seqable AnyInteger) Filter -> Filter]
(defn abo [xs idxs f]
  {:pre [(every? symbol? xs)
         (every? integer? idxs)
         (fl/Filter? f)]
   :post [(fl/Filter? %)]}
  ;(prn "abo" xs idxs f)
  (letfn [(lookup [y]
            ; Difference from Typed Racket:
            ;   because abstract-result calls abstract-type, we could have
            ;   already-abstracted filters at this point. We relax the contract
            ;   to allow naturals.
            ;
            ; eg. (ann-form (fn [] (fn [b] b)) [-> [Any -> Any]])
            ;
            ;    In this type the (fn [b] b) is already abstracted as 
            ;      [Any -> Any :filters {:then (! (U nil false) 0), :else (is (U nil false) 0)} :object {:id 0}]
            ;    by the time we call abstract-result.
            {:pre [((some-fn symbol? u/nat?) y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector xs idxs)))
          (rec [f] (abo xs idxs f))
          (sb-t [t] (abstract-type xs idxs t))]
    (fold/fold-rhs ::abo
      {:type-rec sb-t 
       :filter-rec rec
       :locals {:lookup lookup}}
      f)))

;[FnResult -> Function]
(defn FnResult->Function [{:keys [args kws rest drest body] :as fres}]
  {:pre [(FnResult? fres)]
   :post [(r/Function? %)]}
  (u/p :check/FnResult->Function
  (let [; names of formal parameters to abstract from result type
        rest-param-name (or (first rest)
                            (first drest)
                            (first kws))
        arg-names (concat (map first args)
                          (when rest-param-name
                            [rest-param-name]))]
    (r/Function-maker
      (map second args)
      (abstract-result body arg-names)
      (when rest
        (second rest))
      (when drest
        (second drest))
      (when kws
        (second kws))))))

;TODO eliminate, only used in pfn>, not needed.
#_(defn check-anon-fn
  "Check anonymous function, with annotated methods. methods-types
  is a (Seqable (HMap {:dom (Seqable Type) :rng (U nil Type)}))"
  [{:keys [methods] :as expr} methods-types & {:keys [poly]}]
  {:pre [(every? (u/hmap-c? :dom (u/every-c? Type?)
                            :rng (some-fn nil? Type?)
                            :rest nil? ;TODO
                            :drest nil?) ;TODO
                 methods-types)
         ((some-fn nil? 
                   (u/every-c? (u/hvector-c? symbol? r/Bounds?)))
          poly)]
   :post [(TCResult? (expr-type %))]}
  (cond
    ; named fns must be fully annotated, and are checked with normal check
    (:name expr) (let [ftype (apply r/make-FnIntersection 
                                    (doall (for [{:keys [dom rng]} methods-types]
                                             (if rng
                                               (r/make-Function dom rng)
                                               (throw (Exception. "Named anonymous functions require return type annotation"))))))
                       ftype (if poly
                               (c/Poly* (map first poly)
                                      (map second poly)
                                      ftype)
                               ftype)]

                   (check expr (ret ftype)))
    :else
    (let [;_ (prn methods methods-types expr)
          ftype (apply r/make-FnIntersection (mapv FnResult->Function 
                                                   (mapv (fn [m {:keys [dom rng]}]
                                                           (check-anon-fn-method m dom rng))
                                                         methods methods-types)))]
      (assoc expr
             expr-type (ret ftype (fo/-true-filter) obj/-empty)))))

;[Type -> '[Type (Option (Seqable Symbol)) (Option (Seqable F)) (Option (Seqable Bounds)) (Option (U :Poly :PolyDots))]
; -> Type]
(defn unwrap-poly
  "Return a pair vector of the instantiated body of the possibly polymorphic
  type and the names used"
  [t]
  {:pre [(Type? t)]
   :post [((u/hvector-c? Type? 
                         (some-fn nil? (u/every-c? r/F?))
                         (some-fn nil? (u/every-c? r/Bounds?))
                         (some-fn nil? #{:Poly :PolyDots})) %)]}
  (cond
    (r/Poly? t) (let [new-nmes (c/Poly-fresh-symbols* t)
                      new-frees (map r/make-F new-nmes)]
                  [(c/Poly-body* new-nmes t) new-frees (c/Poly-bbnds* new-nmes t) :Poly])
    (r/PolyDots? t) (let [new-nmes (c/PolyDots-fresh-symbols* t)
                          new-frees (map r/make-F new-nmes)]
                      [(c/PolyDots-body* new-nmes t) new-frees (c/PolyDots-bbnds* new-nmes t) :PolyDots])
    :else [t nil nil nil]))

;[Type (Seqable Symbol) (Seqable F) (U :Poly :Polydots nil) -> Type]
(defn rewrap-poly [body inst-frees bnds poly?]
  {:pre [(Type? body)
         (every? r/F? inst-frees)
         ((some-fn nil? #{:Poly :PolyDots}) poly?)]
   :post [(Type? %)]}
  (case poly?
    :Poly (c/Poly* (map :name inst-frees) bnds body)
    :PolyDots (c/PolyDots* (map :name inst-frees) bnds body)
    body))

(declare check-fn-method check-fn-method1)

; Check a sequence of methods against a (possibly polymorphic) function type.
;
; If this is a deftype method, provide a recur-target-fn to handle recur behaviour
; and validate-expected-fn to prevent expected types that include a rest argument.
;
; (ann check-fn-methods [Expr Type & :optional {:recur-target-fn (Nilable [Function -> RecurTarget])
;                                               :validate-expected-fn (Nilable [FnIntersection -> Any])}])
(defn check-fn-methods [methods expected
                        & {:keys [recur-target-fn
                                  validate-expected-fn
                                  self-name]}]
  {:pre [(r/Type? expected)
         ((some-fn nil? symbol?) self-name)]
   :post [(r/Type? %)]}
  ; FIXME Unions of functions are not supported yet
  (let [;; FIXME This is trying to be too smart, should be a simple cond with Poly/PolyDots cases

        ; try and unwrap type enough to find function types
        exp (c/fully-resolve-type expected)
        ; unwrap polymorphic expected types
        [fin inst-frees bnds poly?] (unwrap-poly exp)
        ; once more to make sure (FIXME is this needed?)
        fin (c/fully-resolve-type fin)
        ;ensure a function type
        _ (when-not (r/FnIntersection? fin)
            (u/int-error
              (str (pr-str (prs/unparse-type fin)) " is not a function type")))
        _ (when validate-expected-fn
            (validate-expected-fn fin))
        ;collect all inferred Functions
        inferred-fni (lex/with-locals (when-let [name self-name] ;self calls
                                        (when-not expected 
                                          (u/int-error (str "Recursive functions require full annotation")))
                                        (assert (symbol? name) name)
                                        {name expected})
                       ;scope type variables from polymorphic type in body
                       (free-ops/with-free-mappings (case poly?
                                                      :Poly (zipmap (map r/F-original-name inst-frees)
                                                                    (map #(hash-map :F %1 :bnds %2) inst-frees bnds))
                                                      :PolyDots (zipmap (map r/F-original-name (next inst-frees))
                                                                        (map #(hash-map :F %1 :bnds %2) (next inst-frees) (next bnds)))
                                                      {})
                         (dvar-env/with-dotted-mappings (case poly?
                                                          :PolyDots {(-> inst-frees last r/F-original-name) (last inst-frees)}
                                                          {})
                           (apply r/make-FnIntersection
                                  (mapcat (fn [method]
                                            (let [fnt (check-fn-method method fin
                                                                       :recur-target-fn recur-target-fn)]
                                              fnt))
                                          methods)))))
        ;rewrap in Poly or PolyDots if needed
        pfni (rewrap-poly inferred-fni inst-frees bnds poly?)]
    pfni))

; Can take a CLJ or CLJS function expression.
;
;[FnExpr (Option Type) -> Expr]
(defn check-fn 
  "Check a fn to be under expected and annotate the inferred type"
  [{:keys [methods] :as fexpr} expected]
  {:pre [(TCResult? expected)]
   :post [(TCResult? %)]}
  (ret (check-fn-methods methods (ret-t expected)
                         :self-name (impl/impl-case
                                      :clojure (hygienic/hname-key fexpr)
                                      :cljs (-> fexpr :name :name)))
       (fo/-FS fl/-top fl/-bot) 
       obj/-empty))

;[MethodExpr FnIntersection & :optional {:recur-target-fn (U nil [Function -> RecurTarget])}
;   -> (Seq Function)]
(defn check-fn-method [method fin & {:keys [recur-target-fn]}]
  {:pre [(r/FnIntersection? fin)]
   :post [(seq %)
          (every? r/Function? %)]}
  (u/p :check/check-fn-method
  (let [required-params (impl/impl-case
                          :clojure (:required-params method)
                          :cljs ((if (:variadic method) butlast identity)
                                 (:params method)))
        rest-param (impl/impl-case
                     :clojure (:rest-param method)
                     :cljs ((if (:variadic method) last (constantly nil))
                            (:params method)))
        mfns (relevant-Fns required-params rest-param fin)]
    #_(prn "relevant-Fns" (map prs/unparse-type mfns))
    (cond
      ;If no matching cases, assign parameters to Any
      (empty? mfns) [(check-fn-method1 method 
                                       (r/make-Function (repeat (count required-params) r/-any) ;doms
                                                        r/-any  ;rng 
                                                        (when rest-param ;rest
                                                          r/-any))
                                       :recur-target-fn recur-target-fn)]
      :else (doall
              (for [f mfns]
                (check-fn-method1 method f
                                  :recur-target-fn recur-target-fn)))))))

(declare ^:dynamic *recur-target*)

(defmacro with-recur-target [tgt & body]
  `(binding [*recur-target* ~tgt]
     ~@body))

(declare env+ ->RecurTarget RecurTarget?)

;check method is under a particular Function, and return inferred Function
;
; check-fn-method1 exposes enough wiring to support the differences in deftype
; methods and normal methods via `fn`.
;
; # Differences in recur behaviour
;
; deftype methods do *not* pass the first parameter (usually `this`) when calling `recur`.
;
; eg. (my-method [this a b c] (recur a b c))
;
; The behaviour of generating a RecurTarget type for recurs is exposed via the :recur-target-fn
;
;
;[MethodExpr Function -> Function]
(defn check-fn-method1 [method {:keys [dom rest drest kws] :as expected}
                        & {:keys [recur-target-fn]}]
  {:pre [(r/Function? expected)]
   :post [(r/Function? %)]}
  #_(prn "checking syntax:" (u/emit-form-fn method))
  (u/p :check/check-fn-method1
  (let [body (impl/impl-case
               :clojure (:body method)
               :cljs (:expr method))
        required-params (impl/impl-case
                          :clojure (:required-params method)
                          :cljs ((if (:variadic method) butlast identity)
                                 (:params method)))
        rest-param (impl/impl-case
                     :clojure (:rest-param method)
                     :cljs ((if (:variadic method) last (constantly nil))
                            (:params method)))

        param-name (impl/impl-case
                     :clojure hygienic/hsym-key
                     :cljs :name)
        param-obj (comp #(obj/->Path nil %)
                        param-name)
        ; Difference from Typed Racket:
        ;
        ; Because types can contain abstracted names, we instantiate
        ; the expected type in the range before using it.
        ;
        ; eg. Checking against this function type:
        ;      [Any Any
        ;       -> (HVec [(U nil Class) (U nil Class)]
        ;                :objects [{:path [Class], :id 0} {:path [Class], :id 1}])]))
        ;     means we need to instantiate the HVec type to the actual argument
        ;     names with open-Result.
        ;
        ;     If the actual function method is (fn [a b] ...) we check against:
        ;
        ;       (HVec [(U nil Class) (U nil Class)]
        ;              :objects [{:path [Class], :id a} {:path [Class], :id b}])
        expected-rng (apply ret
                       (open-Result (:rng expected)
                                    (map param-obj
                                         (concat required-params 
                                                 (when rest-param [rest-param])))))
        ;ensure Function fits method
        _ (when-not ((if (or rest drest kws) <= =) (count required-params) (count dom))
            (u/int-error (str "Checking method with incorrect number of expected parameters"
                              ", expected " (count dom) " required parameter(s) with"
                              (if rest " a " " no ") "rest parameter, found " (count required-params)
                              " required parameter(s) and" (if rest-param " a " " no ")
                              "rest parameter.")))

        _ (when-not (or (not rest-param)
                        (some identity [drest rest kws]))
            (u/int-error (str "No type for rest parameter")))

        ;;unhygienic version
        ;        ; Update filters that reference bindings that the params shadow.
        ;        ; Abstracting references to parameters is handled later in abstract-result, but
        ;        ; suffers from bugs due to un-hygienic macroexpansion (see `abstract-result`).
        ;        ; c/In short, don't shadow parameters if you want meaningful filters.
        ;        props (mapv (fn [oldp]
        ;                      (reduce (fn [p sym]
        ;                                {:pre [(fl/Filter? p)
        ;                                       (symbol? sym)]}
        ;                                (subst-filter p sym obj/-empty true))
        ;                              oldp (map :sym required-params)))
        ;                    (:props lex/*lexical-env*))

        _ (when (impl/checking-clojure?)
            (assert (every? symbol? (map param-name required-params))
                    "Unhygienic AST detected"))
        props (:props lex/*lexical-env*)
        fixed-entry (map vector 
                         (map param-name required-params)
                         (concat dom 
                                 (repeat (or rest (:pre-type drest)))))
        ;_ (prn "checking function:" (prs/unparse-type expected))
        check-fn-method1-rest-type *check-fn-method1-rest-type*
        _ (assert check-fn-method1-rest-type "No check-fn bound for rest type")
        rest-entry (when rest-param
                     [[(param-name rest-param)
                       (check-fn-method1-rest-type rest drest kws)]])
        ;_ (prn "rest entry" rest-entry)
        _ (assert ((u/hash-c? symbol? Type?) (into {} fixed-entry))
                  (into {} fixed-entry))
        _ (assert ((some-fn nil? (u/hash-c? symbol? Type?)) (when rest-entry
                                                              (into {} rest-entry))))

        ; if this fn method is a multimethod dispatch method, then infer
        ; a new filter that results from being dispatched "here"
        mm-filter (when-let [{:keys [dispatch-fn-type dispatch-val-ret]} *current-mm*]
                    (u/p :check/check-fn-method1-inner-mm-filter-calc
                    (assert (and dispatch-fn-type dispatch-val-ret))
                    (assert (not (or drest rest rest-param)))
                    (let [disp-app-ret (check-funapp nil nil 
                                                     (ret dispatch-fn-type)
                                                     (map ret dom (repeat (fo/-FS fl/-top fl/-top)) 
                                                          (map param-obj required-params))
                                                     nil)
                          ;_ (prn "disp-app-ret" disp-app-ret)
                          ;_ (prn "disp-fn-type" (prs/unparse-type dispatch-fn-type))
                          ;_ (prn "dom" dom)
                          isa-ret (tc-isa? disp-app-ret dispatch-val-ret)
                          then-filter (-> isa-ret ret-f :then)
                          _ (assert then-filter)]
                      then-filter)))
        ;_ (prn "^^^ mm-filter")

        ;_ (prn "funapp1: inferred mm-filter" mm-filter)

        env (let [env (-> lex/*lexical-env*
                          ;add mm-filter
                          (assoc-in [:props] (set (concat props (when mm-filter [mm-filter]))))
                          ;add parameters to scope
                          ;IF UNHYGIENIC order important, (fn [a a & a]) prefers rightmost name
                          (update-in [:l] merge (into {} fixed-entry) (into {} rest-entry)))
                  flag (atom false :validator u/boolean?)
                  env (if mm-filter
                        (let [t (env+ env [mm-filter] flag)]
                          t)
                        env)]
              (assert (not @flag) "Unreachable method: Local inferred to be bottom when applying multimethod filter")
              env)

        check-fn-method1-checkfn *check-fn-method1-checkfn*
        _ (assert check-fn-method1-checkfn "No check-fn bound for method1")
        ; rng before adding new filters
        crng-nopass
        (u/p :check/check-fn-method1-chk-rng-pass1
        (binding [*current-mm* nil]
          (var-env/with-lexical-env env
            (let [rec (or ; if there's a custom recur behaviour, use the provided
                          ; keyword argument to generate the RecurTarget.
                          (when recur-target-fn
                            (recur-target-fn expected))
                          ; Otherwise, assume we are checking a regular `fn` method
                          (->RecurTarget dom rest drest nil))
                  _ (assert (RecurTarget? rec))]
              (with-recur-target rec
                (check-fn-method1-checkfn body expected-rng))))))

        ; Apply the filters of computed rng to the environment and express
        ; changes to the lexical env as new filters, and conjoin with existing filters.

        ;_ (prn "crng-nopass" crng-nopass)
        {:keys [then else]} (-> crng-nopass expr-type ret-f)
        then-env (u/p :check/check-fn-method1-env+-rng
                   (env+ env [then] (atom true)))
        new-then-props (reduce (fn [fs [sym t]]
                                 {:pre [((u/set-c? fl/Filter?) fs)]}
                                 (if (= t (get-in env [:l sym]))
                                   ;type hasn't changed, no new propositions
                                   fs
                                   ;new type, add positive proposition
                                   (conj fs (fo/-filter t sym))))
                               #{}
                               (:l then-env))

        crng (u/p :check/check-fn-method1-add-rng-filters
               (update-in crng-nopass [expr-type :fl :then] 
                          (fn [f]
                            (apply fo/-and f new-then-props))))
        _ (binding [vs/*current-expr* body
                    ; don't override the env because :do node don't have line numbers
                    ; The :fn-expr that contains this arity rebinds current-env.
                    #_vs/*current-env* #_(:env body)]
            (when (not (sub/subtype? (-> crng expr-type ret-t) (ret-t expected-rng)))
              (expected-error (-> crng expr-type ret-t) (ret-t expected-rng))))
        rest-param-name (when rest-param
                          (param-name rest-param))]
      (FnResult->Function 
        (->FnResult fixed-entry 
                    (when (and kws rest-param)
                      [rest-param-name kws])
                    (when (and rest rest-param)
                      [rest-param-name rest])
                    (when (and drest rest-param) 
                      [rest-param-name drest])
                    (expr-type crng))))))


(add-check-method :do
  [{:keys [exprs] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (let [nexprs (count exprs)
        [env actual-types]
        (reduce (fn [[env actual-rets] [n expr]]
                  {:pre [(lex/PropEnv? env)
                         (integer? n)
                         (< n nexprs)]
                   :post [(u/hvector-c? lex/PropEnv? (every-pred vector? (u/every-c? r/TCResult?)))]}
                  (let [res (u/p :check/do-inner-check
                            (binding [; always prefer envs with :line information, even if inaccurate
                                        vs/*current-env* (if (:line (:env expr))
                                                           (:env expr)
                                                           vs/*current-env*)
                                        vs/*current-expr* expr]
                                (var-env/with-lexical-env env
                                  (-> (check expr 
                                             ;propagate expected type only to final expression
                                             (when (= (inc n) nexprs)
                                               expected))
                                      expr-type))))
                        flow (-> res r/ret-flow r/flow-normal)
                        flow-atom (atom true)
                        ;_ (prn flow)
                        ;add normal flow filter
                        nenv (env+ env [flow] flow-atom)
                        ;_ (prn nenv)
                        ]
;                        _ (when-not @flow-atom 
;                            (binding [; always prefer envs with :line information, even if inaccurate
;                                                  vs/*current-env* (if (:line (:env expr))
;                                                                     (:env expr)
;                                                                     vs/*current-env*)
;                                      vs/*current-expr* expr]
;                              (u/int-error (str "Applying flow filter resulted in local being bottom"
;                                                "\n"
;                                                (with-out-str (print-env* nenv))
;                                                "\nOld: "
;                                                (with-out-str (print-env* env))))))]
                    (if @flow-atom
                      ;reachable
                      [nenv (conj actual-rets res)]
                      ;unreachable
                      (do ;(prn "Detected unreachable code")
                        (reduced [nenv (conj actual-rets (ret (r/Bottom)))])))))
                [lex/*lexical-env* []] (map-indexed vector exprs))]
    (assoc expr
           expr-type (last actual-types)))) ;should be a ret already

(add-check-method :local-binding-expr
  [{:keys [local-binding] :as expr} & [expected]]
  (binding [; no line/col info in AST
            #_vs/*current-env* #_(:env expr)]
    (let [sym (hygienic/hsym-key local-binding)
          t (var-env/type-of sym)
          _ (when (and expected
                       (not (sub/subtype? t (ret-t expected))))
              (prs/with-unparse-ns (expr-ns expr)
                ;FIXME no line number, not present in AST
                (u/tc-delayed-error 
                  (str "Local binding " sym " expected type " (pr-str (prs/unparse-type (ret-t expected)))
                       ", but actual type " (pr-str (prs/unparse-type t)))
                  :form (u/emit-form-fn expr))))]
      (assoc expr
             expr-type (ret t 
                            (fo/-FS (fo/-not-filter (c/Un r/-nil r/-false) sym)
                                    (fo/-filter (c/Un r/-nil r/-false) sym))
                            (obj/->Path nil sym))))))


;[Method -> Symbol]
(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

(declare Java-symbol->Type)

;[Symbol Boolean -> (Option Type)]
(defn symbol->PArray [sym nilable?]
  {:pre [(symbol? sym)
         (u/boolean? nilable?)]
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

;[Symbol Boolean -> Type]
(defn Java-symbol->Type [sym nilable?]
  {:pre [(symbol? sym)
         (u/boolean? nilable?)]
   :post [(Type? %)]}
  (if-let [typ (or ((prs/clj-primitives-fn) sym)
                   (symbol->PArray sym nilable?)
                   (when-let [cls (resolve sym)]
                     (apply c/Un (c/RClass-of-with-unknown-params cls)
                            (when nilable?
                              [r/-nil]))))]
    typ
    (u/tc-delayed-error (str "Method symbol " sym " does not resolve to a type"))))

;[clojure.reflect.Method -> Type]
(defn- instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(r/FnIntersection? %)]}
  (assert (class? (resolve declaring-class)))
  (r/make-FnIntersection (r/make-Function (concat [(c/RClass-of-with-unknown-params declaring-class)]
                                                  (doall (map #(Java-symbol->Type % false) parameter-types)))
                                          (Java-symbol->Type return-type true))))

;[clojure.reflect.Field - Type]
(defn- Field->Type [{:keys [type flags] :as field}]
  {:pre [(instance? clojure.reflect.Field field)]
   :post [(Type? %)]}
  (cond
    (flags :enum) (Java-symbol->Type type false)
    :else (Java-symbol->Type type true)))

;[clojure.reflect.Method -> Type]
(defn Method->Type [{:keys [parameter-types return-type flags] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(r/FnIntersection? %)]}
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (r/make-FnIntersection (r/make-Function (doall (map (fn [[n tsym]] (Java-symbol->Type 
                                                                       tsym (mtd-param-nil/nilable-param? msym nparams n)))
                                                      (map-indexed vector
                                                                   (if (:varargs flags)
                                                                     (butlast parameter-types)
                                                                     parameter-types))))
                                          (Java-symbol->Type return-type (not (mtd-ret-nil/nonnilable-return? msym nparams)))
                                          (when (:varargs flags)
                                            (Java-symbol->Type (last parameter-types) 
                                                               (mtd-param-nil/nilable-param? msym nparams (dec nparams))))))))

;[clojure.reflect.Constructor -> Type]
(defn- Constructor->Function [{:keys [declaring-class parameter-types] :as ctor}]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(r/FnIntersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (u/tc-delayed-error (str "Constructor for unresolvable class " (:class ctor))))]
    (r/make-FnIntersection (r/make-Function (doall (map #(Java-symbol->Type % false) parameter-types))
                                            (c/RClass-of-with-unknown-params cls)
                                            nil nil
                                            :filter (fo/-true-filter))))) ;always a true value

;[MethodExpr -> (U nil NamespacedSymbol)]
(defn MethodExpr->qualsym [{c :class :keys [op method method-name] :as expr}]
  {:pre [(#{:static-method :instance-method} op)]
   :post [((some-fn nil? symbol?) %)]}
  (cond
    method (Method->symbol method)
    (and c method-name) (symbol (str (u/Class->symbol c))
                                (str method-name))))

(defn Type->Classes [t]
  {:post [(every? (some-fn class? nil?) %)]}
  (let [t (c/fully-resolve-type t)]
    (cond
      (r/RClass? t) [(r/RClass->Class t)]
      (r/DataType? t) [(r/DataType->Class t)]
      (r/Value? t) [(class (.val ^Value t))]
      (r/Union? t) (mapcat Type->Classes (.types ^Union t))
      :else [Object])))

(defn possible-methods [t method-name arg-tys static?]
  {:pre [(Type? t)
         (every? Type? arg-tys)]
   :post [(every? (partial instance? clojure.reflect.Method) %)]}
  (let [cs (remove nil? (Type->Classes t))]
    (apply concat 
           (for [c cs]
             (let [{:keys [members]} (reflect/reflect c)]
               (filter (fn [{:keys [flags parameter-types name] :as m}]
                         (and (instance? clojure.reflect.Method m)
                              (= (contains? flags :static)
                                 (boolean static?))
                              (= (count parameter-types)
                                 (count arg-tys))
                              (= (str name)
                                 (str method-name))
                              (every? identity
                                      (map sub/subtype?
                                           arg-tys
                                           (map Java-symbol->Type parameter-types)))))
                       members))))))

;[MethodExpr Type Any -> Expr]
(defn check-invoke-method [{c :class :keys [args tag method env method-name] :as expr} expected inst?
                           & {:keys [ctarget cargs]}]
  {:pre [((some-fn nil? TCResult?) expected)]
   :post [(-> % expr-type TCResult?)]}
  #_(prn "invoke method: " (when method (Method->symbol method)) inst?)
  (binding [vs/*current-env* env]
    (let [msym (MethodExpr->qualsym expr)
          rfin-type (or (when msym
                          (@mth-override/METHOD-OVERRIDE-ENV msym))
                        (when method
                          (Method->Type method)))
          _ (when-not rfin-type 
              (u/int-error (str "Unresolved " (if inst? "instance" "static") 
                                " method invocation " 
                                (when c
                                  (str (u/Class->symbol c) "/"))
                                method-name 
                                ".\n\nHint: add type hints."
                                "\n\nin: " (u/emit-form-fn expr))))
          _ (when inst?
              (let [ctarget (or ctarget (check (:target expr)))
                    target-class (resolve (:declaring-class method))
                    _ (assert (class? target-class))]
                ;                (prn "check target" (prs/unparse-type (ret-t (expr-type ctarget)))
                ;                     (prs/unparse-type (c/RClass-of (u/Class->symbol (resolve (:declaring-class method))) nil)))
                (when-not (sub/subtype? (ret-t (expr-type ctarget)) (c/RClass-of-with-unknown-params target-class))
                  (u/tc-delayed-error (str "Cannot call instance method " (Method->symbol method)
                                           " on type " (pr-str (prs/unparse-type (ret-t (expr-type ctarget)))))
                                      :form (u/emit-form-fn expr)))))
          cargs (or cargs (doall (map check args)))
          result-type (check-funapp expr args (ret rfin-type) (map expr-type cargs) expected)
          _ (when expected
              (when-not (sub/subtype? (ret-t result-type) (ret-t expected))
                (u/tc-delayed-error (str "Return type of " (if inst? "instance" "static")
                                         " method " (Method->symbol method)
                                         " is " (prs/unparse-type (ret-t result-type))
                                         ", expected " (prs/unparse-type (ret-t expected)) "."
                                         (when (sub/subtype? r/-nil (ret-t result-type))
                                           (str "\n\nHint: Use `non-nil-return` and `nilable-param` to configure "
                                                "where `nil` is allowed in a Java method call. `method-type` "
                                                "prints the current type of a method.")))
                                    :form (u/emit-form-fn expr))))]
      (assoc expr
             expr-type result-type))))

(add-check-method :static-method
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  #_(prn "static-method" (-> expr :method :name))
  (let [spec (static-method-special expr expected)]
    (if (not= :default spec)
      spec
      (check-invoke-method expr expected false))))

(add-check-method :instance-method
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [spec (instance-method-special expr expected)]
    (if (not= :default spec)
      spec
      (check-invoke-method expr expected true))))

(def COMPILE-STUB-PREFIX "compile__stub")

(add-check-method :static-field
  [{:keys [field] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert field "Static field requires type hints")
  (let []
    (assoc expr
           expr-type (ret (Field->Type field)))))

(declare unwrap-datatype)

(add-check-method :instance-field
  [{:keys [target field target-class field-name] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  #_(prn "instance-field:" expr)
  (binding [vs/*current-expr* expr]
    (when-not target-class
      (u/int-error (str "Call to instance field "
                        (symbol field-name)
                        " requires type hints.")))
    (let [; may be prefixed by COMPILE-STUB-PREFIX
          target-class (symbol
                         (str/replace-first (.getName ^Class target-class)
                                            (str COMPILE-STUB-PREFIX ".")
                                            ""))
          fsym (symbol field-name)
          hinted-cls (u/symbol->Class target-class)
          cexpr (check target)
          ; check that the hinted class at least matches the runtime class we expect
          _ (let [expr-ty (c/fully-resolve-type (-> cexpr expr-type ret-t))
                  cls (cond
                         (r/DataType? expr-ty) (u/symbol->Class (:the-class expr-ty))
                         (r/RClass? expr-ty) (u/symbol->Class (:the-class expr-ty)))]
              (when-not (and cls
                             (sub/class-isa? cls hinted-cls))
                (u/tc-delayed-error (str "Instance field " fsym " expected "
                                         (pr-str hinted-cls)
                                         ", actual " (pr-str (prs/unparse-type expr-ty)))
                                    :form (u/emit-form-fn expr))))
          
                            ; datatype fields are special
          result-t (if-let [override (when-let [dtp (dt-env/get-datatype target-class)]
                                       (let [dt (if (r/Poly? dtp)
                                                  ;generate new names
                                                  (unwrap-datatype dtp (repeatedly (:nbound dtp) gensym))
                                                  dtp)
                                             _ (assert ((some-fn r/DataType? r/Record?) dt))]
                                         (-> (c/DataType-fields* dt) (get (symbol (repl/demunge (str fsym)))))))]
                     override
                     ; if not a datatype field, convert as normal
                     (if field
                       (Field->Type field)
                       (u/tc-delayed-error (str "Instance field " fsym " needs type hints")
                                           :form (u/emit-form-fn expr)
                                           :return (r/TCError-maker))))] 
      (assoc expr
             expr-type (ret result-t)))))

;[Symbol -> Type]
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

              :else (u/tc-delayed-error (str "Cannot generate constructor type for: " sym)
                                        :return r/Err)))]
    (resolve-ctor (dt-env/get-datatype sym))))

(add-check-method :instance-of
  [{cls :class :keys [the-expr] :as expr} & [expected]]
  (let [cls-stub (u/Class->symbol cls)
        clssym (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))
        inst-of (c/RClass-of-with-unknown-params clssym)
        cexpr (check the-expr)
        expr-tr (expr-type cexpr)]
    (assoc expr
           expr-type (ret (c/Un r/-true r/-false)
                          (fo/-FS (fo/-filter-at inst-of (ret-o expr-tr))
                                  (fo/-not-filter-at inst-of (ret-o expr-tr)))
                          obj/-empty))))

(defn ctor-Class->symbol 
  "Returns a symbol representing this constructor's Class, removing any compiler stubs."
  [cls]
  (let [cls-stub (u/Class->symbol cls)]
    (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))))

(defmulti new-special (fn [{:keys [class] :as expr} & [expected]] (ctor-Class->symbol class)))

;; Multimethod definition

(derive ::expected-dispatch-type fold/fold-rhs-default)

(fold/add-fold-case ::expected-dispatch-type
                    Function
                    (fn [ty _]
                      (assoc ty :rng (r/make-Result r/-any))))

;return the expected type for the dispatch fn of the given multimethod's expected type
;[Type -> Type]
(defn expected-dispatch-type [mm-type]
  {:pre [(r/AnyType? mm-type)]
   :post [(r/AnyType? %)]}
  (fold/fold-rhs ::expected-dispatch-type
                 {:type-rec expected-dispatch-type}
                 mm-type))

(defmethod new-special 'clojure.lang.MultiFn
  [{[nme-expr dispatch-expr default-expr hierarchy-expr :as args] :args :as expr} & [expected]]
  (assert expected)
  (assert (= 4 (count args)))
  (assert (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
          "Multimethod hierarchy cannot be customised")
  (assert (= (:val default-expr) :default)
          "Non :default default dispatch value NYI")
  (let [mm-name (:val nme-expr)
        _ (assert (string? (:val nme-expr)))
        mm-qual (symbol (str (expr-ns expr)) mm-name)
        ;_ (prn "mm-qual" mm-qual)
        ;_ (prn "expected ret-t" (prs/unparse-type (ret-t expected)))
        ;_ (prn "expected ret-t class" (class (ret-t expected)))
        expected-mm-disp (expected-dispatch-type (ret-t expected))
        cdisp (check dispatch-expr (ret expected-mm-disp))
        _ (mm/add-multimethod-dispatch-type mm-qual (ret-t (expr-type cdisp)))]
    (assoc expr
           expr-type (ret (c/In (c/RClass-of clojure.lang.MultiFn) (ret-t expected))))))

(defmethod new-special :default [expr & [expected]] ::not-special)

; This node does not have line numbers in jvm.tools.analyzer.
; Hopefully a case above it binds *current-env*
(add-check-method :new
  [{cls :class :keys [ctor args] :as expr} & [expected]]
  (when-not (:line vs/*current-env*)
    (prn "Internal Bug! No line information for :new"))
  (let [spec (new-special expr expected)]
    (cond
      (not= ::not-special spec) spec
      :else
      (let [inst-types *inst-ctor-types*
            clssym (ctor-Class->symbol cls)
            ifn (let [ctor-fn (or (@ctor-override/CONSTRUCTOR-OVERRIDE-ENV clssym)
                                  (and (dt-env/get-datatype clssym)
                                       (DataType-ctor-type clssym))
                                  (when ctor
                                    (Constructor->Function ctor)))
                      _ (when-not ctor-fn 
                          (u/int-error (str "Unresolved constructor invocation " 
                                            (when cls
                                              (u/Class->symbol cls))
                                            ".\n\nHint: add type hints."
                                            "\n\nin: " (u/emit-form-fn expr))))
                      ctor-fn (if inst-types
                                (inst/manual-inst ctor-fn inst-types)
                                ctor-fn)]
                  (ret ctor-fn))
            ;_ (prn "Expected constructor" (prs/unparse-type (ret-t ifn)))
            cargs (mapv check args)
            res-type (check-funapp expr args ifn (map expr-type cargs) nil)
            _ (when (and expected (not (sub/subtype? (ret-t res-type) (ret-t expected))))
                (expected-error (ret-t res-type) (ret-t expected)))]
        (assoc expr
               expr-type res-type)))))

(add-check-method :throw
  [{:keys [exception] :as expr} & [expected]]
  (let [cexception (check exception)
        _ (when-not (sub/subtype? (ret-t (expr-type cexception))
                                  (c/RClass-of Throwable))
            (u/tc-delayed-error (str "Cannot throw: "
                                     (prs/unparse-type (ret-t (expr-type cexception))))))]
    (assoc expr
           expr-type (ret (c/Un)
                          (fo/-FS fl/-top fl/-top) 
                          obj/-empty
                          ;never returns normally
                          (r/-flow fl/-bot)))))

(u/defrecord RecurTarget [dom rest drest kws]
  "A target for recur"
  [(every? Type? dom)
   ((some-fn nil? Type?) rest)
   (nil? drest) ;TODO
   (nil? kws)]) ;TODO

(defmacro set-validator-doc! [var val-fn]
  `(set-validator! ~var (fn [a#] (assert (~val-fn a#)
                                         (str "Invalid reference state: " ~var
                                              " with value: "
                                              (pr-str a#)))
                          true)))

(defonce ^:dynamic *recur-target* nil)
(set-validator-doc! #'*recur-target* (some-fn nil? RecurTarget?))

(defn check-recur [args env recur-expr expected check]
  (binding [vs/*current-env* env]
    (let [{:keys [dom rest] :as recur-target} (if-let [r *recur-target*]
                                                r
                                                (u/int-error (str "No recur target")))
          _ (assert (not ((some-fn :drest :kw) recur-target)) "NYI")
          fixed-args (if rest
                       (butlast args)
                       args)
          rest-arg (when rest
                     (last args))
          rest-arg-type (when rest-arg
                          [(impl/impl-case
                             :clojure (c/RClass-of Seqable [rest])
                             :cljs (c/Protocol-of 'cljs.core/ISeqable [rest]))])
          cargs (mapv check args (map ret 
                                      (concat dom 
                                              (when rest-arg-type
                                                [rest-arg-type]))))
          _ (when-not (and (= (count fixed-args) (count dom))
                           (= (boolean rest) (boolean rest-arg)))
              (u/tc-delayed-error 
                (str "Wrong number of arguments to recur:"
                     " Expected: " ((if rest inc identity) 
                                    (count dom))
                     " Given: " ((if rest-arg inc identity)
                                 (count fixed-args)))))]
      (assoc recur-expr
             expr-type (ret (c/Un))))))

;Arguments passed to recur must match recur target exactly. Rest parameter
;equals 1 extra argument, either a Seqable or nil.
(add-check-method :recur
  [{:keys [args env] :as expr} & [expected]]
  (check-recur args env expr expected check))

(defn binding-init-sym [binding-init]
  {:pre [(= :binding-init (:op binding-init))]
   :post [(symbol? %)]}
  (-> binding-init
      :local-binding
      hygienic/hsym-key))

(declare combine-props)

(defn check-let [binding-inits body expr is-loop expected & {:keys [expected-bnds check-let-checkfn]}]
  (assert check-let-checkfn "No checkfn bound for let")
  (u/p :check/check-let
  (cond
    (and is-loop (seq binding-inits) (not expected-bnds) )
    (do
      (u/tc-delayed-error "Loop requires more annotations")
      (assoc expr
             expr-type (ret (c/Un))))
    :else
    (let [env (reduce (fn [env [expr expected-bnd]]
                        {:pre [(lex/PropEnv? env)]
                         :post [(lex/PropEnv? env)]}
                        (let [init (impl/impl-case
                                     :clojure (-> expr :local-binding :init)
                                     :cljs (-> expr :init))
                              sym (impl/impl-case
                                    :clojure (binding-init-sym expr)
                                    :cljs (:name expr))
                              ; check rhs
                              {:keys [t fl flow]} (expr-type
                                                    (binding [vs/*current-expr* init]
                                                      (var-env/with-lexical-env env
                                                        (check-let-checkfn init (when is-loop
                                                                                  (ret expected-bnd))))))
                              _ (when (and expected-bnd
                                           (not (sub/subtype? t expected-bnd)))
                                  (u/tc-delayed-error 
                                    (str "Loop variable " sym " initialised to "
                                         (pr-str (prs/unparse-type t))
                                         ", expected " (pr-str (prs/unparse-type expected-bnd))
                                         "\n\nForm:\n\t" (u/emit-form-fn init))))
                              t (or expected-bnd t)]
                          (cond
                            (fl/FilterSet? fl)
                            (let [{:keys [then else]} fl
                                  p* [(fo/-imp (fo/-not-filter (c/Un r/-nil r/-false) sym) then)
                                      (fo/-imp (fo/-filter (c/Un r/-nil r/-false) sym) else)]
                                  flow-f (r/flow-normal flow)
                                  flow-atom (atom true)
                                  new-env (-> env
                                              ;update binding type
                                              (assoc-in [:l sym] t)
                                              ;update props
                                              (update-in [:props] #(set 
                                                                     (apply concat 
                                                                            (combine-props p* % (atom true)))))
                                              (env+ [(if (= fl/-bot flow-f) fl/-top flow-f)] flow-atom))
                                  _ (when-not @flow-atom 
                                      (binding [vs/*current-expr* init]
                                        (u/int-error
                                          (str "Applying flow filter resulted in local being bottom"
                                               "\n"
                                               (with-out-str (print-env* new-env))
                                               "\nOld: "
                                               (with-out-str (print-env* env))))))]
                              new-env)

                            (fl/NoFilter? fl) (do
                                                (assert (= (r/-flow fl/-top) flow))
                                                (-> env
                                                    ;no propositions to add, just update binding type
                                                    (assoc-in [:l sym] t))))))
                      lex/*lexical-env* (map vector binding-inits (or expected-bnds
                                                                      (repeat nil))))

          cbody (var-env/with-lexical-env env
                  (if is-loop
                    (binding [*recur-target* (->RecurTarget expected-bnds nil nil nil)]
                      (check-let-checkfn body expected))
                    (binding [vs/*current-expr* body]
                      (check-let-checkfn body expected))))
          ;now we return a result to the enclosing scope, so we
          ;erase references to any bindings this scope introduces
          unshadowed-ret
          (reduce (fn [ty sym]
                    {:pre [(TCResult? ty)
                           (symbol? sym)]}
                    (-> ty
                        (update-in [:t] subst-type sym obj/-empty true)
                        (update-in [:fl] subst-filter-set sym obj/-empty true)
                        (update-in [:o] subst-object sym obj/-empty true)
                        (update-in [:flow :normal] subst-filter sym obj/-empty true)))
                  (expr-type cbody)
                  (map (impl/impl-case
                         :clojure (comp hygienic/hsym-key :local-binding)
                         :cljs :name)
                       binding-inits))]
      (assoc expr
             expr-type unshadowed-ret)))))

;unhygienic version
;(defn check-let [binding-inits body expr is-loop expected & {:keys [expected-bnds]}]
;  (assert (or (not is-loop) expected-bnds) (u/error-msg "Loop requires more annotations"))
;  (let [check-let-checkfn *check-let-checkfn*
;        env (reduce (fn [env [{{:keys [sym init]} :local-binding} expected-bnd]]
;                      {:pre [(lex/PropEnv? env)]
;                       :post [(lex/PropEnv? env)]}
;                        (let [;TODO optimisation: this should be false when aliasing like (let [a a] ...)
;                              shadows-local? (-> env :l (find sym))
;                              ; check rhs
;                              {:keys [t fl o]} (let [noshadow-ret 
;                                                     (time
;                                                     (->
;                                                       (expr-type
;                                                         (binding [vs/*current-expr* init]
;                                                           (var-env/with-lexical-env env
;                                                             (check-let-checkfn init (when is-loop
;                                                                                       (ret expected-bnd)))))))
;                                                       )
;                                                     _ (prn "^^ noshadow-ret")
;                                                     
;                                                     ;substitute previous references to sym with an empty object,
;                                                     ;as old binding is shadowed
;                                                     ; Rather expensive, only perform when necessary (if shadowing actually occurs).
;                                                     shadow-ret
;                                                     (time 
;                                                       (if shadows-local?
;                                                         (-> noshadow-ret
;                                                           (update-in [:t] subst-type sym obj/-empty true)
;                                                           (update-in [:fl] subst-filter-set sym obj/-empty true)
;                                                           (update-in [:o] subst-object sym obj/-empty true))
;                                                         noshadow-ret))
;                                                     _ (prn "^^ shadow-ret")]
;                                                 shadow-ret)
;
;                            ; update old env and new result with previous references of sym (which is now shadowed)
;                            ; replaced with an empty object
;                            ;
;                            ; This is rather expensive with large types, so only perform when another local binding
;                            ; is actually shadowed.
;                            
;                            env (time
;                                  (if shadows-local?
;                                  (-> env
;                                    (update-in [:l] #(let [sc (into {} (for [[oldsym ty] %]
;                                                                         [oldsym (subst-type ty sym obj/-empty true)]))]
;                                                       sc))
;                                    (update-in [:props] (fn [props]
;                                                          (mapv #(subst-filter % sym obj/-empty true) props))))
;                                  env))
;                              _ (prn "^^ calc shadow")]
;                        (cond
;                          (fl/FilterSet? fl)
;                          (let [{:keys [then else]} fl
;                                p* [(fo/-imp (fo/-not-filter (c/Un r/-nil r/-false) sym) then)
;                                    (fo/-imp (fo/-filter (c/Un r/-nil r/-false) sym) else)]
;                                new-env (-> env
;                                          ;update binding type
;                                          (assoc-in [:l sym] t)
;                                          ;update props
;                                          (update-in [:props] #(apply concat 
;                                                                      (combine-props p* % (atom true)))))]
;                            new-env)
;
;                          (fl/NoFilter? fl) (-> env
;                                           ;no propositions to add, just update binding type
;                                           (assoc-in [:l sym] t)))))
;                    lex/*lexical-env* (map vector binding-inits (or expected-bnds
;                                                                (repeat nil))))
;
;        cbody (var-env/with-lexical-env env
;                (if is-loop
;                  (binding [*recur-target* (->RecurTarget expected-bnds nil nil nil)]
;                    (check-let-checkfn body expected))
;                  (binding [vs/*current-expr* body]
;                    (check-let-checkfn body expected))))
;
;        ;now we return a result to the enclosing scope, so we
;        ;erase references to any bindings this scope introduces
;        unshadowed-type 
;        (reduce (fn [ty sym]
;                  {:pre [(TCResult? ty)
;                         (symbol? sym)]}
;                  (-> ty
;                    (update-in [:t] subst-type sym obj/-empty true)
;                    (update-in [:fl] subst-filter-set sym obj/-empty true)
;                    (update-in [:o] subst-object sym obj/-empty true)))
;                (expr-type cbody)
;                (map (comp :sym :local-binding) binding-inits))]
;    (assoc expr
;           expr-type unshadowed-type)))

(add-check-method :let
  [{:keys [is-loop binding-inits body] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (if is-loop
    (let [loop-bnd-anns *loop-bnd-anns*]
      (binding [*loop-bnd-anns* nil]
        (check-let binding-inits body expr true expected :expected-bnds loop-bnd-anns
                   :check-let-checkfn check)))
    (check-let binding-inits body expr false expected :check-let-checkfn check)))

(defn check-letfn [bindings body letfn-expr expected check-fn-letfn]
  (let [inits-expected
        ;try and find annotations, and throw a delayed error if not found
        ;(this expression returns nil)
        (impl/impl-case
          :clojure (when (#{:map} (:op (-> body :exprs first)))
                     (into {}
                           (for [[lb-expr type-syn-expr] (partition 2 (-> body :exprs first :keyvals))]
                             (do
                               (assert (#{:local-binding-expr} (:op lb-expr)))
                               [(-> lb-expr :local-binding :sym)
                                (binding [prs/*parse-type-in-ns* (expr-ns letfn-expr)]
                                  (prs/parse-type (u/constant-expr type-syn-expr)))]))))
          :cljs (when (#{:map} (-> body :statements first :op))
                  (into {} 
                        (for [[lb-expr type-syn-expr] 
                              (map vector 
                                   (-> body :statements first :keys)
                                   (-> body :statements first :vals))]
                          [(-> lb-expr :info :name)
                           (binding [prs/*parse-type-in-ns* (expr-ns letfn-expr)]
                             (prs/parse-type (:form type-syn-expr)))]))))]
    (if-not inits-expected
      (u/tc-delayed-error (str "letfn requires annotation, see: "
                               (impl/impl-case :clojure 'clojure :cljs 'cljs) ".core.letfn>")
                          :return (assoc letfn-expr
                                         expr-type (error-ret expected)))

      (let [cbinding-inits
            (lex/with-locals inits-expected
              (impl/impl-case
                :clojure (doall
                           (for [b-init bindings]
                             (let [sym (-> b-init :local-binding :sym)
                                   init (-> b-init :init)]
                               (check-fn-letfn init (ret (inits-expected sym))))))
                :cljs (doall
                        (for [{:keys [name init]} bindings]
                          (let [expected-fn (inits-expected name)]
                            (assert expected-fn (str "No expected type for " name))
                            (check-fn-letfn init (ret expected-fn)))))))

            ;ignore the type annotations at the top of the body
            normal-letfn-body
            (impl/impl-case
              :clojure (-> body
                           (update-in [:exprs] rest))
              :cljs (-> body
                        (update-in [:statements] rest)))

            cbody (lex/with-locals inits-expected
                    (check-fn-letfn normal-letfn-body expected))]
        (assoc letfn-expr
               expr-type (expr-type cbody))))))

; annotations are in the first expression of the body (a :do)
(add-check-method :letfn
  [{:keys [binding-inits body] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (check-letfn binding-inits body expr expected check))

;[(Seqable Filter) Filter -> Filter]
(defn resolve* [atoms prop]
  {:pre [(every? fl/Filter? atoms)
         (fl/Filter? prop)]
   :post [(fl/Filter? %)]}
  (reduce (fn [prop a]
            (cond
              (fl/AndFilter? a)
              (loop [ps (:fs a)
                     result []]
                (if (empty? ps)
                  (apply fo/-and result)
                  (let [p (first ps)]
                    (cond
                      (fo/opposite? a p) fl/-bot
                      (fo/implied-atomic? p a) (recur (next ps) result)
                      :else (recur (next ps) (cons p result))))))
              :else prop))
          prop
          atoms))

;[(Seqable Filter) -> (Seqable Filter)]
(defn flatten-props [ps]
  {:post [(every? fl/Filter? %)]}
  (loop [acc #{}
         ps ps]
    (cond
      (empty? ps) acc
      (fl/AndFilter? (first ps)) (recur acc (concat (-> ps first :fs) (next ps)))
      :else (recur (conj acc (first ps)) (next ps)))))

(def type-equal? =)

;[(Seqable Filter) (Seqable Filter) (Atom Boolean) 
;  -> '[(Seqable (U ImpFilter fl/OrFilter AndFilter))
;       (Seqable (U TypeFilter NotTypeFilter))]]
(defn combine-props [new-props old-props flag]
  {:pre [(every? fl/Filter? (concat new-props old-props))
         (instance? clojure.lang.Atom flag)
         (u/boolean? @flag)]
   :post [(let [[derived-props derived-atoms] %]
            (and (every? (some-fn fl/ImpFilter? fl/OrFilter? fl/AndFilter?) derived-props)
                 (every? (some-fn fl/TypeFilter? fl/NotTypeFilter?) derived-atoms)))]}
  (let [atomic-prop? (some-fn fl/TypeFilter? fl/NotTypeFilter?)
        {new-atoms true new-formulas false} (group-by (comp boolean atomic-prop?) (flatten-props new-props))]
    (loop [derived-props []
           derived-atoms new-atoms
           worklist (concat old-props new-formulas)]
      (if (empty? worklist)
        [derived-props derived-atoms]
        (let [p (first worklist)
              p (resolve* derived-atoms p)]
          (cond
            (fl/AndFilter? p) (recur derived-props derived-atoms (concat (:fs p) (next worklist)))
            (fl/ImpFilter? p) 
            (let [{:keys [a c]} p
                  implied? (some (fn [p] (fo/implied-atomic? a p)) (concat derived-props derived-atoms))]
              #_(prn "combining " (unparse-filter p) " with " (map unparse-filter (concat derived-props
                                                                                          derived-atoms))
                     " and implied:" implied?)
              (if implied?
                (recur derived-props derived-atoms (cons c (rest worklist)))
                (recur (cons p derived-props) derived-atoms (next worklist))))
            (fl/OrFilter? p)
            (let [ps (:fs p)
                  new-or (loop [ps ps
                                result []]
                           (cond
                             (empty? ps) (apply fo/-or result)
                             (some (fn [other-p] (fo/opposite? (first ps) other-p))
                                   (concat derived-props derived-atoms))
                             (recur (next ps) result)
                             (some (fn [other-p] (fo/implied-atomic? (first ps) other-p))
                                   derived-atoms)
                             fl/-top
                             :else (recur (next ps) (cons (first ps) result))))]
              (if (fl/OrFilter? new-or)
                (recur (cons new-or derived-props) derived-atoms (next worklist))
                (recur derived-props derived-atoms (cons new-or (next worklist)))))
            (and (fl/TypeFilter? p)
                 (type-equal? (c/Un) (:type p)))
            (do 
              ;(prn "Variable set to bottom:" (unparse-filter p))
              (reset! flag false)
              [derived-props derived-atoms])
            (fl/TypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (fl/NotTypeFilter? p)
                 (type-equal? r/-any (:type p)))
            (do 
              ;(prn "Variable set to bottom:" (unparse-filter p))
              (reset! flag false)
              [derived-props derived-atoms])
            (fl/NotTypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (fl/TopFilter? p) (recur derived-props derived-atoms (next worklist))
            (fl/BotFilter? p) (do 
                                ;(prn "Bot filter found")
                                (reset! flag false)
                                [derived-props derived-atoms])
            :else (recur (cons p derived-props) derived-atoms (next worklist))))))))

;; also not yet correct
;; produces old without the contents of rem
;[Type Type -> Type]
(defn remove* [old rem]
  (let [old (c/fully-resolve-type old)
        rem (c/fully-resolve-type rem)
        initial (if (sub/subtype? old rem)
                  (c/Un) ;the empty type
                  (cond
                    ;FIXME TR also tests for App? here. ie (or (r/Name? old) (App? old))
                    (r/Name? old) ;; must be different, since they're not subtypes 
                    ;; and n must refer to a distinct struct type
                    old
                    (r/Union? old) (let [l (:types old)]
                                   (apply c/Un (map (fn [e] (remove* e rem)) l)))
                    (r/Mu? old) (remove* (c/unfold old) rem)
                    (r/Poly? old) (let [vs (c/Poly-fresh-symbols* old)
                                        b (c/Poly-body* vs old)]
                                    (c/Poly* vs 
                                             (c/Poly-bbnds* vs old)
                                             (remove* b rem)))
                    :else old))]
    (if (sub/subtype? old initial) old initial)))

(defn KeyPE->Type [k]
  {:pre [(pe/KeyPE? k)]
   :post [(r/Type? %)]}
  (r/-val (:val k)))

; This is where filters are applied to existing types to generate more specific ones
;[Type Filter -> Type]
(defn update [t lo]
  {:pre [((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)]
   :post [(r/Type? %)]}
  (u/p :check/update
  (let [t (c/fully-resolve-type t)]
    (cond
      ; The easy cases: we have a filter without a further path to travel down.
      ; Just update t with the correct polarity.

      (and (fl/TypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)
            _ (assert (Type? u))
            r (c/restrict u t)]
        r)

      (and (fl/NotTypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)]
        (assert (Type? u))
        (remove* t u))

      ; unwrap unions and intersections to update their members

      (r/Union? t) (let [ts (:types t)
                       new-ts (mapv (fn [t] 
                                      (let [n (update t lo)]
                                        n))
                                    ts)]
                   (apply c/Un new-ts))
      (r/Intersection? t) (let [ts (:types t)]
                          (apply c/In (doall (map (fn [t] (update t lo)) ts))))

      ;from here, t is fully resolved and is not a Union or Intersection

      ;heterogeneous map ops
      ; Positive and negative information down a keyword path
      ; eg. (number? (-> hmap :a :b))
      (and ((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)
           (pe/KeyPE? (first (:path lo)))
           (r/HeterogeneousMap? t))
      (let [polarity (fl/TypeFilter? lo)
            {update-to-type :type :keys [path id]} lo
            [fkeype & rstpth] path
            fpth (KeyPE->Type fkeype)
            ; use this filter to update the right hand side value
            next-filter ((if polarity fo/-filter fo/-not-filter) 
                         update-to-type id rstpth)
            present? (c/hmap-present-key? t fpth)
            absent? (c/hmap-absent-key? t fpth)]
        ;updating a KeyPE should consider 3 cases:
        ; 1. the key is declared present
        ; 2. the key is declared absent
        ; 3. the key is not declared present, and is not declared absent
        (cond
          present?
          ; -hmap simplifies to bottom if an entry is bottom
          (c/-hmap (update-in (:types t) [fpth] update next-filter)
                   (:absent-keys t)
                   (:other-keys? t))
          absent?
          t

          ; key not declared present or absent
          :else
          (c/Un
            (c/-hmap (assoc-in (:types t) [fpth] (update r/-any next-filter))
                     (:absent-keys t)
                     (:other-keys? t))
            ; if we can prove we only ever update this path to nil,
            ; we can ignore the absent case.
            (let [updated-nil (update r/-nil next-filter)]
              (if-not (r/Bottom? updated-nil)
                (c/-hmap (:types t)
                         (conj (:absent-keys t) fpth)
                         (:other-keys? t))
                r/-nothing)))))

      ; nil returns nil on keyword lookups
      (and (fl/NotTypeFilter? lo)
           (pe/KeyPE? (first (:path lo)))
           (r/Nil? t))
      (update r/-nil (update-in lo [:path] rest))

      ; update count information based on a call to `count`
      ; eg. (= 1 (count a))
      (and (fl/TypeFilter? lo)
           (pe/CountPE? (first (:path lo))))
      (let [u (:type lo)]
        (if-let [cnt (when (and (r/Value? u) (integer? (:val u)))
                       (r/make-ExactCountRange (:val u)))]
          (c/restrict cnt t)
          (do (tc-warning "Cannot infer Count from type " (prs/unparse-type u))
              t)))

      ;can't do much without a NotCountRange type or difference type
      (and (fl/NotTypeFilter? lo)
           (pe/CountPE? (first (:path lo))))
      t

      ; Update class information based on a call to `class`
      ; eg. (= java.lang.Integer (class a))
      (and (fl/TypeFilter? lo)
           (pe/ClassPE? (-> lo :path first)))
      (let [_ (assert (empty? (rest (:path lo))))
            u (:type lo)]
        (cond 
          ;restrict the obvious case where the path is the same as a Class Value
          ; eg. #(= (class %) Number)
          (and (r/Value? u)
               (class? (:val u)))
          (c/restrict (c/RClass-of-with-unknown-params (:val u)) t)

          ; handle (class nil) => nil
          (r/Nil? u)
          (c/restrict r/-nil t)

          :else
          (do (tc-warning "Cannot infer type via ClassPE from type " (prs/unparse-type u))
              t)))

      ; Does not tell us anything.
      ; eg. (= Number (class x)) ;=> false
      ;     does not reveal whether x is a subtype of Number, eg. (= Integer (class %))
      (and (fl/NotTypeFilter? lo)
           (pe/ClassPE? (-> lo :path first)))
      t

      ; keyword invoke of non-hmaps
      ; (let [a (ann-form {} (Map Any Any))]
      ;   (number? (-> a :a :b)))
      ; 
      ; I don't think there's anything interesting worth encoding:
      ; use HMap for accurate updating.
      (and (or (fl/TypeFilter? lo)
               (fl/NotTypeFilter? lo))
           (pe/KeyPE? (first (:path lo))))
      t

      ; calls to `keys` and `vals`
      (and ((some-fn fl/TypeFilter? fl/NotTypeFilter?) lo)
           ((some-fn pe/KeysPE? pe/ValsPE?) (first (:path lo))))
      (let [[fstpth & rstpth] (:path lo)
            u (:type lo)
            ;_ (prn "u" (prs/unparse-type u))

            ; solve for x:  t <: (Seqable x)
            x (gensym)
            subst (free-ops/with-bounded-frees {(r/make-F x) r/no-bounds}
                    (u/handle-cs-gen-failure
                      (cgen/infer {x r/no-bounds} {} 
                                  [u]
                                  [(c/RClass-of clojure.lang.Seqable [(r/make-F x)])]
                                  r/-any)))
            ;_ (prn "subst for Keys/Vals" subst)
            _ (when-not subst
                (u/int-error (str "Cannot update " (if (pe/KeysPE? fstpth) "keys" "vals") " of an "
                                  "IPersistentMap with type: " (pr-str (prs/unparse-type u)))))
            element-t-subst (get subst x)
            _ (assert (crep/t-subst? element-t-subst))
            ; the updated 'keys/vals' type
            element-t (:type element-t-subst)
            ;_ (prn "element-t" (prs/unparse-type element-t))
            _ (assert element-t)]
        (assert (empty? rstpth) (str "Further path NYI keys/vals"))
        (if (fl/TypeFilter? lo)
          (c/restrict (if (pe/KeysPE? fstpth)
                        (c/RClass-of IPersistentMap [element-t r/-any])
                        (c/RClass-of IPersistentMap [r/-any element-t]))
                      t)
          ; can we do anything for a NotTypeFilter?
          t))


      :else (u/int-error (str "update along ill-typed path " (pr-str (prs/unparse-type t)) " " (with-out-str (pr lo))))))))

; f can be a composite filter. bnd-env is a the :l of a PropEnv
; ie. a map of symbols to types
;[(IPersistentMap Symbol Type) Filter -> PropEnv]
(defn update-composite [bnd-env f]
  {:pre [(lex/lex-env? bnd-env)
         (fl/Filter? f)]
   :post [(lex/lex-env? %)]}
  #_(prn "update-composite" #_bnd-env #_f)
  (cond
    ; At this point, the OrFilter will be simplified. To update
    ; the types we need to make explicit the fact
    ; (| (! ... a) (! ... b))  is shorthand for
    ;
    ; (| (& (! ... a) (is ... b))
    ;    (& (is ... a) (! ... b))
    ;    (& (! ... a) (! ... b)))
    ;
    ;  then use the verbose representation to update the types.
;    ((some-fn fl/AndFilter? fl/OrFilter?) f)
;    (let [; normalise filters to a set of AndFilters, which are disjuncts
;          disjuncts (if (fl/AndFilter? f)
;                      #{f}
;                      (.fs ^OrFilter f))
;          _ (assert (not-any? fl/OrFilter? disjuncts)
;                    disjuncts)
;          ; each disjunct expands can be expanded to more filters
;          ; this is a list of the new, expanded disjuncts
;          expanded-disjucts (mapcat
;                              (fn [inner-f]
;                                (assert (not (fl/OrFilter? inner-f)) inner-f)
;                                (let [conjuncts (if (fl/AndFilter? inner-f)
;                                                  (.fs ^AndFilter inner-f)
;                                                  #{inner-f})]
;                                  (assert (every? fo/atomic-filter? conjuncts)
;                                          (pr-str inner-f))
;                                  (map (fn [positive-filters]
;                                         (let [negative-filters (map fo/negate (set/difference conjuncts positive-filters))
;                                               combination-filter (apply fo/-and (concat positive-filters negative-filters))]
;                                           combination-filter))
;                                       (map set (remove empty? (comb/subsets conjuncts))))))
;                              disjuncts)
;          update-and (fn [init-env ^AndFilter and-f]
;                       {:pre [(fl/AndFilter? and-f)]}
;                       (reduce (fn [env a]
;                                 {:pre [(fo/atomic-filter? a)]}
;                                 ;eagerly merge
;                                 (merge-with c/In env (update-composite env a)))
;                               init-env (.fs ^AndFilter f)))]
;      ;update env with each disjunct. If variables change, capture both old and new types with Un.
;      ; first time around is special. At least 1 of disjuncts must be applied to the
;      ; environment, so we throw away the initial environment instead of merging it.
;      (let [first-time? (atom true)]
;        (reduce (fn [env fl]
;                  (let [updated-env (cond
;                                      (fl/AndFilter? fl) (update-and env fl)
;                                      (fo/atomic-filter? fl) (update-composite env fl)
;                                      :else (throw (Exception. "shouldn't get here")))]
;                    (if @first-time?
;                      (do (reset! first-time? false)
;                          updated-env)
;                      (merge-with c/Un env updated-env))))
;                bnd-env
;                expanded-disjucts)))

;    (fl/AndFilter? f)
;    (reduce (fn [env a]
;              #_(prn "And filter")
;              ;eagerly merge
;              (merge-with c/In env (update-composite env a)))
;            bnd-env (.fs ^AndFilter f))
;
;    (fl/OrFilter? f)
;    (let [fs (.fs ^OrFilter f)]
;      (reduce (fn [env positive-filters]
;                #_(prn "inside orfilter")
;                (let [negative-filters (map fo/negate (set/difference fs positive-filters))
;                      combination-filter (apply fo/-and (concat positive-filters negative-filters))]
;                  (merge-with c/Un env (update-composite env combination-filter))))
;              bnd-env 
;              (map set (remove empty? (comb/subsets fs)))))
;
    (fl/BotFilter? f)
    (do ;(prn "update-composite: found bottom, unreachable")
      (zipmap (keys bnd-env) (repeat (c/Un))))

    (or (fl/TypeFilter? f)
        (fl/NotTypeFilter? f))
    (let [x (:id f)]
      (if-not (bnd-env x)
        bnd-env
        (update-in bnd-env [x] (fn [t]
                                 ;check if var is ever a target of a set!
                                 ; or not currently in scope
                                 (if (or (nil? t)
                                         (r/is-var-mutated? x))
                                   ; if it is, we do nothing
                                   t
                                   ;otherwise, refine the type
                                   (let [t (or t r/-any)
                                         new-t (update t f)]
                                     new-t))))))
    :else bnd-env))

;; sets the flag box to #f if anything becomes (U)
;[PropEnv (Seqable Filter) (Atom Boolean) -> PropEnv]
(defn env+ [env fs flag]
  {:pre [(lex/PropEnv? env)
         (every? fl/Filter? fs)
         (u/boolean? @flag)]
   :post [(lex/PropEnv? %)
          (u/boolean? @flag)]}
  #_(prn 'env+ fs)
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              {:pre [(lex/PropEnv? env)
                     (fl/Filter? f)]}
              (let [new-env (update-in env [:l] update-composite f)]
                ; update flag if a variable is now bottom
                (when-let [bs (seq (filter (comp #{(c/Un)} val) (:l new-env)))]
                  ;(prn "variables are now bottom: " (map key bs))
                  (reset! flag false))
                new-env))
            (assoc env :props (set (concat atoms props)))
            (concat atoms props))))
;  (letfn [(update-env [env f]
;            {:pre [(lex/PropEnv? env)
;                   (fl/Filter? f)]}
;            (let [new-env (update-in env [:l] update-composite f)]
;              ; update flag if a variable is now bottom
;              (when-let [bs (seq (filter (comp #{(c/Un)} val) (:l new-env)))]
;                ;(prn "variables are now bottom: " (map key bs))
;                (reset! flag false))
;              new-env))]
;    (let [[props atoms] (combine-props fs (:props env) flag)
;          all-filters (apply fo/-and (concat props atoms))]
;      (-> env
;          (update-env all-filters)
;          (assoc :props (set (concat atoms props)))))))

(def object-equal? =)

(defonce ^:dynamic *check-if-checkfn* nil)

;[TCResult Expr Expr (Option Type) -> TCResult]
(defn check-if [tst thn els & [expected]]
  {:pre [(TCResult? tst)
         ((some-fn TCResult? nil?) expected)]
   :post [(TCResult? %)]}
  (let [check-if-checkfn (if-let [c *check-if-checkfn*]
                           c
                           (assert nil "No checkfn bound for if"))]
    (letfn [(tc [expr reachable?]
              {:post [(TCResult? %)]}
              (when-not reachable?
                #_(prn "Unreachable code found.. " expr))
              (cond
                ;; if reachable? is #f, then we don't want to verify that this branch has the appropriate type
                ;; in particular, it might be (void)
                (and expected reachable?)
                (-> (check-if-checkfn expr (-> expected
                                                 (assoc :fl (fo/-FS fl/-top fl/-top))
                                                 (assoc :o obj/-empty)
                                                 (assoc :flow (r/-flow fl/-top))))
                    expr-type)
                ;; this code is reachable, but we have no expected type
                reachable? (-> (check-if-checkfn expr) expr-type)
                ;; otherwise, this code is unreachable
                ;; and the resulting type should be the empty type
                :else (do #_(prn (u/error-msg "Not checking unreachable code"))
                          (ret (c/Un)))))]
      (let [{fs+ :then fs- :else :as f1} (ret-f tst)
            ;          _ (prn "check-if: fs+" (prs/unparse-filter fs+))
            ;          _ (prn "check-if: fs-" (prs/unparse-filter fs-))
            flag+ (atom true :validator u/boolean?)
            flag- (atom true :validator u/boolean?)

            ;_ (print-env)
            ;idsym (gensym)
            env-thn (env+ lex/*lexical-env* [fs+] flag+)
            ;          _ (do (pr "check-if: env-thn")
            ;              (print-env* env-thn))
            env-els (env+ lex/*lexical-env* [fs-] flag-)
            ;          _ (do (pr "check-if: env-els")
            ;              (print-env* env-els))
            ;          new-thn-props (set
            ;                          (filter atomic-filter?
            ;                                  (set/difference
            ;                                    (set (:props lex/*lexical-env*))
            ;                                    (set (:props env-thn)))))
            ;_ (prn idsym"env+: new-thn-props" (map unparse-filter new-thn-props))
            ;          new-els-props (set
            ;                          (filter atomic-filter?
            ;                                  (set/difference
            ;                                    (set (:props lex/*lexical-env*))
            ;                                    (set (:props env-els)))))
            ;_ (prn idsym"env+: new-els-props" (map unparse-filter new-els-props))
            {ts :t fs2 :fl os2 :o flow2 :flow :as then-ret} 
            (binding [vs/*current-expr* thn]
              (var-env/with-lexical-env env-thn
                (tc thn @flag+)))

            {us :t fs3 :fl os3 :o flow3 :flow :as else-ret} 
            (binding [vs/*current-expr* els]
              (var-env/with-lexical-env env-els
                (tc els @flag-)))

            ;_ (prn "flow2" (prs/unparse-flow-set flow2))
            ;_ (prn "flow3" (prs/unparse-flow-set flow3))
            ]

        ;some optimization code here, contraditions etc? omitted

        ;      (prn "check-if: then branch:" (prs/unparse-TCResult then-ret))
        ;      (prn "check-if: else branch:" (prs/unparse-TCResult else-ret))
        (cond
          ;both branches reachable
          (and (not (type-equal? (c/Un) ts))
               (not (type-equal? (c/Un) us)))
          (let [;_ (prn "both branches reachable")
                r (let [filter (cond
                                 (or (fl/NoFilter? fs2)
                                     (fl/NoFilter? fs3)) (fo/-FS fl/-top fl/-top)
                                 (and (fl/FilterSet? fs2)
                                      (fl/FilterSet? fs3))
                                 (let [{f2+ :then f2- :else} fs2
                                       {f3+ :then f3- :else} fs3
                                       ; +ve test, +ve then
                                       new-thn-props (:props env-thn)
                                       ;_ (prn "new-thn-props" (map prs/unparse-filter new-thn-props))
                                       new-els-props (:props env-els)
                                       ;_ (prn "new-els-props" (map prs/unparse-filter new-els-props))
                                       +t+t (apply fo/-and fs+ f2+ new-thn-props)
                                       ;_ (prn "+t+t" (prs/unparse-filter +t+t))
                                       ; -ve test, +ve else
                                       -t+e (apply fo/-and fs- f3+ new-els-props)
                                       ;_ (prn "-t+e" (prs/unparse-filter -t+e))
                                       ; +ve test, -ve then
                                       +t-t (apply fo/-and fs+ f2- new-thn-props)
                                       ;_ (prn "+t-t" (prs/unparse-filter +t-t))
                                       ; -ve test, -ve else
                                       -t-e (apply fo/-and fs- f3- new-els-props)
                                       ;_ (prn "-t-e" (prs/unparse-filter -t-e))

                                       final-thn-prop (fo/-or +t+t -t+e)
                                       ;_ (prn "final-thn-prop" (prs/unparse-filter final-thn-prop))
                                       final-els-prop (fo/-or +t-t -t-e)
                                       ;_ (prn "final-els-prop" (prs/unparse-filter final-els-prop))
                                       fs (fo/-FS final-thn-prop final-els-prop)]
                                   fs)
                                 :else (u/int-error (str "What are these?" fs2 fs3)))
                        type (c/Un ts us)
                        object (if (object-equal? os2 os3) os2 (obj/->EmptyObject))

                        ;only bother with something interesting if a branch is unreachable (the next two cond cases)
                        ;Should be enough for `assert`
                        ;flow (r/-flow (fo/-or flow2 flow3))
                        flow (r/-flow fl/-top)
                        ]
                    (ret type filter object flow))]
            ;(prn "check if:" "both branches reachable, with combined result" (prs/unparse-TCResult r))
            (if expected (check-below r expected) r))
          ;; both branches unreachable, flow-set is ff
          (and (= us (c/Un))
               (= ts (c/Un)))
          (ret (c/Un) (fo/-FS fl/-top fl/-top) obj/-empty (r/-flow fl/-bot))
          ;; special case if one of the branches is unreachable
          (type-equal? us (c/Un))
          (if expected (check-below (ret ts fs2 os2 flow2) expected) (ret ts fs2 os2 flow2))
          (type-equal? ts (c/Un))
          (if expected (check-below (ret us fs3 os3 flow3) expected) (ret us fs3 os3 flow3))
          :else (u/int-error "Something happened"))))))

(add-check-method :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [ctest (binding [vs/*current-expr* test]
                (check test))]
    (assoc expr
           expr-type (binding [*check-if-checkfn* check]
                       (check-if (expr-type ctest) then else)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethods

;[Expr (Option TCResult) -> Expr]
(defn check-normal-def [{:keys [var init init-provided env] :as expr} & [expected]]
  (assert init-provided)
  (let [vsym (u/var->symbol var)
        warn-if-unannotated? (ns-opts/warn-on-unannotated-vars? (expr-ns expr))
        t (var-env/lookup-Var-nofail vsym)
        check? (var-env/check-var? vsym)]
    (cond
      (and check? t)
      (let [cinit (when init-provided
                    (check init (ret t)))
            _ (when cinit
                (when-not (sub/subtype? (ret-t (expr-type cinit)) t)
                  (expected-error (ret-t (expr-type cinit)) t))
                ; now consider this var as checked
                (var-env/add-checked-var-def vsym))]
        (assoc expr
               expr-type (ret (c/RClass-of Var [t t]))))
      (or (not check?) 
          (and warn-if-unannotated?
               (not t)))
      (do (println (when-let [line (-> expr :env :line)] 
                     (str line ": ")) 
                   "Not checking" vsym "definition")
          (flush)
          (assoc expr
                 expr-type (ret (c/RClass-of Var [(or t r/-nothing) (or t r/-any)]))))
      :else (u/tc-delayed-error (str "Found untyped var definition: " vsym
                                     "\nHint: Add the annotation for " vsym
                                     " via check-ns or cf")
                                :return (assoc expr
                                               expr-type (ret (r/TCError-maker)))))))

;TODO print a hint that `ann` forms must be wrapping in `cf` at the REPL
(add-check-method :def
  [{:keys [var init init-provided env] :as expr} & [expected]]
  (assert (:line env))
  ;(prn "Checking def" var)
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (cond 
      ;ignore macro definitions and declare
      (or (.isMacro ^Var var)
          (not init-provided))
      (let [actual-t (c/RClass-of Var [(r/Bottom) r/-any])
            _ (when (and expected
                         (not (sub/subtype? actual-t (ret-t expected))))
                (expected-error actual-t (ret-t expected)))]
        (assoc expr
               expr-type (ret actual-t)))

      :else (check-normal-def expr expected))))

(defn unwrap-tfn
  "Get the instantiated type wrapped by this TFn"
  ([dt nms]
   {:pre [(r/Type? dt)
          (every? symbol? nms)]
    :post [(r/Type? %)]}
   (if (r/TypeFn? dt)
     (c/TypeFn-body* nms dt)
     dt))
  ([dt] (let [nms (when (r/TypeFn? dt)
                    (c/TypeFn-fresh-symbols* dt))]
          (unwrap-tfn dt nms))))

;FIXME I think this hurts more than it helps
;[Type (Seqable Symbol) -> Type]
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

; don't check these implicit methods in a record
(def record-implicits
  '#{entrySet values keySet clear putAll remove put get containsValue isEmpty size without
     assoc iterator seq entryAt containsKey equiv cons empty count getLookupThunk valAt
     withMeta meta equals hashCode hasheq})

(declare check-new-instance-method)

(defn protocol-implementation-type [datatype {:keys [declaring-class] :as method-sig}]
  (let [pvar (c/Protocol-interface->on-var declaring-class)
        ptype (pcl-env/get-protocol pvar)
        demunged-msym (symbol (repl/demunge (str (:name method-sig))))
        ans (doall (map c/fully-resolve-type (sub/datatype-ancestors datatype)))
        ;_ (prn "datatype" datatype)
        ;_ (prn "ancestors" (pr-str ans))
        ]
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
                                  (u/int-error (str "Must provide instantiated ancestor for datatype "
                                                    (:the-class datatype) " to check protocol implementation: "
                                                    pvar)))
                              _ (when (< 1 (count relevant-ancestors))
                                  (u/int-error (str "Ambiguous ancestors for datatype when checking protocol implementation: "
                                                    (pr-str (vec relevant-ancestors)))))]
                          an))
            _ (assert (r/Protocol? pancestor) (pr-str pancestor))
            ;_ (prn "pancestor" pancestor)
            pargs (seq (:poly? pancestor))
            unwrapped-p (if (r/Protocol? ptype)
                          ptype
                          (c/instantiate-typefn ptype pargs))
            _ (assert (r/Protocol? unwrapped-p))
            mth (get (:methods unwrapped-p) demunged-msym)
            _ (when-not mth
                (u/int-error (str "No matching annotation for protocol method implementation: "
                                  demunged-msym)))]
        (extend-method-expected datatype mth)))))

(defn datatype-method-expected [datatype method-sig]
  {:post [(r/Type? %)]}
  (or (protocol-implementation-type datatype method-sig)
      (extend-method-expected datatype (instance-method->Function method-sig))))

(add-check-method :deftype*
  [{nme :name :keys [methods compiled-class env] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  ;TODO check fields match, handle extra fields in records
  #_(prn "Checking deftype definition:" nme)
  (binding [vs/*current-env* env]
    (let [cmmap (into {} (for [[k v] (:mmap expr)]
                           [[(symbol (first k)) (count (second k))]
                            (@#'clojure.reflect/method->map v)]))
          _ (assert ((u/hash-c? (u/hvector-c? (every-pred symbol? (complement namespace))
                                              u/nat?)
                                #(instance? clojure.reflect.Method %))
                     cmmap))
          field-syms (:hinted-fields expr)
          _ (assert (every? symbol? field-syms))
          ; unannotated datatypes are handled below
          dtp (dt-env/get-datatype nme)
          [nms bbnds dt] (if (r/TypeFn? dtp)
                           (let [nms (c/TypeFn-fresh-symbols* dtp)
                                 bbnds (c/TypeFn-bbnds* nms dtp)
                                 body (c/TypeFn-body* nms dtp)]
                             [nms bbnds body])
                           [nil nil dtp])
          expected-fields (when dt
                            (c/DataType-fields* dt))
          expected-field-syms (vec (keys expected-fields))
          ret-expr (assoc expr
                          expr-type (ret (c/RClass-of Class)))]

      (cond
        (not dtp)
        (u/tc-delayed-error (str "deftype " nme " must have corresponding annotation. "
                                 "See ann-datatype and ann-record")
                            :return ret-expr)

        (not ((some-fn r/DataType? r/Record?) dt))
        (u/tc-delayed-error (str "deftype " nme " cannot be checked against: " (prs/unparse-type dt))
                            :return ret-expr)

        (if (r/Record? dt)
          (c/isa-DataType? compiled-class)
          (c/isa-Record? compiled-class))
        (let [datatype? (c/isa-DataType? compiled-class)]
          #_(prn (c/isa-DataType? compiled-class)
               (c/isa-Record? compiled-class)
               (r/DataType? dt)
               (r/Record? dt))
          (u/tc-delayed-error (str (if datatype? "Datatype " "Record ") nme 
                                   " is annotated as a " (if datatype? "record" "datatype") 
                                   ", should be a " (if datatype? "datatype" "record") ". "
                                   "See ann-datatype and ann-record")
                              :return ret-expr))

        (not= expected-field-syms field-syms)
        (u/tc-delayed-error (str "deftype " nme " fields do not match annotation. "
                                 " Expected: " (vec expected-field-syms)
                                 ", Actual: " (vec field-syms))
                            :return ret-expr)

        :else
        (let [check-method? (fn [inst-method]
                              (not (and (r/Record? dt)
                                        (record-implicits (symbol (:name inst-method))))))
              _ (doseq [{:keys [env] :as inst-method} methods
                        :when (check-method? inst-method)]
                  (when t/*trace-checker*
                    (println "Checking deftype* method: " (:name inst-method))
                    (flush))
                  (binding [vs/*current-env* env]
                    (let [method-nme (:name inst-method)
                          _ (assert (symbol? method-nme))
                          ; minus the target arg
                          method-sig (cmmap [method-nme (dec (count (:required-params inst-method)))])]
                      (if-not (instance? clojure.reflect.Method method-sig)
                        (u/tc-delayed-error (str "Internal error checking deftype " nme " method: " method-nme
                                                 ". Available methods: " (pr-str (map (comp first first) cmmap))))
                        (let [expected-ifn (datatype-method-expected dt method-sig)]
                          ;(prn "method expected type" (prs/unparse-type expected-ifn))
                          ;(prn "names" nms)
                          (lex/with-locals expected-fields
                            (free-ops/with-free-mappings 
                              (zipmap (map (comp r/F-original-name r/make-F) nms) 
                                      (map (fn [nm bnd] {:F (r/make-F nm) :bnds bnd}) nms bbnds))
                              ;(prn "lexical env when checking method" method-nme lex/*lexical-env*)
                              ;(prn "frees when checking method" 
                              ;     (into {} (for [[k {:keys [name]}] clojure.core.typed.tvar-env/*current-tvars*]
                              ;                [k name])))
                              ;(prn "bnds when checking method" 
                              ;     clojure.core.typed.tvar-bnds/*current-tvar-bnds*)
                              ;(prn "expected-ifn" expected-ifn)
                              (check-fn-methods
                                [inst-method]
                                expected-ifn
                                :recur-target-fn
                                  (fn [{:keys [dom] :as f}]
                                    {:pre [(r/Function? f)]
                                     :post [(RecurTarget? %)]}
                                    (->RecurTarget (rest dom) nil nil nil))
                                :validate-expected-fn
                                  (fn [fin]
                                    {:pre [(r/FnIntersection? fin)]}
                                    (when (some #{:rest :drest :kws} (:types fin))
                                      (u/int-error
                                        (str "Cannot provide rest arguments to deftype method: "
                                             (prs/unparse-type fin)))))))))))))]
          ret-expr)))))

;[Expr FnIntersection -> Expr]
(defn check-new-instance-method
  [{:keys [body required-params] :as expr} expected-fin]
  {:pre [(r/FnIntersection? expected-fin)]}
  (if (not= 1 (count (:types expected-fin)))
    (u/tc-delayed-error (str "Checking of deftype methods with more than one arity not yet implemented")
                        :return (assoc expr expr-type (ret (r/TCError-maker))))
    (let [{:keys [dom rng] :as expected-fn} (-> expected-fin :types first)]
      (if (:rest expected-fn)
        (u/tc-delayed-error (str "deftype methods do not support rest arguments")
                            :return (assoc expr expr-type (ret (r/TCError-maker))))
        (let [cbody (lex/with-locals (zipmap (map hygienic/hsym-key required-params) dom)
                      ; recur target omits the first argument, 'this'
                      (with-recur-target (->RecurTarget (rest dom) nil nil nil)
                        (check body (r/Result->TCResult rng))))
              _ (when-not (sub/subtype? (-> cbody expr-type ret-t) (r/Result-type* rng))
                  (expected-error (-> cbody expr-type ret-t) (r/Result-type* rng)))]
          (assoc expr
                 expr-type (expr-type cbody)))))))

(add-check-method :import*
  [{:keys [class-str] :as expr} & [expected]]
  (assoc expr
         expr-type (ret r/-nil)))

(add-check-method :case*
  [{:keys [] :as expr} & [expected]]
  #_(prn "Checking case")
  ; tests have no duplicates
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [;_ (prn (:the-expr expr))
          cthe-expr (check (:the-expr expr))
          etype (expr-type cthe-expr)
          ctests (mapv check (:tests expr))
          cthens-and-envs (doall
                            (for [[tst-ret thn] (map vector (map expr-type ctests) (:thens expr))]
                              (let [{{fs+ :then} :fl :as rslt} (tc-equiv := etype tst-ret)
                                    flag+ (atom true)
                                    env-thn (env+ lex/*lexical-env* [fs+] flag+)
                                    then-ret (var-env/with-lexical-env env-thn
                                               (check thn expected))]
                                [(assoc thn
                                        expr-type (expr-type then-ret))
                                 env-thn])))
          ;TODO consider tests that failed to refine env
          cdefault (check (:default expr) expected)
          case-result (let [type (apply c/Un (map (comp :t expr-type) (cons cdefault (map first cthens-and-envs))))
                            ; TODO
                            filter (fo/-FS fl/-top fl/-top)
                            ; TODO
                            object obj/-empty]
                        (ret type filter object))]
      (assoc expr
             expr-type case-result))))

(add-check-method :catch
  [{ecls :class, :keys [handler local-binding] :as expr} & [expected]]
  (let [local-sym (hygienic/hsym-key local-binding)
        local-type (c/RClass-of-with-unknown-params ecls)
        chandler (lex/with-locals {local-sym local-type}
                   (check handler expected))]
    (assoc expr
           expr-type (expr-type chandler))))

; filters don't propagate between components of a `try`, nor outside of it.
(add-check-method :try
  [{:keys [try-expr catch-exprs finally-expr] :as expr} & [expected]]
  (let [ctry-expr (check try-expr expected)
        ccatch-exprs (mapv #(check % expected) catch-exprs)
        _cfinally-expr_ (when finally-expr
                          (check finally-expr))]
    (assoc expr
           expr-type (ret (apply c/Un (-> ctry-expr expr-type ret-t) 
                                 (map (comp ret-t expr-type) ccatch-exprs))))))

(add-check-method :set!
  [{:keys [target val env] :as expr} & [expected]]
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [ctarget (check target)
          cval (check val (expr-type ctarget))
          _ (when-not (sub/subtype? 
                        (-> cval expr-type ret-t)
                        (-> ctarget expr-type ret-t))
              (u/tc-delayed-error (str "Cannot set! " (-> ctarget expr-type ret-t prs/unparse-type pr-str)
                                       " to " (-> cval expr-type ret-t prs/unparse-type pr-str))))]
      (assoc expr
             expr-type (expr-type cval)
             :target ctarget
             :val cval))))

(comment
  ;; error checking
  (cf (if 1 'a 'b) Number)
  )
