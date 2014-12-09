(ns clojure.core.typed.check
  {:skip-wiki true
   :core.typed {:collect-only true}}
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.abo :as abo]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.array-ops :as arr-ops]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.assoc-utils :as assoc-u]
            [clojure.core.typed.check.apply :as apply]
            [clojure.core.typed.check.case :as case]
            [clojure.core.typed.check.set-bang :as set!]
            [clojure.core.typed.check.cli :as cli]
            [clojure.core.typed.check.def :as def]
            [clojure.core.typed.check.do :as do]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.check.get :as get]
            [clojure.core.typed.check.nth :as nth]
            [clojure.core.typed.check.nthnext :as nthnext]
            [clojure.core.typed.check.fn :as fn]
            [clojure.core.typed.check.fn-methods :as fn-methods]
            [clojure.core.typed.check.fn-method-utils :as fn-method-u]
            [clojure.core.typed.check.if :as if]
            [clojure.core.typed.check.invoke :as invoke]
            [clojure.core.typed.check.invoke-kw :as invoke-kw]
            [clojure.core.typed.check.isa :as isa]
            [clojure.core.typed.check.let :as let]
            [clojure.core.typed.check.loop :as loop]
            [clojure.core.typed.check.letfn :as letfn]
            [clojure.core.typed.check.set :as set]
            [clojure.core.typed.check.vector :as vec]
            [clojure.core.typed.check.map :as map]
            [clojure.core.typed.check.monitor :as monitor]
            [clojure.core.typed.check.multi :as multi]
            [clojure.core.typed.check.multi-utils :as multi-u]
            [clojure.core.typed.check.method :as method]
            [clojure.core.typed.check.print-env :as print-env]
            [clojure.core.typed.check.recur :as recur]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.check.tag :as tag]
            [clojure.core.typed.check.type-hints :as type-hints]
            [clojure.core.typed.check.try :as try]
            [clojure.core.typed.check.catch :as catch]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.value :as value]
            [clojure.core.typed.check.special.ann-form :as ann-form]
            [clojure.core.typed.check.special.fn :as special-fn]
            [clojure.core.typed.check.special.tc-ignore :as tc-ignore]
            [clojure.core.typed.check.special.loop :as special-loop]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.cs-gen :as cgen]
            [clojure.core.typed.cs-rep :as crep]
            [clojure.core.typed.ctor-override-env :as ctor-override]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.dvar-env :as dvar-env]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-protocols :as fprotocol]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.fold-rep :as fold]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.method-override-env :as mth-override]
            [clojure.core.typed.method-param-nilables :as mtd-param-nil]
            [clojure.core.typed.method-return-nilables :as mtd-ret-nil]
            [clojure.core.typed.mm-env :as mm]
            [clojure.core.typed.ns-deps :as ns-deps]
            [clojure.core.typed.ns-options :as ns-opts]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.open-result :as open-result]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.protocol-env :as pcl-env]
            [clojure.core.typed.protocol-env :as ptl-env]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.reflect-utils :as reflect-u]
            [clojure.core.typed.subst :as subst]
            [clojure.core.typed.subst-obj :as subst-obj]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.tc-equiv :as equiv]
            [clojure.core.typed.tvar-bnds :as tvar-bnds]
            [clojure.core.typed.tvar-env :as tvar-env]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.update :as update]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.pprint :as pprint]
            [clojure.repl :as repl])
  (:import (clojure.lang IPersistentMap Var Seqable)))

(alter-meta! *ns* assoc :skip-wiki true
             :core.typed {:collect-only true})

(t/ann ^:no-check clojure.core.typed.parse-unparse/*unparse-type-in-ns* (t/U nil t/Sym))
(t/ann ^:no-check clojure.core.typed.util-vars/*already-checked* (t/U nil (t/Atom1 (t/Vec t/Sym))))

;==========================================================
; # Type Checker
;
; The type checker is implemented here.

(declare check-expr)

(defn check-asts [asts]
  (doall
    (for [ast asts]
      (check-expr ast))))

(t/ann check-ns-and-deps [t/Sym -> nil])
(defn check-ns-and-deps 
  "Type check a namespace and its dependencies.
  Assumes type annotations in each namespace
  has already been collected."
  ([nsym]
   {:pre [(symbol? nsym)]
    :post [(nil? %)]}
   (p/p :check-clj/check-ns-and-deps
   (cu/check-ns-and-deps*
     nsym
     {:ast-for-ns ana-clj/ast-for-ns
      :check-asts check-asts
      :check-ns check-ns-and-deps}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(defmulti check (fn [expr & [expected]]
                  {:pre [((some-fn nil? r/TCResult?) expected)]}
                  (:op expr)))

(u/add-defmethod-generator check)

(defn check-expr [expr & [expected]]
  (when vs/*trace-checker*
    (println "Checking line:" (-> expr :env :line))
    (flush))
  (u/p :check/check-expr
    (check expr expected)))

(add-check-method :const [expr & [expected]] 
  (value/check-value expr expected))

(add-check-method :quote [{:keys [expr] :as quote-expr} & [expected]] 
  (let [cexpr (check expr expected)]
    (assoc quote-expr
           :expr cexpr
           u/expr-type (u/expr-type cexpr))))


(add-check-method :map
  [expr & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:keys %))
          (vector? (:vals %))]}
  (map/check-map check expr expected))

(add-check-method :set
  [{:keys [items] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:items %))]}
  (set/check-set check expr expected))

(add-check-method :vector
  [{:keys [items] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:items %))]}
  (vec/check-vector check expr expected))

(add-check-method :var
  [{:keys [var] :as expr} & [expected]]
  {:pre [(var? var)]}
  (binding [vs/*current-expr* expr]
    (let [id (coerce/var->symbol var)
          _ (when-not (var-env/used-var? id)
              (var-env/add-used-var id))
          t (var-env/lookup-Var-nofail (coerce/var->symbol var))]
      (if t
        (assoc expr
               u/expr-type (r/ret t (fo/-FS fl/-top fl/-top) obj/-empty))
        (err/tc-delayed-error
          (str "Unannotated var " id
               "\nHint: Add the annotation for " id
               " via check-ns or cf")
          :return (assoc expr
                         u/expr-type 
                         (r/ret (or (when expected
                                    (r/ret-t expected))
                                  (r/TCError-maker))
                              (fo/-FS fl/-top fl/-top) 
                              obj/-empty)))))))

(add-check-method :the-var
  [{:keys [^Var var env] :as expr} & [expected]]
  {:pre [(var? var)]}
  (let [id (coerce/var->symbol var)
        macro? (.isMacro var)
        _ (when-not (or macro?
                        (var-env/used-var? id))
            (var-env/add-used-var id))
        t (var-env/lookup-Var-nofail id)
        t (cond
            t t
            macro? r/-any
            :else (err/tc-delayed-error (str "Untyped var reference: " id
                                           "\nHint: Add the annotation for " id
                                           " via check-ns or cf")
                                      :form (ast-u/emit-form-fn expr)
                                      :return (r/TCError-maker)))]
    (assoc expr
           u/expr-type (r/ret (c/RClass-of Var [t t])
                          (fo/-true-filter)
                          obj/-empty))))


(comment
  (equiv/tc-equiv (r/ret (r/-val :if))
            (r/ret (prs/parse-type '(U ':if ':case))
                 ))

  )

(defmulti invoke-special (fn [{fexpr :fn :keys [op] :as expr} & args] 
                           {:pre [(#{:invoke} op)]
                            :post [((some-fn nil? symbol?) %)]}
                           (when (#{:var} (:op fexpr))
                             (when-let [var (:var fexpr)]
                               (coerce/var->symbol var)))))
(u/add-defmethod-generator invoke-special)

(defmulti invoke-apply (fn [{[fexpr] :args :keys [op] :as expr} & args]
                         {:pre [(#{:invoke} op)]
                          :post [((some-fn nil? symbol?) %)]}
                         (when (#{:var} (:op fexpr))
                           (when-let [var (:var fexpr)]
                             (coerce/var->symbol var)))))
(u/add-defmethod-generator invoke-apply)

(defmulti static-method-special (fn [expr & args]
                                  {:post [((some-fn nil? symbol?) %)]}
                                  (cu/MethodExpr->qualsym expr)))
(defmulti instance-method-special (fn [expr & args]
                                    {:post [((some-fn nil? symbol?) %)]}
                                    (cu/MethodExpr->qualsym expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups


; only handle special case that the first argument is literal class
(add-invoke-special-method 'clojure.core/cast
  [{:keys [args] :as expr} & [expected]]
  {:post [(or (#{:default} %)
              (and (r/TCResult? (u/expr-type %))
                   (vector? (:args %))))]}
  (when-not (#{2} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/cast,"
                      " expected 2, given " (count args))))
  (let [cargs (mapv check args)
        ct (-> (first cargs) u/expr-type r/ret-t c/fully-resolve-type)]
    (if (and (r/Value? ct) (class? (:val ct)))
      (let [v-t (-> (check (second args)) u/expr-type r/ret-t)
            t (c/In v-t (c/Un r/-nil (c/RClass-of-with-unknown-params (:val ct))))
            _ (when (and t expected)
                (when-not (sub/subtype? t (r/ret-t expected))
                  (cu/expected-error t (r/ret-t expected))))]
        (-> expr
            (update-in [:fn] check)
            (assoc :args cargs
                   u/expr-type (r/ret t))))
      :default)))

(add-invoke-special-method 'clojure.core.typed/var>*
  [{[sym-expr :as args] :args fexpr :fn :as expr} & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{1} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core.typed/var>,"
                      " expected 1, given " (count args))))
  (let [sym (ast-u/quote-expr-val sym-expr)
        _ (assert (symbol? sym))
        t (var-env/lookup-Var-nofail sym)
        _ (when-not t
            (err/tc-delayed-error (str "Unannotated var: " sym)))
        _ (when (and t expected)
            (when-not (sub/subtype? t (r/ret-t expected))
              (cu/expected-error t (r/ret-t expected))))]
    (-> expr
        ; var>* is internal, don't check
        #_(update-in [:fn] check)
        (assoc u/expr-type (r/ret (or t (r/TCError-maker)))))))

; ignore some keyword argument related intersections
(add-invoke-special-method 'clojure.core/seq?
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{1} (count args))
    (err/int-error (str "Wrong number of arguments to clojure.core/seq?,"
                      " expected 1, given " (count args))))
  (let [cfexpr (check fexpr)
        [ctarget :as cargs] (mapv check args)]
    (cond 
      ; handle keyword args macroexpansion
      (r/KwArgsSeq? (-> ctarget u/expr-type r/ret-t))
      (assoc expr
             :fn cfexpr
             :args cargs
             u/expr-type (r/ret r/-true (fo/-true-filter)))
      ; records never extend ISeq
      (r/Record? (-> ctarget u/expr-type r/ret-t c/fully-resolve-type))
      (assoc expr
             :fn cfexpr
             :args cargs
             u/expr-type (r/ret r/-false (fo/-false-filter)))
      :else (invoke/normal-invoke check expr fexpr args expected
                           :cargs cargs))))

(add-invoke-special-method 'clojure.core/extend
  [{[atype & protos :as args] :args :as expr} & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (and atype (even? (count protos))) 
    (err/int-error (str "Wrong number of arguments to extend, expected at least one with an even "
                 "number of variable arguments, given " (count args))))
  (let [catype (check atype)
        ret-expr (-> expr
                     ; don't check extend
                     ;(update-in [:fn] check)
                     (assoc u/expr-type (r/ret r/-nil)))
        ; this is a Value type containing a java.lang.Class instance representing
        ; the type extending the protocol, or (Value nil) if extending to nil
        target-literal-class (r/ret-t (u/expr-type catype))]
    (cond
      (not (and (r/Value? target-literal-class)
                ((some-fn class? nil?) (:val target-literal-class))))
      (err/tc-delayed-error
        (str "Must provide a Class or nil as first argument to extend, "
             "got " (pr-str (prs/unparse-type target-literal-class)))
        :return ret-expr)

      (and expected (not (sub/subtype? r/-any (r/ret-t expected))))
      (do (cu/expected-error r/-any (r/ret-t expected))
          ret-expr)
      :else
      (let [; this is the actual core.typed type of the thing extending the protocol
            target-type (let [v (:val target-literal-class)]
                          (if (nil? v)
                            r/-nil
                            (c/RClass-of-with-unknown-params v)))

            ; build expected types for each method map
            extends (for [[prcl-expr mmap-expr] (partition 2 protos)]
                      (let [protocol (do (when-not (= :var (:op prcl-expr))
                                           (err/int-error  "Must reference protocol directly with var in extend"))
                                         (ptl-env/resolve-protocol (coerce/var->symbol (:var prcl-expr))))
                            _ (when-not (r/Protocol? protocol)
                                (err/int-error (str "Expecting Protocol type, found " protocol)))
                            expected-mmap (c/make-HMap ;get all combinations
                                                       :optional
                                                       (into {}
                                                             (for [[msym mtype] (:methods protocol)]
                                                               [(r/-val (keyword (name msym))) 
                                                                (cu/extend-method-expected target-type mtype)])))]
                        {:expected-hmap expected-mmap
                         :prcl-expr prcl-expr
                         :mmap-expr mmap-expr}))
            cargs (vec
                    (cons catype
                          (mapcat
                            (fn [{:keys [mmap-expr expected-hmap prcl-expr]}]
                              (let [cprcl-expr (check prcl-expr)
                                    cmmap-expr (check mmap-expr (r/ret expected-hmap))
                                    actual (-> cmmap-expr u/expr-type r/ret-t)]
                                [cprcl-expr cmmap-expr]))
                            extends)))
            _ (assert (== (count cargs)
                          (count args)))]
        (assoc ret-expr
               :args cargs)))))

;into-array>
;
; Usage: (into-array> javat cljt coll)
;        (into-array> cljt coll)
(add-invoke-special-method 'clojure.core.typed/into-array>*
  [{:keys [args] :as expr} & [expected]]
  {:post [(and (r/TCResult? (u/expr-type %))
               (vector? (:args %)))]}
  (when-not (#{2 3 4} (count args)) 
    (err/int-error "Wrong number of args to into-array>*"))
  (let [has-java-syn? (#{3 4} (count args))
        [javat-syn cljt-syn coll-expr]
        (cond 
          (= 3 (count args)) args
          (= 4 (count args)) (next args) ;handle temporary hacky case
          :else (cons nil args))

        javat (let [syn (or (when has-java-syn? (ast-u/quote-expr-val javat-syn))  ; generalise javat-syn if provided, otherwise cljt-syn
                            (ast-u/quote-expr-val cljt-syn))
                    c (-> 
                        (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                          (prs/parse-type syn))
                        arr-ops/Type->array-member-Class)]
                (assert (class? c))
                c)
        cljt (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
               (prs/parse-type (ast-u/quote-expr-val cljt-syn)))
        ccoll (check coll-expr (r/ret (c/Un r/-nil (c/RClass-of Seqable [cljt]))))]
    (-> expr
        ; into-array>* is internal, don't check it
        #_(update-in [:fn] check)
        ; the coll is always last
        (assoc :args (-> args pop (conj ccoll))
               u/expr-type (r/ret (r/PrimitiveArray-maker javat cljt cljt))))))

;not
(add-invoke-special-method 'clojure.core/not
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (when-not (= 1 (count args)) 
    (err/int-error "Wrong number of args to clojure.core/not"))
  (let [[ctarget :as cargs] (mapv check args)
        {fs+ :then fs- :else} (-> ctarget u/expr-type r/ret-f)]
    (assoc expr
           :args cargs
           u/expr-type (r/ret (prs/parse-type 'boolean) 
                          ;flip filters
                          (fo/-FS fs- fs+)
                          obj/-empty))))

;get
(add-invoke-special-method 'clojure.core/get
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check args)
        r (get/invoke-get expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check expr fexpr args expected
                     :cargs cargs))))

(defmethod static-method-special 'clojure.lang.RT/get
  [{:keys [args] :as expr} & [expected]]
  {:pre [args]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check args)
        r (get/invoke-get expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (method/check-invoke-method check expr expected false
                                  :cargs cargs))))

;FIXME should be the same as (apply hash-map ..) in invoke-apply
(defmethod static-method-special 'clojure.lang.PersistentHashMap/create
  [{:keys [args] :as expr} & [expected]]
  {:post [(or (#{:default} %)
              (and (-> % u/expr-type r/TCResult?)
                   (vector? (:args %))))]}
  (binding [vs/*current-expr* expr]
    (let [_ (when-not (#{1} (count args)) 
              (err/int-error "Incorrect number of arguments to clojure.lang.PersistentHashMap/create"))
          cargs (mapv check args)
          targett (-> (first cargs) u/expr-type r/ret-t)]
      (cond
        (r/KwArgsSeq? targett)
        (assoc expr
               :args cargs
               u/expr-type (r/ret (c/KwArgsSeq->HMap targett)))
        (r/HeterogeneousSeq? targett)
        (let [res (reduce (fn [t [kt vt]]
                            {:pre [(r/Type? t)]}
                            ;preserve bottom
                            (if (= (c/Un) vt)
                              vt
                              (do (assert (r/HeterogeneousMap? t))
                                  (assoc-in t [:types kt] vt))))
                          (c/-complete-hmap {}) (:types targett))]
          (assoc expr
                 :args cargs
                 u/expr-type (r/ret res)))
        :else :default))))

;; FIXME when updating tools.analyzer past 0.5.0, update :keyword-invoke fields
(add-check-method :keyword-invoke
  [{kw :fn :keys [args] :as expr} & [expected]]
  {:pre [(and (#{:const} (:op kw))
              (keyword? (:val kw)))
         (#{1 2} (count args))]
   :post [(r/TCResult? (u/expr-type %))
          (vector? (:args %))]}
  (let [ckw (check kw)
        cargs (mapv check args)]
    (assoc expr
           :fn ckw
           :args cargs
           u/expr-type (invoke-kw/invoke-keyword
                         expr
                         (u/expr-type ckw)
                         (u/expr-type (first cargs))
                         (when (#{2} (count cargs))
                           (u/expr-type (second cargs)))
                         expected))))

; Will this play nicely with file mapping?
(add-check-method :prim-invoke ; protocol methods
  [expr & [expected]]
  (check (assoc expr :op :invoke)))

(add-check-method :protocol-invoke ; protocol methods
  [expr & [expected]]
  (check (assoc expr :op :invoke)))

;binding
;FIXME use `check-normal-def`
;FIXME record checked-var-def info
(add-invoke-special-method 'clojure.core/push-thread-bindings
  [{[bindings-expr & other-args :as args] :args :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (when-not (empty? other-args)
    (err/int-error (str "push-thread-bindings expected one argument, given " (count args))))
  ; only support (push-thread-bindings (hash-map @~[var bnd ...]))
  ; like `binding`s expansion
  (when-not (and (#{:invoke} (-> bindings-expr :op))
                 (#{#'hash-map} (-> bindings-expr :fn :var))
                 (even? (count (-> bindings-expr :args))))
    (err/nyi-error (str "Can only check push-thread-bindings with a well-formed call to hash-map as first argument"
                      " (like bindings expansion)")))
  (let [new-bindings-exprs (apply hash-map (-> bindings-expr :args))
        cargs
        (vec
          (apply concat
                 (for [[{:keys [op var] :as var-expr} bnd-expr] new-bindings-exprs]
                   (do
                     (assert (#{:the-var} op))
                     (let [expected (var-env/type-of (coerce/var->symbol var))
                           cvar-expr (check var-expr)
                           cexpr (check bnd-expr (r/ret expected))
                           actual (-> cexpr u/expr-type r/ret-t)]
                       (when (not (sub/subtype? actual expected))
                         (err/tc-delayed-error (str "Expected binding for "
                                                  (coerce/var->symbol var)
                                                  " to be: " (prs/unparse-type expected)
                                                  ", Actual: " (prs/unparse-type actual))))
                       [cvar-expr cexpr])))))]
    (-> expr
        ; push-thread-bindings is unannotated
        #_(update-in [:fn] check)
        (assoc :args cargs
               u/expr-type (r/ret r/-nil)))))

(defn swap!-dummy-arg-expr [env [target-expr & [f-expr & args]]]
  (assert f-expr)
  (assert target-expr)
  (let [; transform (swap! t f a ...) to (swap! t (fn [t'] (f t' a ...)))
        ;
        ;generate fresh symbol for function param
        sym (gensym 'swap-val)
        derefed-param (ast-u/dummy-local-binding-expr sym env)
        ;
        ;dummy-fn is (fn [t'] (f t' a ...)) with hygienic bindings
        dummy-fn-expr (ast-u/dummy-fn-expr
                        [; (fn [t'] (f t' a ...))
                         (ast-u/dummy-fn-method-expr
                           ; (f t' a ...)
                           (ast-u/dummy-invoke-expr f-expr
                                              (concat [derefed-param]
                                                      args)
                                              env)
                           [(ast-u/dummy-local-binding-expr sym env)]
                           nil
                           env)]
                        nil
                        env)]
    dummy-fn-expr))

; Any Type Env -> Expr
(defn dummy-ann-form-expr [expr t env]
  (ast-u/dummy-do-expr
    [(ast-u/dummy-const-expr spec/special-form env)
     (ast-u/dummy-const-expr ::t/ann-form env)
     (ast-u/dummy-const-expr 
       {:type (binding [vs/*verbose-types* true]
                (prs/unparse-type t))}
       env)]
    expr
    env))

;; TODO repopulate :args etc.
;swap!
;
; attempt to rewrite a call to swap! to help type inference
(add-invoke-special-method 'clojure.core/swap!
  [{fexpr :fn :keys [args env] :as expr} & [expected]]
  (let [target-expr (first args)
        ctarget-expr (check target-expr)
        target-t (-> ctarget-expr u/expr-type r/ret-t c/fully-resolve-type)
        deref-type (when (and (r/RClass? target-t)
                              (= 'clojure.lang.Atom (:the-class target-t)))
                     (when-not (= 2 (count (:poly? target-t)))
                       (err/int-error (str "Atom takes 2 arguments, found " (count (:poly? target-t)))))
                     (second (:poly? target-t)))
        ]
    (if deref-type
      (cond
        ; TODO if this is a lambda we can do better eg. (swap! (atom> Number 1) (fn [a] a))
        ;(#{:fn} (:op (second args)))

        :else
          (let [dummy-arg (swap!-dummy-arg-expr env args)
                ;_ (prn (ast-u/emit-form-fn dummy-arg) "\n" deref-type)
                expected-dummy-fn-type (r/make-FnIntersection
                                         (r/make-Function
                                           [deref-type]
                                           deref-type))
                delayed-errors (err/-init-delayed-errors)
                actual-dummy-fn-type 
                (binding [vs/*delayed-errors* delayed-errors]
                  (-> (invoke/normal-invoke check 
                                     expr
                                     (ast-u/dummy-var-expr
                                       'clojure.core/swap!
                                       env)
                                     [(first args)
                                      (dummy-ann-form-expr
                                        dummy-arg
                                        expected-dummy-fn-type
                                        env)]
                                     expected)
                      u/expr-type r/ret-t))]
            ;(prn "deref expected" deref-type)
            ;(prn "expected-dummy-fn-type" expected-dummy-fn-type)
            ;(prn "actual-dummy-fn-type" actual-dummy-fn-type)
            ;(prn "subtype?" (sub/subtype? actual-dummy-fn-type expected-dummy-fn-type))
            (if (seq @delayed-errors)
              :default
              (assoc expr
                     u/expr-type (r/ret deref-type)))))
      :default)))

;=
(add-invoke-special-method 'clojure.core/= 
  [{:keys [args] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check args)]
    (-> expr
        (update-in [:fn] check)
        (assoc :args cargs
               u/expr-type (apply equiv/tc-equiv := (map u/expr-type cargs))))))

;identical
(defmethod static-method-special 'clojure.lang.Util/identical
  [{:keys [args] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check args)]
    (assoc expr
           :args cargs
           u/expr-type (apply equiv/tc-equiv := (map u/expr-type cargs)))))

;equiv
(defmethod static-method-special 'clojure.lang.Util/equiv
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)]
    (assoc expr
           :args cargs
           u/expr-type (apply equiv/tc-equiv := (map u/expr-type cargs)))))

;isa? (2 arity is special)
(add-invoke-special-method 'clojure.core/isa?
  [{:keys [args] :as expr} & [expected]]
  (cond
    (#{2} (count args))
    (let [[cchild-expr cparent-expr :as cargs] (mapv check args)]
      (-> expr
          (update-in [:fn] check)
          (assoc :args cargs
                 u/expr-type (isa/tc-isa? (u/expr-type cchild-expr)
                                          (u/expr-type cparent-expr)))))
    :else :default))

;FIXME need to review if any repeated "check"s happen between invoke-apply and specials
;apply
(add-invoke-special-method 'clojure.core/apply
  [expr & [expected]]
  ;(prn "special apply:")
  (let [e (invoke-apply expr expected)]
    (if (= e cu/not-special)
      :default
      e)))


;TODO this should be a special :do op
;manual instantiation
(add-invoke-special-method 'clojure.core.typed/inst-poly
  [{[pexpr targs-exprs :as args] :args :as expr} & [expected]]
  (when-not (#{2} (count args)) 
    (err/int-error "Wrong arguments to inst"))
  (let [ptype (c/fully-resolve-type (-> (check pexpr) u/expr-type r/ret-t))
        ; support (inst :kw ...)
        ptype (if (c/keyword-value? ptype)
                (c/KeywordValue->Fn ptype)
                ptype)]
    (if-not ((some-fn r/Poly? r/PolyDots?) ptype)
      (binding [vs/*current-expr* pexpr]
        (err/tc-delayed-error (str "Cannot instantiate non-polymorphic type: " (prs/unparse-type ptype))
                            :return (assoc expr
                                           u/expr-type (cu/error-ret expected))))
      (let [targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)
                            vs/*current-expr* expr]
                    (doall (map prs/parse-type (ast-u/quote-expr-val targs-exprs))))]
        (assoc expr
               u/expr-type (r/ret 
                             (binding [prs/*unparse-type-in-ns* (cu/expr-ns expr)
                                       vs/*current-expr* expr]
                               (inst/manual-inst ptype targs))))))))

(defonce ^:dynamic *inst-ctor-types* nil)
(set-validator! #'*inst-ctor-types* (some-fn nil? (con/every-c? r/Type?)))

;TODO this should be a special :do op
;manual instantiation for calls to polymorphic constructors
(add-invoke-special-method 'clojure.core.typed/inst-poly-ctor
  [{[ctor-expr targs-exprs] :args :as expr} & [expected]]
  (let [targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                (mapv prs/parse-type (ast-u/quote-expr-val targs-exprs)))
        cexpr (binding [*inst-ctor-types* targs]
                (check ctor-expr))]
    (assoc expr
           u/expr-type (u/expr-type cexpr))))

;debug printing
(add-invoke-special-method 'clojure.core.typed/print-env
  [{[debug-string :as args] :args :as expr} & [expected]]
  (when-not (#{1} (count args)) 
    (err/int-error (str "Wrong arguments to print-env, Expected 1, found " (count args))))
  (when-not (= :const (:op debug-string))
    (err/int-error "Must pass print-env a string literal"))
  ;DO NOT REMOVE
  (println (:val debug-string))
  (flush)
  (prs/with-unparse-ns (cu/expr-ns expr)
    (print-env/print-env*))
  ;DO NOT REMOVE
  (assoc expr
         u/expr-type (r/ret r/-nil (fo/-false-filter) obj/-empty)))

;filter printing
(add-invoke-special-method 'clojure.core.typed/print-filterset
  [{[debug-string form :as args] :args :as expr} & [expected]]
  (when-not (#{2} (count args)) 
    (err/int-error (str "Wrong arguments to print-filterset. Expected 2, found " (count args))))
  (when-not (= :const (:op debug-string)) 
    (err/int-error "Must pass print-filterset a string literal as the first argument."))
  (let [cform (check form expected)
        cargs [debug-string cform]
        t (u/expr-type cform)]
    ;DO NOT REMOVE
    (println (:val debug-string))
    (flush)
    ;(prn (:fl t))
    (prs/with-unparse-ns (cu/expr-ns expr)
      (if (fl/FilterSet? (:fl t))
        (do (pprint/pprint (prs/unparse-filter-set (:fl t)))
            (flush))
        (prn (:fl t)))
      (prn (prs/unparse-object (:o t)))
      (prn 'Flow (prs/unparse-filter (-> t :flow r/flow-normal))))
    ;DO NOT REMOVE
    (assoc expr
           :args cargs
           u/expr-type t)))

;unchecked casting
(add-invoke-special-method 'clojure.core.typed.unsafe/ignore-with-unchecked-cast*
  [{[frm quote-expr] :args, :keys [env], :as expr} & [expected]]
  (let [tsyn (ast-u/quote-expr-val quote-expr)
        parsed-ty (binding [vs/*current-env* env
                            prs/*parse-type-in-ns* (cu/expr-ns expr)]
                    (prs/parse-type tsyn))]
    (assoc expr
           u/expr-type (r/ret parsed-ty))))

;pred
(add-invoke-special-method 'clojure.core.typed/pred*
  [{[tsyn-expr nsym-expr _pred-fn_ :as args] 
    :args, :keys [env], :as expr} & [expected]]
  {:pre [(#{3} (count args))]}
  (let [tsyn (ast-u/quote-expr-val tsyn-expr)
        nsym (ast-u/quote-expr-val nsym-expr)
        ptype 
        ; frees are not scoped when pred's are parsed at runtime,
        ; so we simulate the same here.
        (binding [tvar-env/*current-tvars* {}
                  dvar-env/*dotted-scope* {}]
          (prs/with-parse-ns nsym
            (prs/parse-type tsyn)))]
    (assoc expr
           u/expr-type (r/ret (prs/predicate-for ptype)))))

;fn literal
(add-invoke-special-method 'clojure.core.typed/fn>-ann
  [{:keys [args] :as expr} & [expected]]
  (let [[fexpr quote-expr] args
        type-syns (ast-u/quote-expr-val quote-expr)
        expected
        (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
          (apply
            r/make-FnIntersection
            (doall
              (for [{:keys [dom-syntax has-rng? rng-syntax]} type-syns]
                (r/make-Function (mapv prs/parse-type dom-syntax)
                                 (if has-rng?
                                   (prs/parse-type rng-syntax)
                                   r/-any))))))
        cfexpr (check fexpr (r/ret expected))
        cargs [cfexpr quote-expr]]
    (assoc expr
           :args cargs
           u/expr-type (u/expr-type cfexpr))))

;polymorphic fn literal
(add-invoke-special-method 'clojure.core.typed/pfn>-ann
  [{:keys [args] :as expr} & [expected]]
  (assert false "pfn> NYI")
         ;FIXME these are :quote exprs
  #_(let [[fexpr {poly-decl :val} {method-types-syn :val}] args
        frees-with-bounds (map prs/parse-free poly-decl)
        method-types (free-ops/with-bounded-frees frees-with-bounds
                       (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                         (doall 
                           (for [{:keys [dom-syntax has-rng? rng-syntax]} method-types-syn]
                             {:dom (doall (map prs/parse-type dom-syntax))
                              :rng (if has-rng?
                                     (prs/parse-type rng-syntax)
                                     r/-any)}))))
        cexpr (-> (check-anon-fn fexpr method-types :poly frees-with-bounds)
                  (update-in [u/expr-type :t] (fn [fin] (c/Poly* (map first frees-with-bounds) 
                                                             (map second frees-with-bounds)
                                                             fin))))]
    cexpr))

;loop
(add-invoke-special-method 'clojure.core.typed/loop>-ann
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [[loop-expr expected-quote-expr] args
        expected-bnds-syn (ast-u/quote-expr-val expected-quote-expr)
        expected-bnds (binding [prs/*parse-type-in-ns* (cu/expr-ns loop-expr)]
                        (mapv prs/parse-type expected-bnds-syn))
        cloop-expr
        ;loop may be nested, type the first loop found
        (binding [recur-u/*loop-bnd-anns* expected-bnds]
          (check loop-expr expected))
        cargs [cloop-expr expected-quote-expr]]
    (assoc expr
           :args cargs
           u/expr-type (u/expr-type cloop-expr))))

;seq
(add-invoke-special-method 'clojure.core/seq
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [_ (assert (#{1} (count args))
                  "Wrong number of arguments to seq")
        [ccoll :as cargs] (mapv check args)]
    ;(prn "special seq: ccoll type" (prs/unparse-type (r/ret-t (u/expr-type ccoll))))
    (cond
      ; for (apply hash-map (seq kws)) macroexpansion of keyword args
      (r/KwArgsSeq? (r/ret-t (u/expr-type ccoll)))
      (assoc expr
             :args cargs
             u/expr-type (u/expr-type ccoll))

      :else (invoke/normal-invoke check expr fexpr args expected :cargs cargs))))

;make vector
(add-invoke-special-method 'clojure.core/vector
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [cargs (mapv check args)]
    (-> expr
        (update-in [:fn] check)
        (assoc 
          :args cargs
          u/expr-type (r/ret (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                                  :filters (mapv (comp r/ret-f u/expr-type) cargs)
                                  :objects (mapv (comp r/ret-o u/expr-type) cargs)))))))

;make hash-map
(add-invoke-special-method 'clojure.core/hash-map
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [cargs (mapv check args)]
    (cond
      (every? r/Value? (keys (apply hash-map (mapv (comp r/ret-t u/expr-type) cargs))))
      (-> expr
        (update-in [:fn] check)
        (assoc :args cargs
               u/expr-type (r/ret (c/-complete-hmap
                                (apply hash-map (mapv (comp r/ret-t u/expr-type) cargs))))))
      :else (invoke/normal-invoke check expr fexpr args expected :cargs cargs))))

;(apply concat hmap)
(add-invoke-apply-method 'clojure.core/concat
  [{[_ & args] :args :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [cargs (mapv check args)
        tmap (when (#{1} (count cargs))
               (c/fully-resolve-type (r/ret-t (u/expr-type (last cargs)))))]
    (binding [vs/*current-expr* expr]
      (cond
        tmap
        (let [r (c/HMap->KwArgsSeq tmap false)
              _ (when expected
                  (when-not (sub/subtype? r (r/ret-t expected))
                    (cu/expected-error r (r/ret-t expected))))]
          (-> expr
              (update-in [:fn] check)
              (assoc u/expr-type (r/ret r))))
        :else cu/not-special))))

;apply hash-map
(add-invoke-apply-method 'clojure.core/hash-map
  [{[_ & args] :args :as expr} & [expected]]
  {:post [(or 
            (and (-> % u/expr-type r/TCResult?)
                 (vector? (:args %)))
            (= % cu/not-special))]}
  (let [cargs (mapv check args)]
    ;(prn "apply special (hash-map): ")
    (cond
      (and (#{1} (count cargs))
           (r/KwArgsSeq? (u/expr-type (last cargs))))
      (-> expr
          (update-in [:fn] check)
          (assoc :args cargs
                 u/expr-type (r/ret (c/KwArgsSeq->HMap (-> (u/expr-type (last cargs)) r/ret-t)))))

      (and (seq cargs)
           ((some-fn r/HeterogeneousVector? r/HeterogeneousList? r/HeterogeneousSeq?) 
            (r/ret-t (u/expr-type (last cargs))))
           ;; every key must be a Value
           (every? r/Value? (keys (apply hash-map (concat (map (comp r/ret-t u/expr-type) (butlast cargs))
                                                          (mapcat vector (:types (r/ret-t (u/expr-type (last cargs))))))))))
      (-> expr
          (update-in [:fn] check)
          (assoc :args cargs
                 u/expr-type (r/ret (c/-complete-hmap
                                  (apply hash-map (concat (map (comp r/ret-t u/expr-type) (butlast cargs))
                                                          (mapcat vector (:types (r/ret-t (u/expr-type (last cargs)))))))))))
      :else cu/not-special)))


;nth
(defmethod static-method-special 'clojure.lang.RT/nth
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check args)
        r (nth/invoke-nth check expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (method/check-invoke-method check expr expected false
                                  :cargs cargs))))

;nthnext
(add-invoke-special-method 'clojure.core/nthnext
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [cargs (mapv check args)
        r (nthnext/check-nthnext check expr expected :cargs cargs)]
    (if-not (#{cu/not-special} r)
      r
      (invoke/normal-invoke check expr fexpr args expected :cargs cargs))))

;assoc
(add-invoke-special-method 'clojure.core/assoc
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [[target & keyvals] args

        _ (when-not (<= 3 (count args))
            (err/int-error (str "assoc accepts at least 3 arguments, found "
                                     (count args))))
        _ (when-not (even? (count keyvals))
            (err/int-error "assoc accepts an even number of keyvals"))

        ctarget (check target)
        targetun (-> ctarget u/expr-type r/ret-t)
        ckeyvals (doall (map check keyvals))
        keypair-types (partition 2 (map u/expr-type ckeyvals))
        cargs (vec (cons ctarget ckeyvals))]
    (if-let [new-hmaps (apply assoc-u/assoc-type-pairs targetun keypair-types)]
      (-> expr
        (update-in [:fn] check)
        (assoc
          :args cargs
          u/expr-type (r/ret new-hmaps
                         (fo/-true-filter) ;assoc never returns nil
                         obj/-empty)))
      
      ;; to do: improve this error message
      (err/tc-delayed-error (str "Cannot assoc args `"
                               (clojure.string/join " "
                                 (map (comp prs/unparse-type u/expr-type) ckeyvals))
                               "` on "
                               (prs/unparse-type targetun))
                          :return (-> expr
                                      (update-in [:fn] check)
                                      (assoc
                                        :args cargs
                                        u/expr-type (cu/error-ret expected)))))
    ))

(add-invoke-special-method 'clojure.core/dissoc
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(or (= % :default) (-> % u/expr-type r/TCResult?))]}
  (let [_ (when-not (seq args)
            (err/int-error (str "dissoc takes at least one argument, given: " (count args))))
        [ctarget & cdissoc-args :as cargs] (mapv check args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cdissoc-args)]
    (if-let [new-t (assoc-u/dissoc-keys ttarget targs)]
      (-> expr
          (update-in [:fn] check)
          (assoc
            :args cargs
            u/expr-type (r/ret new-t)))
      (invoke/normal-invoke check expr fexpr args expected
                     :cargs cargs))))

; merge
(add-invoke-special-method 'clojure.core/merge
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (let [[ctarget & cmerge-args :as cargs] (mapv check args)
        basemap (-> ctarget u/expr-type r/ret-t c/fully-resolve-type)
        targs (map u/expr-type cmerge-args)]
    (if-let [merged (apply assoc-u/merge-types basemap targs)]
      (-> expr
          (update-in [:fn] check)
          (assoc :args cargs
                 u/expr-type (r/ret merged
                                (fo/-true-filter) ;assoc never returns nil
                                obj/-empty)))
      (invoke/normal-invoke check expr fexpr args expected
                     :cargs cargs))))

;conj
(add-invoke-special-method 'clojure.core/conj
  [{fexpr :fn :keys [args] :as expr} & [expected]]
  (let [[ctarget & cconj-args :as cargs] (mapv check args)
        ttarget (-> ctarget u/expr-type r/ret-t)
        targs (map u/expr-type cconj-args)]
    (if-let [conjed (apply assoc-u/conj-types ttarget targs)]
      (-> expr
          (update-in [:fn] check)
          (assoc :args cargs
                 u/expr-type (r/ret conjed
                                (fo/-true-filter) ; conj never returns nil
                                obj/-empty)))
      (invoke/normal-invoke check expr fexpr args expected
                     :cargs cargs))))

#_(add-invoke-special-method 'clojure.core/update-in
  [{:keys [args env] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [error-expr (-> expr
                         (update-in [:fn] check)
                         (assoc u/expr-type (r/ret (r/TCError-maker))))]
      (cond
        (not (< 3 (count args))) (err/tc-delayed-error (str "update-in takes at least 3 arguments"
                                                          ", actual " (count args))
                                                     :return error-expr)

        :else
        (let [[ctarget-expr cpath-expr cfn-expr & more-exprs] (doall (map check args))
              path-type (-> cpath-expr u/expr-type r/ret-t c/fully-resolve-type)]
          (if (not (HeterogeneousVector? path-type))
            (err/tc-delayed-error (str "Can only check update-in with vector as second argument")
                                :return error-expr)
            (let [path (:types path-type)
                  follow-path (reduce (fn [t pth]
                                        (when t
                                          ))
                                      (-> ctarget-expr u/expr-type r/ret-t)
                                      path)]
              (assert nil))))))))
        

(comment
  (method-expected-type (prs/parse-type '[Any -> Any])
                        (prs/parse-type '(Value :op))
                        (prs/parse-type '(Value :if)))
  ;=> ['{:if Any} -> Any]
  )

; cli
;TODO add cargs to result
(add-invoke-special-method 'clojure.tools.cli/cli
  [{[args-expr & specs-exprs] :args :keys [env] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:args %))]}
  (binding [vs/*current-env* env]
    (let [args-expected-ty (prs/parse-type `(t/U nil (clojure.lang.Seqable String)))
          cargs-expr (binding [vs/*current-env* (:env args-expr)]
                       (check args-expr))
          _ (when-not (sub/subtype? 
                        (-> cargs-expr u/expr-type r/ret-t)
                        args-expected-ty)
              (binding [vs/*current-env* (:env args-expr)]
                (cu/expected-error (-> cargs-expr u/expr-type r/ret-t) args-expected-ty)))
          spec-map-ty (reduce (fn [t spec-expr]
                                (if-let [[keyt valt] (cli/parse-cli-spec check spec-expr)]
                                  (-> t
                                    (assoc-in [:types keyt] valt))
                                  ; resort to a general type
                                  (do
                                    ;(prn "cli: giving up because of" (ast-u/emit-form-fn spec-expr)
                                         ;"\n" spec-expr)
                                    (reduced 
                                      (c/RClass-of IPersistentMap [(c/RClass-of clojure.lang.Keyword) r/-any])))))
                              (c/-complete-hmap {})
                              specs-exprs)

          actual (r/-hvec [spec-map-ty 
                           (prs/parse-type `(clojure.lang.Seqable String))
                           (prs/parse-type `String)])
          _ (when expected
              (when-not (sub/subtype? actual (r/ret-t expected))
                (cu/expected-error 
                  actual (r/ret-t expected))))
          cargs (vec (cons cargs-expr specs-exprs))]
      (-> expr
        (update-in [:fn] check)
        (assoc :args cargs
               u/expr-type (r/ret actual))))))

; FIXME this needs a line number from somewhere!
(defmethod instance-method-special 'clojure.lang.MultiFn/addMethod
  [{[dispatch-val-expr method-expr :as args] :args target :instance :keys [env] :as expr} & [expected]]
  (when-not (= 2 (count args))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn/addMethod"))
  (when-not (#{:var} (:op target))
    (err/int-error "Must call addMethod with a literal var"))
  (let [var (:var target)
        _ (assert (var? var))
        mmsym (coerce/var->symbol var)
        ret-expr (assoc expr
                        u/expr-type (r/ret (c/RClass-of clojure.lang.MultiFn)))
        default? (cu/default-defmethod? var (ast-u/emit-form-fn dispatch-val-expr))]
    (cond
      ;skip if warn-on-unannotated-vars is in effect
      (or (and (ns-opts/warn-on-unannotated-vars? (cu/expr-ns expr))
               (not (var-env/lookup-Var-nofail mmsym)))
          (not (var-env/check-var? mmsym)))
      (do (u/tc-warning (str "Not checking defmethod" mmsym "with dispatch value" (ast-u/emit-form-fn dispatch-val-expr)))
          ret-expr)
      :else
      (let [_ (assert (#{:var} (:op target)))
            _ (when-not (#{:fn} (:op method-expr))
                (err/int-error (str "Method must be a fn")))
            ctarget (check target)
            cdispatch-val-expr (check dispatch-val-expr)
            dispatch-type (mm/multimethod-dispatch-type mmsym)]
        (if-not dispatch-type
          (binding [vs/*current-env* env]
            (err/tc-delayed-error (str "Multimethod requires dispatch type: " mmsym
                                       "\n\nHint: defmulti must be checked before its defmethods")
                                  :return (assoc ret-expr
                                               :instance ctarget)))
          (let [method-expected (var-env/type-of mmsym)
                cmethod-expr 
                (binding [multi-u/*current-mm* 
                          (when-not default?
                            {:dispatch-fn-type dispatch-type
                             :dispatch-val-ret (u/expr-type cdispatch-val-expr)})]
                  (check method-expr (r/ret method-expected)))
                cargs [cdispatch-val-expr cmethod-expr]]
            (assoc ret-expr
                   :instance ctarget
                   :args cargs)))))))

(add-invoke-special-method :default [& args] :default)
(defmethod static-method-special :default [& args] :default)
(defmethod instance-method-special :default [& args] :default)


;TODO attach new :args etc.
;convert apply to normal function application
(add-invoke-apply-method :default 
  [expr & [expected]]
  (let [t (apply/check-apply check expr expected)]
    (if (= t cu/not-special)
      t
      (assoc expr
             u/expr-type t))))

(add-check-method :invoke
  [{fexpr :fn :keys [args env] :as expr} & [expected]]
  {:post [(r/TCResult? (u/expr-type %))
          (vector? (:args %))
          #_(-> % :fn u/expr-type r/TCResult?)]}
  #_(prn "invoke:" ((some-fn :var :keyword :op) fexpr))
  (binding [vs/*current-env* env]
    (let [e (invoke-special expr expected)]
      (if (not= :default e) 
        e
        (let [cfexpr (check fexpr)]
          (cond
            (c/keyword-value? (r/ret-t (u/expr-type cfexpr)))
            (let [[ctarget cdefault :as cargs] (mapv check args)]
              (assert (<= 1 (count args) 2))
              (assoc expr
                     :fn cfexpr
                     :args cargs
                     u/expr-type (invoke-kw/invoke-keyword 
                                   expr
                                   (u/expr-type cfexpr)
                                   (u/expr-type ctarget)
                                   (when cdefault
                                     (u/expr-type cdefault)) 
                                   expected)))

            :else (invoke/normal-invoke check expr fexpr args expected :cfexpr cfexpr)))))))

(add-check-method :fn
  [{:keys [env] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:methods %))]}
  (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
            vs/*current-expr* expr
            fn-method-u/*check-fn-method1-checkfn* check
            fn-method-u/*check-fn-method1-rest-type*
              (fn [remain-dom rest drest kws]
                {:pre [(or (r/Type? rest)
                           (r/DottedPretype? drest)
                           (r/KwArgs? kws))
                       (#{1} (count (filter identity [rest drest kws])))
                       (every? r/Type? remain-dom)]
                 :post [(r/Type? %)]}
                (cond
                  (or rest drest)
                  ; rest argument is always a nilable non-empty seq. Could
                  ; be slightly more clever here if we have a `rest`, but what about
                  ; `drest`?
                  (c/Un r/-nil 
                        (c/In (r/-hseq remain-dom
                                       :rest rest
                                       :drest drest)
                              (r/make-CountRange 1)))

                  :else (c/KwArgs->Type kws)))]
    (let [cexpr (fn/check-fn 
                  expr 
                  (let [default-ret (r/ret (r/make-FnIntersection
                                             (r/make-Function [] r/-any r/-any)))]
                    (cond (and expected (not= r/-any (r/ret-t expected))) expected
                          :else default-ret)))
          #_#__ (when expected
              (let [actual (r/ret-t (u/expr-type cexpr))]
                (when-not (sub/subtype? actual (r/ret-t expected))
                  (cu/expected-error actual (r/ret-t expected)))))]
      cexpr)))


;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form ::t/tc-ignore
  [expr expected]
  (tc-ignore/check-tc-ignore check expr expected))

(defmethod internal-special-form ::t/tag
  [{[_ _ {{tag :tag} :val} :as statements] :statements :keys [ret] :as expr} expected]
  {:pre [(#{3} (count statements))]
   :post [(-> % u/expr-type r/TCResult?)]}
  (let [cret (binding [vs/*current-expr* ret]
               (check ret expected))
        _ (binding [vs/*current-expr* expr] ;check-tag overrides current-expr when needed
            (tag/check-tag ret tag))]
    (assoc expr
           :ret cret
           u/expr-type (u/expr-type cret))))

(defmethod internal-special-form ::t/fn
  [{[_ _ {{fn-anns :ann} :val} :as statements] :statements fexpr :ret :as expr} expected]
  (special-fn/check-special-fn check expr expected))

(defmethod internal-special-form ::t/ann-form
  [{[_ _ {{tsyn :type} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (ann-form/check-ann-form check expr expected))

(defmethod internal-special-form ::t/loop
  [{[_ _ {{tsyns :ann} :val} :as statements] :statements frm :ret, :keys [env], :as expr} expected]
  (special-loop/check-special-loop check expr expected))

(defmethod internal-special-form :default
  [expr expected]
  (err/int-error (str "No such internal form: " (ast-u/emit-form-fn expr))))

(add-check-method :do
  [expr & [expected]]
  {:post [(r/TCResult? (u/expr-type %))
          (vector? (:statements %))]}
  (do/check-do check internal-special-form expr expected))

(add-check-method :monitor-enter
                  [expr & [expected]]
                  (monitor/check-monitor check expr expected))

(add-check-method :monitor-exit
                  [expr & [expected]]
                  (monitor/check-monitor check expr expected))


(add-check-method :local
  [{sym :name :as expr} & [expected]]
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [t (var-env/type-of sym)
          _ (when (and expected
                       (not (sub/subtype? t (r/ret-t expected))))
              (prs/with-unparse-ns (cu/expr-ns expr)
                (err/tc-delayed-error 
                  (str "Local binding " sym " expected type " (pr-str (prs/unparse-type (r/ret-t expected)))
                       ", but actual type " (pr-str (prs/unparse-type t)))
                  :form (ast-u/emit-form-fn expr))))]
      (assoc expr
             u/expr-type (r/ret t 
                            (fo/-FS (fo/-not-filter (c/Un r/-nil r/-false) sym)
                                    (fo/-filter (c/Un r/-nil r/-false) sym))
                            (obj/-path nil sym))))))

(add-check-method :host-interop
  [{:keys [m-or-f target] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [ctarget (check target)]
    (err/tc-delayed-error (str "Unresolved host interop: " m-or-f
                             (type-hints/suggest-type-hints 
                               m-or-f 
                               (-> ctarget u/expr-type r/ret-t) 
                               [])
                             "\n\nHint: use *warn-on-reflection* to identify reflective calls"
                             "\n\nin: " (ast-u/emit-form-fn expr)))
    (assoc expr 
           :target ctarget
           u/expr-type (cu/error-ret expected))))

(add-check-method :static-call
  [expr & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  #_(prn "static-method")
  (let [spec (static-method-special expr expected)]
    (if (not= :default spec)
      spec
      (method/check-invoke-method check expr expected false))))

(add-check-method :instance-call
  [expr & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (if (contains? % :args)
            (vector? (:args %))
            true)]}
  (let [spec (instance-method-special expr expected)]
    (if (not= :default spec)
      spec
      (method/check-invoke-method check expr expected true))))

(add-check-method :static-field
  [expr & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [field (cu/FieldExpr->Field expr)]
    (assert field)
    (assoc expr
           u/expr-type (r/ret (cu/Field->Type field)))))

(add-check-method :instance-field
  [{target :instance target-class :class field-name :field :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  #_(prn "instance-field:" expr)
  (binding [vs/*current-expr* expr]
   (let [field (cu/FieldExpr->Field expr)
         cexpr (check target)]
    (if-not target-class
      ; I think target-class will never be false
      (err/tc-delayed-error (str "Call to instance field "
                               (symbol field-name)
                               " requires type hints."
                               (type-hints/suggest-type-hints 
                                 field-name 
                                 (-> cexpr u/expr-type r/ret-t)
                                 []))
                          :form (ast-u/emit-form-fn expr)
                          :return (assoc expr
                                         :instance cexpr
                                         u/expr-type (cu/error-ret expected)))
      (let [_ (assert (class? target-class))
            fsym (symbol field-name)
            ; check that the hinted class at least matches the runtime class we expect
            _ (let [expr-ty (c/fully-resolve-type (-> cexpr u/expr-type r/ret-t))
                    cls (cond
                          (r/DataType? expr-ty) (coerce/symbol->Class (:the-class expr-ty))
                          (r/RClass? expr-ty) (coerce/symbol->Class (:the-class expr-ty)))]
                (when-not (and cls
                               ; in case target-class has been redefined
                               (sub/class-isa? cls (-> target-class coerce/Class->symbol coerce/symbol->Class)))
                  (err/tc-delayed-error (str "Instance field " fsym " expected "
                                           (pr-str target-class)
                                           ", actual " (pr-str (prs/unparse-type expr-ty)))
                                      :form (ast-u/emit-form-fn expr))))

            ; datatype fields are special
            result-t (if-let [override (when-let [dtp (dt-env/get-datatype (coerce/Class->symbol target-class))]
                                         (let [dt (if (r/Poly? dtp)
                                                    ;generate new names
                                                    (cu/unwrap-datatype dtp (repeatedly (:nbound dtp) gensym))
                                                    dtp)
                                               _ (assert ((some-fn r/DataType? r/Record?) dt))
                                               demunged (symbol (repl/demunge (str fsym)))]
                                           (-> (c/DataType-fields* dt) (get demunged))))]
                       override
                       ; if not a datatype field, convert as normal
                       (if field
                         (cu/Field->Type field)
                         (err/tc-delayed-error (str "Instance field " fsym " needs type hints")
                                             :form (ast-u/emit-form-fn expr)
                                             :return (r/TCError-maker))))] 
        (assoc expr
               :instance cexpr
               u/expr-type (r/ret result-t)))))))


(add-check-method :instance?
  [{cls :class the-expr :target :as expr} & [expected]]
  (let [inst-of (c/RClass-of-with-unknown-params cls)
        cexpr (check the-expr)
        expr-tr (u/expr-type cexpr)]
    (assoc expr
           :target cexpr
           u/expr-type (r/ret (c/Un r/-true r/-false)
                          (fo/-FS (fo/-filter-at inst-of (r/ret-o expr-tr))
                                  (fo/-not-filter-at inst-of (r/ret-o expr-tr)))
                          obj/-empty))))

(defmulti new-special (fn [{:keys [class] :as expr} & [expected]] (coerce/ctor-Class->symbol class)))


(defmethod new-special 'clojure.lang.MultiFn
  [{[nme-expr dispatch-expr default-expr hierarchy-expr :as args] :args :as expr} & [expected]]
  (when-not expected
    (err/int-error "clojure.lang.MultiFn constructor requires an expected type"))
  (when-not (== 4 (count args))
    (err/int-error "Wrong arguments to clojure.lang.MultiFn constructor"))
  (when-not (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
    (err/int-error "Multimethod hierarchy cannot be customised"))
  (when-not (= (:val default-expr) :default)
    (err/int-error "Non :default default dispatch value NYI"))
  (let [mm-name (:val nme-expr)
        _ (when-not (string? mm-name)
            (err/int-error "MultiFn name must be a literal string"))
        mm-qual (symbol (str (cu/expr-ns expr)) mm-name)
        ;_ (prn "mm-qual" mm-qual)
        ;_ (prn "expected r/ret-t" (prs/unparse-type (r/ret-t expected)))
        ;_ (prn "expected r/ret-t class" (class (r/ret-t expected)))
        expected-mm-disp (multi/expected-dispatch-type (r/ret-t expected))
        cdisp (check dispatch-expr (r/ret expected-mm-disp))
        cargs [(check nme-expr)
               cdisp
               (check default-expr)
               (check hierarchy-expr)]
        _ (assert (== (count cargs) (count args)))
        _ (mm/add-multimethod-dispatch-type mm-qual (r/ret-t (u/expr-type cdisp)))]
    (assoc expr
           :args cargs
           u/expr-type (r/ret (c/In (c/RClass-of clojure.lang.MultiFn) (r/ret-t expected))))))

(defmethod new-special :default [expr & [expected]] cu/not-special)

(add-check-method :new
  [{cls :class :keys [args env] :as expr} & [expected]]
  {:post [(vector? (:args %))
          (-> % u/expr-type r/TCResult?)]}
  (binding [vs/*current-expr* expr
            vs/*current-env* env]
    (let [ctor (cu/NewExpr->Ctor expr)
          spec (new-special expr expected)]
      (cond
        (not= cu/not-special spec) spec
        :else
        (let [inst-types *inst-ctor-types*
              cls (ast-u/new-op-class expr)
              clssym (coerce/ctor-Class->symbol cls)
              cargs (mapv check args)
              ctor-fn (or (@ctor-override/CONSTRUCTOR-OVERRIDE-ENV clssym)
                          (and (dt-env/get-datatype clssym)
                               (cu/DataType-ctor-type clssym))
                          (when ctor
                            (cu/Constructor->Function ctor)))]
          (if-not ctor-fn
            (err/tc-delayed-error (str "Unresolved constructor invocation " 
                                     (type-hints/suggest-type-hints 
                                       nil 
                                       nil 
                                       (map (comp r/ret-t u/expr-type) cargs)
                                       :constructor-call clssym)
                                     ".\n\nHint: add type hints"
                                     "\n\nin: " (ast-u/emit-form-fn expr))
                                :form (ast-u/emit-form-fn expr)
                                :return (assoc expr
                                               :args cargs
                                               u/expr-type (cu/error-ret expected)))
            (let [ctor-fn (if inst-types
                            (inst/manual-inst ctor-fn inst-types)
                            ctor-fn)
                  ifn (r/ret ctor-fn)
                  ;_ (prn "Expected constructor" (prs/unparse-type (r/ret-t ifn)))
                  res-type (funapp/check-funapp expr args ifn (map u/expr-type cargs) nil)
                  _ (when (and expected (not (sub/subtype? (r/ret-t res-type) (r/ret-t expected))))
                      (cu/expected-error (r/ret-t res-type) (r/ret-t expected)))]
              (assoc expr
                     :args cargs
                     u/expr-type res-type))))))))

(add-check-method :throw
  [{:keys [exception] :as expr} & [expected]]
  (let [cexception (check exception)
        _ (when-not (sub/subtype? (r/ret-t (u/expr-type cexception))
                                  (c/RClass-of Throwable))
            (err/tc-delayed-error (str "Cannot throw: "
                                     (prs/unparse-type (r/ret-t (u/expr-type cexception))))))]
    (assoc expr
           :exception cexception
           u/expr-type (r/ret (c/Un)
                          (fo/-FS fl/-top fl/-top) 
                          obj/-empty
                          ;never returns normally
                          (r/-flow fl/-bot)))))

(add-check-method :recur
  [{args :exprs :keys [env] :as expr} & [expected]]
  {:post [(vector? (:exprs %))]}
  (recur/check-recur args env expr expected check))

(add-check-method :loop
  [{binding-inits :bindings :keys [body] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (loop/check-loop check expr expected))

(add-check-method :let
  [{bindings :bindings :keys [body] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let/check-let check expr expected))

(add-check-method :letfn
  [{bindings :bindings :keys [body] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (letfn/check-letfn bindings body expr expected check))

(add-check-method :with-meta
  [{:keys [expr meta] :as with-meta-expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [cexpr (check expr expected)
        cmeta (check meta)]
    (assoc with-meta-expr 
           :expr cexpr
           :meta cmeta
           u/expr-type (u/expr-type cexpr))))


(add-check-method :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [ctest (binding [vs/*current-expr* test]
                (check test))]
    (if/check-if check expr ctest then else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethods

(add-check-method :def
  [{:keys [var init init-provided env] :as expr} & [expected]]
  ;(prn "Checking def" var)
  (let [init-provided (contains? expr :init)]
    (binding [vs/*current-env* (if (:line env) env vs/*current-env*)
              vs/*current-expr* expr]
      (cond 
        ;ignore macro definitions and declare
        (or (.isMacro ^Var var)
            (not init-provided))
        (let [actual-t (c/RClass-of Var [(r/Bottom) r/-any])
              _ (when (and expected
                           (not (sub/subtype? actual-t (r/ret-t expected))))
                  (cu/expected-error actual-t (r/ret-t expected)))]
          (assoc expr
                 u/expr-type (r/ret actual-t)))

        :else (def/check-normal-def check expr expected)))))

(add-check-method :deftype
  [{expired-class :class-name :keys [fields methods env] :as expr} & [expected]]
  {:pre [(class? expired-class)]
   :post [(-> % u/expr-type r/TCResult?)]}
  ;TODO check fields match, handle extra fields in records
  #_(prn "Checking deftype definition:" nme)
  (binding [vs/*current-env* env]
    (let [compiled-class 
          (-> expired-class coerce/Class->symbol coerce/symbol->Class)
          _ (assert (class? compiled-class))
          nme (coerce/Class->symbol compiled-class)
          field-syms (map :name fields)
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
                          u/expr-type (r/ret (c/RClass-of Class)))]

      (cond
        (not dtp)
        (err/tc-delayed-error (str "deftype " nme " must have corresponding annotation. "
                                 "See ann-datatype and ann-record")
                            :return ret-expr)

        (not ((some-fn r/DataType? r/Record?) dt))
        (err/tc-delayed-error (str "deftype " nme " cannot be checked against: " (prs/unparse-type dt))
                            :return ret-expr)

        (if (r/Record? dt)
          (c/isa-DataType? compiled-class)
          (c/isa-Record? compiled-class))
        (let [datatype? (c/isa-DataType? compiled-class)]
          #_(prn (c/isa-DataType? compiled-class)
               (c/isa-Record? compiled-class)
               (r/DataType? dt)
               (r/Record? dt))
          (err/tc-delayed-error (str (if datatype? "Datatype " "Record ") nme 
                                   " is annotated as a " (if datatype? "record" "datatype") 
                                   ", should be a " (if datatype? "datatype" "record") ". "
                                   "See ann-datatype and ann-record")
                              :return ret-expr))

        (not= expected-field-syms 
              ; remove implicit __meta and __extmap fields
              (if (c/isa-Record? compiled-class)
                (drop-last 2 field-syms)
                field-syms))
        (err/tc-delayed-error (str "deftype " nme " fields do not match annotation. "
                                 " Expected: " (vec expected-field-syms)
                                 ", Actual: " (vec field-syms))
                            :return ret-expr)

        :else
        (let [check-method? (fn [inst-method]
                              (not (and (r/Record? dt)
                                        (cu/record-implicits (symbol (:name inst-method))))))
              _ (binding [fn-method-u/*check-fn-method1-checkfn* check
                          fn-method-u/*check-fn-method1-rest-type* 
                          (fn [& args] 
                            (err/int-error "deftype method cannot have rest parameter"))]
                  (doseq [{:keys [env] :as inst-method} methods
                          :when (check-method? inst-method)]
                    (assert (#{:method} (:op inst-method)))
                    (when vs/*trace-checker*
                      (println "Checking deftype* method: " (:name inst-method))
                      (flush))
                    (binding [vs/*current-env* env]
                      (let [method-nme (:name inst-method)
                            _ (assert (symbol? method-nme))
                            ;_ (prn "method-nme" method-nme)
                            ;_ (prn "inst-method" inst-method)
                            _ (assert (:this inst-method))
                            _ (assert (:params inst-method))
                            ; minus the target arg
                            method-sig (first (filter 
                                                (fn [{:keys [name required-params]}]
                                                  (and (= (count (:parameter-types inst-method))
                                                          (count required-params))
                                                       (#{(munge method-nme)} name)))
                                                (:methods inst-method)))]
                        (if-not method-sig
                            (err/tc-delayed-error (str "Internal error checking deftype " nme " method: " method-nme))
                          (let [expected-ifn (cu/datatype-method-expected dt method-sig)]
                            ;(prn "method expected type" expected-ifn)
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
                                (fn-methods/check-fn-methods
                                  [inst-method]
                                  expected-ifn
                                  :recur-target-fn
                                  (fn [{:keys [dom] :as f}]
                                    {:pre [(r/Function? f)]
                                     :post [(recur-u/RecurTarget? %)]}
                                    (recur-u/->RecurTarget (rest dom) nil nil nil))
                                  :validate-expected-fn
                                  (fn [fin]
                                    {:pre [(r/FnIntersection? fin)]}
                                    (when (some #{:rest :drest :kws} (:types fin))
                                      (err/int-error
                                        (str "Cannot provide rest arguments to deftype method: "
                                             (prs/unparse-type fin))))))))))))))]
          ret-expr)))))

(add-check-method :import
  [expr & [expected]]
  (assoc expr
         u/expr-type (r/ret r/-nil)))

(add-check-method :case-test
  [{:keys [test] :as expr} & [expected]]
  (let [ctest (check test expected)]
    (assoc expr
           :test ctest
           u/expr-type (u/expr-type ctest))))

(add-check-method :case
  [{target :test :keys [tests thens default] :as expr} & [expected]]
  {:post [((every-pred vector?
                       (con/every-c? (every-pred
                                       (comp #{:case-test} :op)
                                       :test)))
           (:tests %))
          ((every-pred vector?
                       (con/every-c? (every-pred
                                       (comp #{:case-then} :op)
                                       :then)))
           (:thens %))
          (-> % u/expr-type r/TCResult?)]}
  ; tests have no duplicates
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [ctarget (check target)
          target-ret (u/expr-type ctarget)
          _ (assert (r/TCResult? target-ret))
          ctests (mapv check tests)
          tests-rets (map u/expr-type ctests)
          ; Can we derive extra information from 'failed'
          ; tests? Delegate to check-case-thens for future enhancements.
          cthens (case/check-case-thens check target-ret tests-rets thens expected)
          cdefault (let [flag+ (atom true :validator con/boolean?)
                         neg-tst-fl (let [val-ts (map (comp c/fully-resolve-type r/ret-t) tests-rets)]
                                      (if (every? r/Value? val-ts)
                                        (fo/-not-filter-at (apply c/Un val-ts)
                                                           (r/ret-o target-ret))
                                        fl/-top))
                         env-default (update/env+ lex/*lexical-env* [neg-tst-fl] flag+)
                         _ (when-not @flag+
                             ;; FIXME should we ignore this branch?
                             (u/tc-warning "Local became bottom when checking case default"))]
                     ;(prn "neg-tst-fl" neg-tst-fl)
                     ;(prn "env-default" env-default)
                     (var-env/with-lexical-env env-default
                       (check default expected)))
          case-result (let [type (apply c/Un (map (comp :t u/expr-type) (cons cdefault cthens)))
                            ; TODO
                            filter (fo/-FS fl/-top fl/-top)
                            ; TODO
                            object obj/-empty]
                        (r/ret type filter object))]
      (assoc expr
             :test ctarget
             :tests ctests
             :thens cthens
             :default cdefault
             u/expr-type case-result))))

(add-check-method :catch
  [{handler :body :keys [local] :as expr} & [expected]]
  (catch/check-catch check expr expected))

(add-check-method :try
  [{:keys [body catches finally] :as expr} & [expected]]
  {:post [(vector? (:catches %))
          (-> % u/expr-type r/TCResult?)]}
  (try/check-try check expr expected))

(add-check-method :set!
  [{:keys [target val env] :as expr} & [expected]]
  (set!/check-set! check expr expected))
