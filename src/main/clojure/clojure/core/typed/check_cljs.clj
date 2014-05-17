(ns clojure.core.typed.check-cljs
  (:require [clojure.core.typed]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.check-below :as below]
            [clojure.core.typed.local-result :as local-result]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.check.let :as let]
            [clojure.core.typed.check.loop :as loop]
            [clojure.core.typed.check.letfn :as letfn]
            [clojure.core.typed.check.recur :as recur]
            [clojure.core.typed.check.def :as def]
            [clojure.core.typed.check.do :as do]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.if :as if]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.check.fn :as fn]
            [clojure.core.typed.check.map :as map]
            [clojure.core.typed.check.dot-cljs :as dot]
            [clojure.core.typed.check.fn-method-utils :as fn-method-u]
            [clojure.core.typed.check.set-bang :as set!]
            [clojure.core.typed.check.set :as set]
            [clojure.core.typed.check.vector :as vec]
            [clojure.core.typed.check.print-env :as pr-env]
            [clojure.core.typed.check.special.fn :as special-fn]
            [clojure.core.typed.check.special.ann-form :as ann-form]
            [clojure.core.typed.check.special.tc-ignore :as tc-ignore]
            [clojure.core.typed.check.special.loop :as special-loop]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.type-rep :as r :refer [ret ret-t ret-o]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.utils :as u :refer [expr-type]]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.filter-rep :as f]
            [clojure.core.typed.filter-ops :as fl]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.object-rep :as o]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.object-rep :a obj]
            [clojure.core.typed.analyze-cljs :as ana]))

(alias 't 'clojure.core.typed)

(declare check)

(defn check-asts [asts]
  (doall
    (for [ast asts]
      (check ast))))

(defn check-ns [nsym]
  {:pre [(symbol? nsym)]
   :post [(nil? %)]}
  (cu/check-ns-and-deps*
    nsym
    {:ast-for-ns ana/ast-for-ns
     :check-asts check-asts
     :check-ns check-ns}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check CLJS AST

(defmulti check (fn [expr & [expected]] 
                  (:op expr)))

(u/add-defmethod-generator check)

(add-check-method :no-op
  [expr & [expected]]
  (assoc expr
         expr-type (ret r/-any)))

;FIXME call constant-type
(add-check-method :constant
  [{:keys [form env] :as expr} & [expected]]
  (let [t (r/-val form)
        _ (binding [vs/*current-env* env]
            (when expected
              (when-not (sub/subtype? t (ret-t expected))
                (cu/expected-error t (ret-t expected)))))]
    (assoc expr
           expr-type (ret t))))

(add-check-method :list
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (r/HeterogeneousList-maker (mapv (comp ret-t expr-type) citems))
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (cu/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(add-check-method :vector
  [{:keys [items] :as expr} & [expected]]
  (vec/check-vector check expr expected))

(add-check-method :set
  [{:keys [items] :as expr} & [expected]]
  (set/check-set check expr expected))

(add-check-method :map
  [{mkeys :keys mvals :vals :as expr} & [expected]]
  (map/check-map check expr expected))

(add-check-method :def
  [{:keys [init env] vname :name :as expr} & [expected]]
  (if init
    (def/check-normal-def check expr expected)
    (assoc expr
           u/expr-type (ret r/-any))))

(add-check-method :js
  [{:keys [js-op args env] :as expr} & [expected]]
  (assert js-op "js-op missing")
  (let [res (expr-type (check {:op :invoke
                               :from-js-op expr
                               :env env
                               :f {:op :var
                                   :env env
                                   :info {:name js-op}}
                               :args args}
                              expected))]
    (assoc expr
           expr-type res)))

(defmulti invoke-special (fn [{{:keys [op] :as fexpr} :f :as expr} & expected]
                                (when (= :var op)
                                  (-> fexpr :info :name))))

(defmethod invoke-special :default [& _] ::not-special)

(defmethod invoke-special 'cljs.core.typed/print-env
  [{[{debug-string :form :as texpr} :as args] :args :as expr} & [expected]]
  (assert (= 1 (count args)))
  (assert (string? debug-string))
  ;DO NOT REMOVE
  (pr-env/print-env*)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (ret r/-any)))

(defmethod invoke-special 'cljs.core.typed/inst-poly
  [{[pexpr targs-expr :as args] :args :as expr} & [expected]]
  (assert (#{2} (count args)) "Wrong arguments to inst")
  (let [ptype (let [t (-> (check pexpr) expr-type ret-t)]
                (if (r/Name? t)
                  (c/resolve-Name t)
                  t))
        _ (assert ((some-fn r/Poly? r/PolyDots?) ptype))
        targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
                (doall (map prs/parse-type (:form targs-expr))))]
    (assoc expr
           expr-type (ret (inst/manual-inst ptype targs)))))

(defmethod invoke-special 'cljs.core.typed/loop>-ann
  [{[expr {expected-bnds-syn :form}] :args :as dummy-expr} & [expected]]
  (let [expected-bnds (binding [prs/*parse-type-in-ns* (cu/expr-ns dummy-expr)]
                        (mapv prs/parse-type expected-bnds-syn))]
    ;loop may be nested, type the first loop found
    (binding [recur-u/*loop-bnd-anns* expected-bnds]
      (check expr expected))))

; args are backwards if from inlining
(defmethod invoke-special 'cljs.core/instance?
  [{:keys [args] :as expr} & [expected]]
  (assert (= 2 (count args)) "Wrong arguments to instance?")
  ; are arguments the correct way round?
  (assert (:from-js-op expr) "instance? without inlining NYI")
  (binding [vs/*current-env* (:env expr)
            vs/*current-expr* expr]
    (let [target-expr (first args)
          inst-of-expr (second args)
          varsym (when (#{:var} (:op inst-of-expr))
                   (-> inst-of-expr :info :name))
          _ (when-not varsym
              (err/int-error (str "First argument to instance? must be a datatype var "
                                (:op inst-of-expr))))
          inst-of (c/DataType-with-unknown-params varsym)
          cexpr (check target-expr)
          expr-tr (expr-type cexpr)
          final-ret (ret (r/BooleanCLJS-maker)
                         (fo/-FS (fo/-filter-at inst-of (ret-o expr-tr))
                                 (fo/-not-filter-at inst-of (ret-o expr-tr))))]
      (assoc expr
             expr-type final-ret))))

(add-check-method :invoke
  [{fexpr :f :keys [args] :as expr} & [expected]]
  (let [e (invoke-special expr)]
    (cond
      (not= e ::not-special) e
      :else
      (let [cfexpr (check fexpr)
            cargs (mapv check args)
            ftype (expr-type cfexpr)
            argtys (map expr-type cargs)
            actual (funapp/check-funapp cfexpr cargs ftype argtys expected)]
        (assoc expr
               expr-type actual)))))

;only local bindings are immutable, vars/js do not partipate in occurrence typing
(defn js-var-result [expr vname expected]
  {:pre [((every-pred symbol? namespace) vname)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (let [t (var-env/type-of vname)]
      (below/maybe-check-below
        (ret t)
        expected))))

(add-check-method :var
  [{{vname :name} :info :as expr} & [expected]]
  (assoc expr
         expr-type ((if (namespace vname) js-var-result local-result/local-result)
                    expr vname expected)))

;(ann internal-special-form [Expr (U nil TCResult) -> Expr])
(u/special-do-op spec/special-form internal-special-form)

(defmethod internal-special-form ::t/tc-ignore
  [expr expected]
  (tc-ignore/check-tc-ignore check expr expected))

(defn check-fn-rest [remain-dom & {:keys [rest drest kws prest]}]
  {:pre [(or (r/Type? rest)
             (r/Type? prest)
             (r/DottedPretype? drest)
             (r/KwArgs? kws))
         (#{1} (count (filter identity [rest drest kws prest])))]
   :post [(r/Type? %)]}
  ;(prn "rest" rest)
  ;(prn "drest" drest)
  ;(prn "kws" kws)
  (cond
    (or rest drest)
    (c/Un r/-nil 
          ; only difference to Clojure impl
          (r/TApp-maker (r/Name-maker 'cljs.core.typed/NonEmptySeq)
                        [(or rest (:pre-type drest))]))

    prest (err/nyi-error "NYI handle prest in CLJS")
    :else (c/KwArgs->Type kws)))

(defmacro prepare-check-fn [& body]
  `(binding [fn-method-u/*check-fn-method1-checkfn* check
             fn-method-u/*check-fn-method1-rest-type* check-fn-rest]
     ~@body))

(defmethod internal-special-form ::t/fn
  [{[_ _ {{fn-anns :ann} :val} :as statements] :statements fexpr :ret :as expr} expected]
  (prepare-check-fn
    (special-fn/check-special-fn check expr expected)))

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
  [{:keys [ret statements] :as expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (con/nne-seq? (:statements %))]}
  (do/check-do check internal-special-form expr expected))

(add-check-method :fn
  [{:keys [methods] :as expr} & [expected]]
  (let [;found-meta? (atom nil)
        ;parse-meta (fn [{:keys [ann] :as m}] 
        ;             (or (when (contains? m :ann)
        ;                   (assert ((some-fn list? seq?) ann) 
        ;                           (str "Annotations must be quoted: " m))
        ;                   (reset! found-meta? true)
        ;                   (prs/with-parse-ns (cu/expr-ns expr)
        ;                     (prs/parse-type ann)))
        ;                 r/-any))
        ;manual-annot (doall
        ;               (for [{:keys [variadic params]} methods]
        ;                 (let [fixed (if variadic
        ;                               (butlast params)
        ;                               params)
        ;                       rest (when variadic
        ;                              (last params))]
        ;                   (r/make-Function (mapv parse-meta (map meta fixed))
        ;                                    r/-any
        ;                                    :rest
        ;                                    (when variadic
        ;                                      (parse-meta rest))))))
        ]
    (prepare-check-fn
      (if expected
        (fn/check-fn expr expected)
        (special-fn/check-core-fn-no-expected check expr)))))

(add-check-method :deftype*
  [expr & [expected]]
  (assert (not expected))
  (assoc expr
         expr-type (ret r/-any)))

(add-check-method :set!
  [{:keys [target val] :as expr} & [expected]]
  (set!/check-set! check expr expected))

(add-check-method :dot
  [expr & [expected]]
  (dot/check-dot check expr expected))

(add-check-method :new
  [{:keys [ctor args] :as expr} & [expected]]
  (assert nil ctor))

(add-check-method :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % expr-type r/TCResult?)]}
  (let [ctest (check test)]
    (if/check-if check expr ctest then else expected)))

(add-check-method :let
  [{:keys [bindings #_expr env] :as let-expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let/check-let check let-expr expected))

(add-check-method :letfn
  [{:keys [bindings expr env] :as letfn-expr} & [expected]]
  (letfn/check-letfn bindings expr letfn-expr expected check))

(add-check-method :recur
  [{:keys [exprs env] :as recur-expr} & [expected]]
  (recur/check-recur exprs env recur-expr expected check))

(add-check-method :loop
  [{:keys [bindings #_expr env] :as loop-expr} & [expected]]
  (loop/check-loop check loop-expr expected))

(add-check-method :ns
  [expr & [expected]]
  (assoc expr
         expr-type (ret r/-any)))
