(ns clojure.core.typed.check-cljs
  (:require [clojure.core.typed]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.check.let :as let]
            [clojure.core.typed.check.loop :as loop]
            [clojure.core.typed.check.letfn :as letfn]
            [clojure.core.typed.check.recur :as recur]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.if :as if]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.check.fn :as fn]
            [clojure.core.typed.check.fn-method-utils :as fn-method-u]
            [clojure.core.typed.check.set-bang :as set!]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.type-rep :as r :refer [ret ret-t ret-o]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.utils :as u :refer [def-type expr-type]]
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
            [clojure.core.typed.analyze-cljs :as ana])
  (:import (clojure.core.typed.type_rep Value DottedPretype)))

(declare check)

(defn check-ns [nsym]
  (let [asts (ana/ast-for-ns nsym)]
    (doseq [ast asts]
      (check ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check CLJS AST

(defmulti check (fn [expr & [expected]] 
                  (:op expr)))

(defmethod check :no-op
  [expr & [expected]]
  (assoc expr
         expr-type (ret r/-any)))

(defmethod check :constant
  [{:keys [form env] :as expr} & [expected]]
  (let [t (r/-val form)
        _ (binding [vs/*current-env* env]
            (when expected
              (when-not (sub/subtype? t (ret-t expected))
                (cu/expected-error t (ret-t expected)))))]
    (assoc expr
           expr-type (ret t))))

(defmethod check :list
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (r/HeterogeneousList-maker (mapv (comp ret-t expr-type) citems))
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (cu/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :vector
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (r/-hvec (mapv (comp ret-t expr-type) citems))
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (cu/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :set
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (c/Protocol-of 'cljs.core/ISet [(apply c/Un (map (comp ret-t expr-type) citems))])
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (cu/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :map
  [{mkeys :keys mvals :vals :as expr} & [expected]]
  (let [ckeys (mapv check mkeys)
        cvals (mapv check mvals)
        keyts (map (comp ret-t expr-type) ckeys)
        valts (map (comp ret-t expr-type) cvals)
        ; use heterogeneous version if all keys are keywords
        actual (if (every? #(when (r/Value? %)
                              (keyword? (:val %)))
                           keyts)
                 (c/-complete-hmap (zipmap keyts valts))
                 (c/Protocol-of 'cljs.core/IMap
                                [(apply c/Un keyts)
                                 (apply c/Un valts)]))
        _ (binding [vs/*current-env* (:env expr)]
            (when expected
              (when-not (sub/subtype? actual (ret-t expected))
                (cu/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :def
  [{:keys [init env] vname :name :as expr} & [expected]]
  (assert (not expected))
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (let [ann-type (var-env/lookup-Var-nofail vname)
          check? (var-env/check-var? vname)
          res-expr (assoc expr
                          ;FIXME should really be Var, change when Var is annotated
                          expr-type (ret r/-any))]
      (cond
        (or (not check?)
            (not init)) res-expr
        ann-type
        (let [ cinit (check init (ret ann-type))
              _ (when-not (sub/subtype? (-> cinit expr-type ret-t)
                                        ann-type)
                  (err/tc-delayed-error (str "Var definition did not match annotation."
                                           " Expected: " (prs/unparse-type ann-type)
                                           ", Actual: " (prs/unparse-type (-> cinit expr-type ret-t)))))]
          res-expr)
        :else (err/tc-delayed-error (str "Found untyped var definition: " vname
                                       "\nHint: Add the annotation for " vname
                                       " via check-ns or cf")
                                  :return res-expr)))))

(defmethod check :js
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

; copied from Clojure impl
(defn- print-env*
  ([] (print-env* lex/*lexical-env*))
  ([e]
   {:pre [(lex/PropEnv? e)]}
   ;; DO NOT REMOVE
   (prn {:env (into {} (for [[k v] (:l e)]
                         [k (prs/unparse-type v)]))
         :props (map prs/unparse-filter (:props e))})))

(defmethod invoke-special 'cljs.core.typed/print-env
  [{[{debug-string :form :as texpr} :as args] :args :as expr} & [expected]]
  (assert (= 1 (count args)))
  (assert (string? debug-string))
  ;DO NOT REMOVE
  (print-env*)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (ret r/-any)))

;don't type check
(defmethod invoke-special 'cljs.core.typed/tc-ignore-forms*
  [{:keys [] :as expr} & [expected]]
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

(defmethod invoke-special 'cljs.core.typed/ann-form*
  [{[the-expr {typ-syn :form :as texpr} :as args] :args :as expr} & [expected]]
  (assert (= (count args) 2))
  (binding [vs/*current-expr* expr
            vs/*current-env* (:env expr)]
    (let [current-ns (cu/expr-ns expr)
          given-type (prs/with-parse-ns current-ns
                       (prs/parse-type typ-syn))
          cform (check the-expr (ret given-type))
          _ (when-not (sub/subtype? (-> cform expr-type ret-t) given-type)
              (err/tc-delayed-error 
                (prs/with-unparse-ns current-ns
                  (str "Annotation does not match actual type:\n"
                       "Expected: " (prs/unparse-type given-type)"\n"
                       "Actual: " (prs/unparse-type (-> cform expr-type ret-t))))))
          _ (when expected
              (assert (sub/subtype? given-type (ret-t expected))))]
      (assoc expr
             expr-type (ret given-type)))))

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

(defmethod check :invoke
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

(defmethod check :var
  [{{vname :name} :info :keys [env] :as expr} & [expected]]
  (assoc expr
         expr-type (ret (binding [vs/*current-env* env
                                  vs/*current-expr* expr]
                          (let [t (var-env/type-of-nofail vname)]
                            (if t
                              t
                              (err/tc-delayed-error (str "Found untyped var: " vname)
                                                  :return (or (when expected
                                                                (ret-t expected))
                                                              (r/TCError-maker))))))
                        ;only local bindings are immutable, vars/js do not partipate in occurrence typing
                        (if-not (namespace vname)
                          (fl/-FS (fl/-not-filter (c/Un r/-nil r/-false) vname)
                                  (fl/-filter (c/Un r/-nil r/-false) vname))
                          (fl/-FS f/-top f/-top))
                        (if-not (namespace vname)
                          (o/->Path nil vname)
                          o/-empty))))

(defmethod check :do
  [{:keys [ret statements] :as expr} & [expected]]
  (let [cstatements (mapv check statements)
        cret (check ret expected)]
    (assoc expr
           expr-type (expr-type cret))))
           

(defmethod check :fn
  [{:keys [methods] :as expr} & [expected]]
  (let [found-meta? (atom nil)
        parse-meta (fn [{:keys [ann] :as m}] 
                     (or (when (contains? m :ann)
                           (assert ((some-fn list? seq?) ann) 
                                   (str "Annotations must be quoted: " m))
                           (reset! found-meta? true)
                           (prs/with-parse-ns (cu/expr-ns expr)
                             (prs/parse-type ann)))
                         r/-any))
        manual-annot (doall
                       (for [{:keys [variadic params]} methods]
                         (let [fixed (if variadic
                                       (butlast params)
                                       params)
                               rest (when variadic
                                      (last params))]
                           (r/make-Function (mapv parse-meta (map meta fixed))
                                            r/-any
                                            (when variadic
                                              (parse-meta rest))))))]

  (binding [fn-method-u/*check-fn-method1-checkfn* check
            ;this is identical to the Clojure implementation
            fn-method-u/*check-fn-method1-rest-type* 
            (fn [rest drest kws]
              {:pre [(or (r/Type? rest)
                         (r/DottedPretype? drest)
                         (r/KwArgs? kws))
                     (#{1} (count (filter identity [rest drest kws])))]
               :post [(r/Type? %)]}
              ;(prn "rest" rest)
              ;(prn "drest" drest)
              ;(prn "kws" kws)
              (cond
                (or rest drest)
                (c/Un r/-nil 
                      (r/TApp-maker (r/Name-maker 'cljs.core.typed/NonEmptySeq)
                                    [(or rest (.pre-type ^DottedPretype drest))]))
                :else (c/KwArgs->Type kws)))]
    (fn/check-fn 
      expr
      (or (when @found-meta?
            manual-annot)
          expected
          (ret (r/make-FnIntersection
                 (r/make-Function [] r/-any r/-any))))))))

(defmethod check :deftype*
  [expr & [expected]]
  (assert (not expected))
  (assoc expr
         expr-type (ret r/-any)))

(defmethod check :set!
  [{:keys [target val] :as expr} & [expected]]
  (set!/check-set! check expr expected))

(defn check-dot [{:keys [target field method args] :as dot-expr} expected]
  (let [ctarget (check target)
        target-t (-> ctarget expr-type ret-t)
        resolved (let [t (c/fully-resolve-type target-t)]
                   ;TODO DataType
                   (when ((some-fn r/JSNominal? 
                                   r/StringCLJS?
                                   #_r/DataType?) t)
                     t))]
    (if resolved
      (cond
        field
        (let [field-type (cond
                           (r/StringCLJS? resolved)
                           (jsnom/get-field 'string nil field)
                           (r/JSNominal? resolved)
                           (jsnom/get-field (:name resolved) (:poly? resolved) field))
              _ (assert field-type (str "Don't know how to get field " field
                                        " from " (prs/unparse-type resolved)))]
          (assoc dot-expr
                 expr-type (ret field-type)))
        :else
        (let [method-type (cond
                            (r/StringCLJS? resolved)
                            (jsnom/get-method 'string nil method)
                            (r/JSNominal? resolved)
                            (jsnom/get-method (:name resolved) (:poly? resolved) method))
              _ (assert method-type (str "Don't know how to call method " method
                                         " from " (prs/unparse-type resolved)))
              cargs (mapv check args)
              actual (funapp/check-funapp nil cargs (ret method-type) (map expr-type cargs)
                                       expected)]
          (assoc dot-expr
                 expr-type actual)))
      (err/tc-delayed-error (str "Don't know how to use type " (prs/unparse-type target-t)
                               " with "
                               (if field (str "field " field)
                                 (str "method " method)))
                          :return 
                          (assoc dot-expr
                                 expr-type (ret (or (when expected
                                                      (ret-t expected))
                                                    (r/TCError-maker))))))))

(defmethod check :dot
  [expr & [expected]]
  (check-dot expr expected))

(defmethod check :new
  [{:keys [ctor args] :as expr} & [expected]]
  (assert nil ctor))

(defmethod check :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % expr-type r/TCResult?)]}
  (let [ctest (check test)]
    (if/check-if check expr ctest then else expected)))

(defmethod check :let
  [{:keys [bindings #_expr env] :as let-expr} & [expected]]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let/check-let check let-expr expected))

(defmethod check :letfn
  [{:keys [bindings expr env] :as letfn-expr} & [expected]]
  (letfn/check-letfn bindings expr letfn-expr expected check))

(defmethod check :recur
  [{:keys [exprs env] :as recur-expr} & [expected]]
  (recur/check-recur exprs env recur-expr expected check))

(defmethod check :loop
  [{:keys [bindings #_expr env] :as loop-expr} & [expected]]
  (loop/check-loop check loop-expr expected))

(defmethod check :ns
  [expr & [expected]]
  (assoc expr
         expr-type (ret r/-any)))
