(ns clojure.core.typed.check-cljs
  (:require [clojure.core.typed]
            [clojure.core.typed.check :as chk :refer [expr-type]]
            [clojure.core.typed.type-rep :as r :refer [ret ret-t]]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.utils :as u :refer [def-type]]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.filter-rep :as f]
            [clojure.core.typed.filter-ops :as fl]
            [clojure.core.typed.inst :as inst]
            [clojure.core.typed.object-rep :as o]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.core.typed.js-env :as jsenv]
            [clojure.core.typed.analyze-cljs :as ana])
  (:import (clojure.core.typed.type_rep Value)))

(declare check)

(defn check-ns [nsym]
  (let [asts (ana/ast-for-ns nsym)]
    (doseq [ast asts]
      (check ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check CLJS AST

(defmulti check (fn [expr & [expected]] (:op expr)))

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
                (chk/expected-error t (ret-t expected)))))]
    (assoc expr
           expr-type (ret t))))

(defmethod check :list
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (r/HeterogeneousList-maker (mapv (comp ret-t expr-type) citems))
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (chk/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :vector
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (r/-hvec (mapv (comp ret-t expr-type) citems))
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (chk/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :set
  [{:keys [items] :as expr} & [expected]]
  (let [citems (mapv check items)
        actual (c/Protocol-of 'cljs.core/ISet [(apply c/Un (map (comp ret-t expr-type) citems))])
        _ (binding [vs/*current-env* (:env expr)]
            (when expected 
              (when-not (sub/subtype? actual (ret-t expected))
                (chk/expected-error actual (ret-t expected)))))]
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
                (chk/expected-error actual (ret-t expected)))))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :def
  [{:keys [init env] vname :name :as expr} & [expected]]
  (assert (not expected))
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (let [ann-type (var-env/lookup-Var-nofail vname)
          res-expr (assoc expr
                          ;FIXME should really be Var, change when protocols are implemented
                          expr-type (ret r/-any))]
      (if ann-type
        (let [ cinit (check init (ret ann-type))
              _ (when-not (sub/subtype? (-> cinit expr-type ret-t)
                                        ann-type)
                  (u/tc-delayed-error (str "Var definition did not match annotation."
                                           " Expected: " (prs/unparse-type ann-type)
                                           ", Actual: " (prs/unparse-type (-> cinit expr-type ret-t)))))]
          res-expr)
        (u/tc-delayed-error (str "Found untyped var definition: " vname)
                            :return res-expr)))))

(defmethod check :js
  [{:keys [js-op args env] :as expr} & [expected]]
  (assert js-op "js-op missing")
  (let [res (expr-type (check {:op :invoke
                               :env env
                               :f {:op :var
                                   :env env
                                   :info {:name js-op}}
                               :args args}))]
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

(defmethod invoke-special 'cljs.core.typed/inst-poly
  [{[pexpr targs-expr :as args] :args :as expr} & [expected]]
  (assert (#{2} (count args)) "Wrong arguments to inst")
  (let [ptype (let [t (-> (check pexpr) expr-type ret-t)]
                (if (r/Name? t)
                  (c/resolve-Name t)
                  t))
        _ (assert ((some-fn r/Poly? r/PolyDots?) ptype))
        targs (binding [prs/*parse-type-in-ns* (chk/expr-ns expr)]
                (doall (map prs/parse-type (:form targs-expr))))]
    (assoc expr
           expr-type (ret (inst/manual-inst ptype targs)))))

(defmethod invoke-special 'cljs.core.typed/ann-form*
  [{[the-expr {typ-syn :form :as texpr} :as args] :args :as expr} & [expected]]
  (assert (= (count args) 2))
  (let [current-ns (chk/expr-ns expr)
        given-type (prs/with-parse-ns current-ns
                     (prs/parse-type typ-syn))
        cform (check the-expr (ret given-type))
        _ (assert (sub/subtype? (-> cform expr-type ret-t) given-type)
                  (prs/with-unparse-ns current-ns
                    (str "Annotation does not match actual type:\n"
                         "Expected: " (prs/unparse-type given-type)"\n"
                         "Actual: " (prs/unparse-type (-> cform expr-type ret-t)))))
        _ (when expected
            (assert (sub/subtype? given-type (ret-t expected))))]
    (assoc expr
           expr-type (ret given-type))))

(defmethod invoke-special 'cljs.core.typed/loop>-ann
  [{[expr {expected-bnds-syn :form}] :args :as dummy-expr} & [expected]]
  (let [expected-bnds (binding [prs/*parse-type-in-ns* (chk/expr-ns dummy-expr)]
                        (mapv prs/parse-type expected-bnds-syn))]
    ;loop may be nested, type the first loop found
    (binding [chk/*loop-bnd-anns* expected-bnds]
      (check expr expected))))

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
            actual (chk/check-funapp cfexpr cargs ftype argtys expected)]
        (assoc expr
               expr-type actual)))))

(defmethod check :var
  [{{vname :name} :info :keys [env] :as expr} & [expected]]
  (assoc expr
         expr-type (ret (binding [vs/*current-env* env]
                          (if (= "js" (namespace vname))
                            (jsenv/resolve-js-var (symbol (name vname)))
                            (var-env/type-of vname)))
                        ;only local bindings are immutable, vars/js do not partipate in occurrence typing
                        (if-not ((some-fn nil? #{"js"}) (namespace vname))
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
  [{:keys [] :as expr} & [expected]]
  (binding [chk/*check-fn-method1-checkfn* check]
    (assoc expr
           expr-type (chk/check-fn 
                       expr
                       (or expected
                           (ret (r/make-FnIntersection
                                  (r/make-Function [] r/-any r/-any))))))))

(defmethod check :deftype*
  [expr & [expected]]
  (assert (not expected))
  (assoc expr
         expr-type (ret r/-any)))

(defmethod check :set!
  [{:keys [target val] :as expr} & [expected]]
  (binding [vs/*current-expr* expr]
    (let [ctarget (check target)
          cval (check val)
          target-expected (-> ctarget expr-type ret-t)
          val-type (-> cval expr-type ret-t)
          _ (when-not (sub/subtype? val-type target-expected)
              (chk/expected-error val-type target-expected))
          _ (when-not (and expected (sub/subtype? target-expected (ret-t expected)))
              (chk/expected-error target-expected (ret-t expected)))]
      (assoc expr
             expr-type (ret val-type)))))

(defn check-dot [{:keys [target field method args] :as dot-expr} expected]
  (let [ctarget (check target)
        target-t (-> ctarget expr-type ret-t)
        resolved (let [t (c/fully-resolve-type target-t)]
                   ;TODO DataType
                   (when ((some-fn r/JSNominal? #_r/DataType?) t)
                     t))]
    (if resolved
      (cond
        field
        (let [field-type (cond
                           (r/JSNominal? resolved)
                           (jsnom/get-field (:name resolved) field))
              _ (assert field-type (str "Don't know how to get field " field
                                        " from " (prs/unparse-type resolved)))]
          (assoc dot-expr
                 expr-type (ret field-type)))
        :else
        (let [method-type (cond
                            (r/JSNominal? resolved)
                            (jsnom/get-method (:name resolved) method))
              _ (assert method-type (str "Don't know how to call method " method
                                         " from " (prs/unparse-type resolved)))
              cargs (mapv check args)
              actual (chk/check-funapp nil cargs (ret method-type) (map expr-type cargs)
                                       expected)]
          (assoc dot-expr
                 expr-type actual)))
      (u/tc-delayed-error (str "Don't know how to use type " (prs/unparse-type target-t)
                               " with dot")
                          :return 
                          (assoc dot-expr
                                 expr-type (ret (or (when expected
                                                      (ret-t expected))
                                                    (r/TCError-maker))))))))

(defmethod check :dot
  [expr & [expected]]
  (check-dot expr expected))

(defmethod check :if
  [{:keys [test then else] :as expr} & [expected]]
  (let [ctest (check test)]
    (assoc expr
           expr-type (binding [chk/*check-if-checkfn* check]
                       (chk/check-if (expr-type ctest) then else expected)))))

(defmethod check :let
  [{:keys [bindings expr env] :as let-expr} & [expected]]
  (binding [chk/*check-let-checkfn* check]
    (chk/check-let bindings expr let-expr false expected)))

(defmethod check :letfn
  [{:keys [bindings expr env] :as letfn-expr} & [expected]]
  (chk/check-letfn bindings expr letfn-expr expected check))

(defmethod check :recur
  [{:keys [exprs env] :as recur-expr} & [expected]]
  (chk/check-recur exprs env recur-expr expected check))

(defmethod check :loop
  [{:keys [bindings expr env] :as loop-expr} & [expected]]
  (binding [chk/*check-let-checkfn* check]
    (let [loop-bnd-anns chk/*loop-bnd-anns*]
      (binding [chk/*loop-bnd-anns* nil]
        (chk/check-let bindings expr loop-expr true expected :expected-bnds loop-bnd-anns)))))

(defmethod check :ns
  [expr & [expected]]
  (assoc expr
         expr-type (ret r/-any)))

;; Debug

;(defn ana-cljs [env form]
;  (with-altered-specials
;    (binding [cljs/*cljs-ns* cljs/*cljs-ns*]
;      (cljs/analyze env form))))

;(comment
;  ;; TODO there's a bug in the docstring for cljs.analyzer/analyze: it says :ns is a symbol, when instead it's {:name nsym}
;  (def denv {:locals {} :context :statement :ns {:name 'cljs.user}})
;
;(cljs/analyze denv 1)
;  (cf-cljs 1)
;
;(cljs/analyze denv [1])
;  (cf-cljs [1])
;
;(cljs/analyze denv {:a 1})
;(cf-cljs {:a 1})
;
;(cljs/analyze denv (cljs-ann cljs.user/a Any))
;  (@CLJS-VAR-ENV 'cljs.user/a)
;
;(cljs/analyze denv '(def a 1))
;(cf-cljs (def a 1))
;
;; defprotocol doesn't macroexpand because we've added 'defprotocol as a special
;  (with-altered-specials
;    (prn cljs/specials)
;    (cljs/macroexpand-1 {} '(defprotocol A)))
;(cljs/analyze (cljs/empty-env) '(defprotocol A))
;(cf-cljs (defprotocol A))
;
;
;(ana-cljs denv '(ns cljs.user (:require-macros [clojure.core.typed :as typ])))
;(cljs/macroexpand-1 {} '(ann-form-cljs 'a SymbolCLJS))
;(cf-cljs (clojure.core.typed/ann-form-cljs 'a SymbolCLJS))
;
;  ;occurrence typing
;(cljs-ann cljs.core/symbol? (predicate-cljs SymbolCLJS))
;(ana-cljs denv 'cljs.core/symbol?)
;(cf-cljs (cljs.core/symbol? 'a))
;
;; do                                         
;(ana-cljs denv '(do 1 2 ))
;
;; fn
;(ana-cljs denv '(fn [a] 12 1 ))
;(cf-cljs (fn []))
;(cf-cljs (fn [a]))
;(cf-cljs (fn [a b c]))
;(cf-cljs (fn [a b c]) [BooleanCLJS BooleanCLJS Any -> nil])
;
;(cf-cljs (fn [a] (if a (a) true)) [(U nil [-> BooleanCLJS]) -> BooleanCLJS])
;
;  (ana-cljs {:locals {} :context :expr :ns {:name cljs/*cljs-ns*}}
;            (list `ann-form-cljs 1 'Any))
;
;(ana-cljs denv '(fn [a] a))
;(cf-cljs (fn [a b c] a) [BooleanCLJS BooleanCLJS Any -> BooleanCLJS])
;
;; deftype
;(ana-cljs denv '(deftype A [b] cljs.core/ASeq))
;  (cljs/macroexpand-1 (cljs/empty-env)
;                      '(deftype A [b] 
;                         cljs.core/ASeq 
;                         cljs.core/IFn
;                         (invoke [this a b] a)))
;  (cljs/macroexpand-1 (cljs/empty-env)
;                      '(cljs.core/extend-type A 
;                                              cljs.core/ASeq 
;                                              cljs.core/ISeq 
;                                              (first [this] this)
;                                              #_cljs.core/IFn #_(invoke ([this a b] a))))
;(cljs/analyze (cljs/empty-env) '(deftype A [b] 
;                  cljs.core/ASeq 
;                  cljs.core/ISeq
;                  (invoke [this a b] a)))
;  (ana-cljs '(deftype A [b] 
;               cljs.core/ASeq 
;               cljs.core/IFn
;               (invoke [this a b] a)))
;(ana-cljs denv '(set! o -a 1))
;(ana-cljs denv '(set! o 1))
;(cf-cljs (set! o -a 1))
;(ana-cljs denv '(.-cljs$lang$type 1))
;(cf-cljs (.-cljs$lang$type 1))
;(ana-cljs denv '(.-cljs$lang$type 1))
;(cf-cljs (set! cljs.core/symbol? 1))
;
;
;  (ana-cljs denv '(if 1 2 3))
;  (cf-cljs (if 1 2 3))
;
;  (ana-cljs denv '(let [a 2] a))
;  (cf-cljs (let [a 2] a))
;
;
;  ;ns
;  (cf-cljs (ns my-ns (:require [cljs.core :as s])))
;
;  (check-ns clojure.core.typed.test.logic)
;
;  (cljs/analyze (cljs/empty-env) '(typed.internal/print-env "start"))
;  (cf-cljs (typed.internal/print-env "start"))
;
;  (cljs-ann foo [(U nil [-> BooleanCLJS]) -> BooleanCLJS])
;  (cf-cljs
;    (defn foo [x]
;      (typed.internal/print-env "top-of-foo")
;      (if x
;        (x)
;        false)))
;
;
;
;  )
