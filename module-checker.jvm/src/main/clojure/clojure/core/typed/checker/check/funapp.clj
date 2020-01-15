;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.check.funapp
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.check.invoke-kw :as invoke-kw]
            [clojure.core.typed.checker.check.funapp-one :as funapp1]
            [clojure.core.typed.checker.check.app-error :as app-err]
            [clojure.core.typed.checker.cs-gen :as cgen]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.indirect-utils :as ind-u]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.core.typed.checker.filter-ops :as fops]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.checker.utils :as u]
            [clojure.set :as set]
            [clojure.core.typed :as t]
            [clojure.core.typed.checker.subst :as subst]
            [clojure.core.typed.checker.frees :as frees]
            [clojure.core.typed.checker.experimental.infer-vars :as infer-vars]
            [clojure.core.typed.checker.hset-utils :as hset]))

(def ^:private nth-type (delay (impl/dynaload 'clojure.core.typed.checker.check.nth/nth-type)))

; Expr Expr^n TCResult TCResult^n (U nil TCResult) -> TCResult
;TODO HeterogeneousMap case
(defn check-funapp [fexpr args fexpr-ret-type arg-ret-types expected & {:keys [check-fn]}]
  {:pre [(r/TCResult? fexpr-ret-type)
         (every? r/TCResult? arg-ret-types)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (let [fexpr-type (c/fully-resolve-type (r/ret-t fexpr-ret-type))
        arg-types (mapv r/ret-t arg-ret-types)]
    (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                             (when fexpr
                               (cu/expr-ns fexpr)))
    ;(prn "check-funapp" (prs/unparse-type fexpr-type) (map prs/unparse-type arg-types))
    (cond
      ;; a union of functions can be applied if we can apply all of the elements
      (r/Union? fexpr-type)
      (r/ret (reduce (fn [t ftype]
                     {:pre [(r/Type? t)
                            (r/Type? ftype)]
                      :post [(r/Type? %)]}
                     (c/Un t (r/ret-t (check-funapp fexpr args (r/ret ftype) arg-ret-types expected))))
                   (c/Un)
                   (:types fexpr-type)))

      ; try the first thing that looks like a Fn.
      ; FIXME This should probably try and invoke every Fn it can
      ; find, need to figure out how to clean up properly
      ; after a failed invocation.
      (r/Intersection? fexpr-type)
      (let [a-fntype (first (filter
                              (fn [t]
                                ;; would be nice to recur on check-funapp on each of these
                                ;; instead of this hack to find a function type.
                                (or (r/FnIntersection? t)
                                    (r/HeterogeneousVector? t)
                                    (c/keyword-value? t)
                                    (c/ifn-ancestor t)
                                    (r/Poly? t)))
                              (map c/fully-resolve-type (:types fexpr-type))))]
        (if a-fntype
          (check-funapp fexpr args (r/ret a-fntype) arg-ret-types expected)
          (err/int-error (str "Cannot invoke type: " (prs/unparse-type fexpr-type)))))

      (c/ifn-ancestor fexpr-type)
      (check-funapp fexpr args (r/ret (c/ifn-ancestor fexpr-type)) arg-ret-types expected)

      ;keyword function
      (c/keyword-value? fexpr-type)
      (let [[target-ret default-ret & more-args] arg-ret-types]
        (assert (empty? more-args))
        (invoke-kw/invoke-keyword nil fexpr-ret-type target-ret default-ret expected))

      ;set function
      ;FIXME yuck. Also this is wrong, should be APersistentSet or something that *actually* extends IFn
      (and (r/RClass? fexpr-type)
           (isa? (coerce/symbol->Class (:the-class fexpr-type)) 
                 clojure.lang.IPersistentSet))
      (do
        (when-not (#{1} (count args))
          (err/tc-delayed-error (str "Wrong number of arguments to set function (" (count args)")")))
        (below/maybe-check-below
          (r/ret r/-any)
          expected))

      ;FIXME same as IPersistentSet case
      (and (r/RClass? fexpr-type)
           (isa? (coerce/symbol->Class (:the-class fexpr-type)) clojure.lang.IPersistentMap))
      ;rewrite ({..} x) as (f {..} x), where f is some dummy fn
      (let [mapfn (prs/parse-type `(t/All [x#] [(t/Map t/Any x#) t/Any :-> (t/U nil x#)]))]
        (check-funapp fexpr args (r/ret mapfn) (concat [fexpr-ret-type] arg-ret-types) expected))

      ;FIXME same as IPersistentSet case
      (and (r/RClass? fexpr-type)
           (isa? (coerce/symbol->Class (:the-class fexpr-type)) clojure.lang.IPersistentVector))
      ;rewrite ({..} x) as (f {..} x), where f is some dummy fn
      (let [mapfn (prs/parse-type `(t/All [x# y#]
                                          (t/IFn [(t/Vec x#) t/Int :-> x#]
                                                 [(t/Vec x#) t/Int y# :-> (t/U y# x#)])))]
        (check-funapp fexpr args (r/ret mapfn) (concat [fexpr-ret-type] arg-ret-types) expected))

      ;Symbol function
      (and (r/RClass? fexpr-type)
           ('#{clojure.lang.Symbol} (:the-class fexpr-type)))
      (let [symfn (prs/parse-type `(t/All [x#] [(t/U (t/Map t/Any x#) t/Any) :-> (t/U x# nil)]))]
        (check-funapp fexpr args (r/ret symfn) arg-ret-types expected))
      
      ;Var function
      (and (r/RClass? fexpr-type)
           ('#{clojure.lang.Var} (:the-class fexpr-type)))
      (let [{[_ ftype :as poly?] :poly?} fexpr-type
            _ (assert (#{2} (count poly?))
                      "Assuming clojure.lang.Var only takes 1 argument")]
        (check-funapp fexpr args (r/ret ftype) arg-ret-types expected))

      ;Error is perfectly good fn type
      (r/TCError? fexpr-type)
      (below/maybe-check-below
        (r/ret r/Err)
        expected)

      ; Unchecked function is upcast to anything so we don't check arguments,
      ; but return Unchecked or expected.
      (r/Unchecked? fexpr-type)
      (let [{:keys [vsym]} fexpr-type]
        (when vsym
          (infer-vars/add-inferred-type
            (or prs/*unparse-type-in-ns*
                (when fexpr
                  (cu/expr-ns fexpr)))
            vsym
            (r/make-FnIntersection
              (r/make-Function
                (repeat (count args) r/-any)
                r/-any))))
        (or expected
            (r/ret (r/-unchecked nil))))

      (and (r/HeterogeneousVector? fexpr-type)
           (#{1 2} (count arg-types))
           (let [i (first arg-types)]
             (and (r/Value? i)
                  (integer? (:val i)))))
      (below/maybe-check-below
        ;; FIXME replace with path-type?
        (r/ret (@nth-type [fexpr-type] (:val (first arg-types)) (second arg-types)))
        expected)

      (r/HSet? fexpr-type)
      (let [fixed (:fixed fexpr-type)
            ret (cond
                  (not (#{1} (count arg-ret-types))) 
                  (do (err/tc-delayed-error (str "Wrong number of arguments to set (" (count args) ")"))
                      (r/ret r/Err))

                  :else
                  (let [[argt] arg-ret-types
                        ; default value is nil
                        set-return (apply c/Un r/-nil fixed)]
                    (if (and (:complete? fexpr-type)
                             (every? (every-pred
                                       r/Value?
                                       (comp hset/valid-fixed? :val))
                                     fixed))
                      (let [; (#{false nil} a) returns false even if a is nil/false
                            filter-type (apply c/Un
                                               (disj (r/sorted-type-set fixed) 
                                                     (r/-val nil)
                                                     (r/-val false)))]
                        (r/ret set-return
                               (fops/-FS
                                 (fops/-filter-at filter-type (r/ret-o argt))
                                 (fops/-not-filter-at filter-type (r/ret-o argt)))))
                      (r/ret set-return))))]
        (below/maybe-check-below
          ret
          expected))

  ; FIXME error messages are worse here because we don't use line numbers for
  ; specific arguments
      ;ordinary Function, single case, special cased for improved error msgs
;      (and (r/FnIntersection? fexpr-type)
;           (let [[{:keys [drest] :as ft} :as ts] (:types fexpr-type)]
;             (and (= 1 (count ts))
;                  (not drest))))
;      ; check/funapp-single-arity-nopoly-nodots
;      (let [argtys arg-ret-types
;            {[t] :types} fexpr-type]
;        (funapp1/check-funapp1 fexpr args t argtys expected)))

      ;ordinary Function, multiple cases
      (r/FnIntersection? fexpr-type)
      (let [ftypes (:types fexpr-type)
            matching-fns (filter (fn [{:keys [dom rest kws prest] :as f}]
                                   {:pre [(r/Function? f)]}
                                   (if prest
                                     (sub/subtypes-prest? arg-types dom prest)
                                     (sub/subtypes-varargs? arg-types dom rest kws)))
                                 ftypes)
            success-ret-type (when-let [f (first matching-fns)]
                               (funapp1/check-funapp1 fexpr args f arg-ret-types expected :check? false))]
        (if success-ret-type
          success-ret-type
          (app-err/plainapp-type-error fexpr args fexpr-type arg-ret-types expected)))

      (and (r/SymbolicClosure? fexpr-type) check-fn)
      (binding [vs/*lexical-env* (:env fexpr-type)]
        (-> (:fexpr fexpr-type)
            (check-fn (r/ret (r/make-FnIntersection
                               (r/make-Function arg-types
                                                r/-infer-any))))
            u/expr-type))

      ;ordinary polymorphic function without dotted rest
      (when (r/Poly? fexpr-type)
        (let [names (c/Poly-fresh-symbols* fexpr-type)
              body (c/Poly-body* names fexpr-type)]
          (when (r/FnIntersection? body)
            (every? (complement :drest) (:types body)))))
      (let [fs-names (c/Poly-fresh-symbols* fexpr-type)
            _ (assert (every? symbol? fs-names))
            fin (c/Poly-body* fs-names fexpr-type)
            bbnds (c/Poly-bbnds* fs-names fexpr-type)
            _ (assert (r/FnIntersection? fin))
            ;; Only infer free variables in the return type
            ret-type
            (free-ops/with-bounded-frees (zipmap (map r/F-maker fs-names) bbnds)
                     (loop [[{:keys [dom rng rest drest kws prest] :as ftype} & ftypes] (:types fin)]
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
                                (and (not (or drest kws prest))
                                     ((if rest <= =) (count dom) (count arg-types)))
                                (cgen/infer-vararg (zipmap fs-names bbnds) {}
                                                   arg-types dom rest (r/Result-type* rng)
                                                   (and expected (r/ret-t expected)))

                                (and prest
                                     (<= (count dom) (count arg-types)))
                                (cgen/infer-prest (zipmap fs-names bbnds) {}
                                                  arg-types dom prest (r/Result-type* rng)
                                                  (and expected (r/ret-t expected)))

                                ;keyword parameters
                                kws
                                (let [{:keys [mandatory optional]} kws
                                      [normal-argtys flat-kw-argtys] (split-at (count dom) arg-types)
                                      _ (when-not (even? (count flat-kw-argtys))
                                          ; move to next arity
                                          (cgen/fail! nil nil)
                                          #_(err/int-error (str "Uneven number of keyword arguments "
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
                                                       (r/Type? kw-key-t)
                                                       (r/Type? kw-val-t)]
                                                 :post [((con/hvector-c? (every-pred vector? (con/every-c? r/Type?)) 
                                                                       (every-pred vector? (con/every-c? r/Type?)))
                                                         %)]}
                                                (when-not (r/Value? kw-key-t)
                                                  ; move to next arity
                                                  (cgen/fail! nil nil)
                                                  #_(err/int-error 
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
                                                      (err/tc-delayed-error (str "Undeclared keyword parameter " 
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
                                    ;(err/tc-delayed-error (str "Missing mandatory keyword keys: "
                                    ;                         (pr-str (vec (interpose ", "
                                    ;                                                 (map prs/unparse-type missing-ks))))))
                                  ;; it's probably a bug to not infer for unused optional args, revisit this
                                  ;(when-let [missing-optional-ks (seq
                                  ;                                 (set/difference (set (keys optional))
                                  ;                                                 (set (keys paired-kw-argtys))))]
                                  ;  (err/nyi-error (str "NYI POSSIBLE BUG?! Unused optional parameters"
                                  ;                    (pr-str (interpose ", " (map prs/unparse-type missing-optional-ks)))))
                                  ;  )
                                  ; infer keyword and fixed parameters all at once
                                  (cgen/infer (zipmap fs-names bbnds) {}
                                              (concat normal-argtys kw-val-actual-tys)
                                              (concat dom kw-val-expected-tys) 
                                              (r/Result-type* rng)
                                              (and expected (r/ret-t expected))))))]
                           (let [;_ (prn "subst:" substitution)
                                 new-ftype (subst/subst-all substitution ftype)]
                             ;(prn "substituted type" new-ftype)
                             (funapp1/check-funapp1 fexpr args new-ftype
                                            arg-ret-types expected :check? false))
                           (if drest
                             (do (err/tc-delayed-error (str "Cannot infer arguments to polymorphic functions with dotted rest"))
                                 nil)
                             (recur ftypes))))))]
        (if ret-type
          ret-type
          (app-err/polyapp-type-error fexpr args fexpr-type arg-ret-types expected)))

      :else ;; any kind of dotted polymorphic function without mandatory or optional keyword args
      (if-let [[pbody fixed-map dotted-map]
               (letfn [(should-infer? [t]
                         (and (r/PolyDots? t)
                              (r/FnIntersection?
                                (c/PolyDots-body* (c/PolyDots-fresh-symbols* t)
                                                  t))))
                       (collect-polydots [t]
                         {:post [((con/hvector-c? r/Type?
                                                  (con/hash-c? symbol? r/Bounds?)
                                                  (con/hash-c? symbol? r/Bounds?))
                                  %)]}
                         (loop [pbody (c/fully-resolve-type t)
                                fixed {}
                                dotted {}]
                           (cond 
                             (r/PolyDots? pbody)
                             (let [vars (vec (c/PolyDots-fresh-symbols* pbody))
                                   bbnds (c/PolyDots-bbnds* vars pbody)
                                   fixed* (apply zipmap (map butlast [vars bbnds]))
                                   dotted* (apply hash-map (map last [vars bbnds]))
                                   pbody (c/PolyDots-body* vars pbody)]
                               (recur (c/fully-resolve-type pbody)
                                      (merge fixed fixed*)
                                      (merge dotted dotted*)))

                             (and (r/FnIntersection? pbody)
                                  (seq (:types pbody))
                                  (not (some :kws (:types pbody))))
                             [pbody fixed dotted])))]
                 ; don't support nested PolyDots yet
                 (when (should-infer? fexpr-type)
                   (collect-polydots fexpr-type)))]
        (let [;_ (prn "polydots, no kw args")
              _ (assert (#{1} (count dotted-map)))
              inferred-rng 
              (free-ops/with-bounded-frees (zipmap (map r/make-F (keys fixed-map)) (vals fixed-map))
                ;(dvar-env/with-dotted-mappings (zipmap (keys dotted-map) (map r/make-F (vals dotted-map)))
                 (some identity
                       (for [{:keys [dom rest drest rng prest pdot] :as ftype} (:types pbody)
                             ;only try inference if argument types match
                             :when (cond
                                     rest (<= (count dom) (count arg-types))
                                     drest (and (<= (count dom) (count arg-types))
                                                (contains? (set (keys dotted-map)) (-> drest :name)))
                                     prest (<= (count dom) (count arg-types))
                                     pdot (and (<= (count dom) (count arg-types))
                                               (contains? (set (keys dotted-map)) (-> pdot :name)))
                                     :else (= (count dom) (count arg-types)))]
                         (cgen/handle-failure
                           ;(prn "Inferring dotted fn" (prs/unparse-type ftype))
                           ;; Only try to infer the free vars of the rng (which includes the vars
                           ;; in filters/objects).
                           (let [substitution (cond
                                                drest (cgen/infer-dots fixed-map (key (first dotted-map)) (val (first dotted-map))
                                                                       arg-types dom (:pre-type drest) (r/Result-type* rng) 
                                                                       (frees/fv rng)
                                                                       :expected (and expected (r/ret-t expected)))

                                                rest (cgen/infer-vararg fixed-map dotted-map
                                                                        arg-types dom rest (r/Result-type* rng)
                                                                        (and expected (r/ret-t expected)))

                                                (and prest
                                                     (<= (count dom) (count arg-types)))
                                                (cgen/infer-prest fixed-map dotted-map
                                                                  arg-types dom prest (r/Result-type* rng)
                                                                  (and expected (r/ret-t expected)))

                                                pdot (cgen/infer-pdot fixed-map (key (first dotted-map))
                                                                      (val (first dotted-map))
                                                                      arg-types dom (:pre-type pdot) (r/Result-type* rng)
                                                                      (frees/fv rng)
                                                                      :expected (and expected (r/ret-t expected)))

                                                :else (cgen/infer fixed-map dotted-map
                                                                  arg-types dom (r/Result-type* rng)
                                                                  (and expected (r/ret-t expected))))
                                 ;_ (prn "substitution:" substitution)
                                 substituted-type (subst/subst-all substitution ftype)
                                 ;_ (prn "substituted-type" (prs/unparse-type substituted-type))
                                 ;_ (prn "args" (map prs/unparse-type arg-types))
                                 ]
                             (or (and substitution
                                      (funapp1/check-funapp1 fexpr args 
                                                     substituted-type arg-ret-types expected :check? false))
                                 (err/tc-delayed-error "Error applying dotted type")
                                 nil))))))]
          ;(prn "inferred-rng"inferred-rng)
          (if inferred-rng
            inferred-rng
            (app-err/polyapp-type-error fexpr args fexpr-type arg-ret-types expected)))

        (let [opts (:opts fexpr-ret-type)]
          #_
          (prn ":invoke opts" opts)
          #_
          (prn "infer-any?" fexpr-ret-type (r/infer-any? (r/ret-t fexpr-ret-type)))
          (err/tc-delayed-error (str "Cannot invoke type: " (pr-str (prs/unparse-type fexpr-type)))
                                :return (or expected (r/ret (c/Un))))))))))

(ind-u/add-indirection ind/check-funapp check-funapp)
