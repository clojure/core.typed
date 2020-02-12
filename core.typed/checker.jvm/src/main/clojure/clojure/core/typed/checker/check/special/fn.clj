;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.check.special.fn
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.filter-ops :as fo]
            [clojure.core.typed.checker.object-rep :as or]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.check.fn :as fn]
            [clojure.core.typed.checker.dvar-env :as dvar]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.check.fn-method-one :as fn-method-one]
            [clojure.core.typed.checker.check.fn-methods :as fn-methods]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.analyzer.common :as ana2]))

(declare wrap-poly)

(defn check-anon [{:keys [methods] :as expr} {:keys [doms rngs rests drests]}
                  {:keys [frees-with-bnds dvar]}]
  {:pre [(#{:fn} (:op expr))]}
  (assert (apply = (map count [doms rngs rests drests rngs methods]))
          (mapv count [doms rngs rests drests rngs methods]))
  ;(prn "check-anon")
  ;(prn "doms" doms)
  (let [; only ever at most one rest type. Enforced by the t/fn macro.
        _ (assert (#{0 1} (count (remove nil? (concat rests drests)))))
        ; fixed entries are indexed by their domain count,
        ; :rest entry has variable arity.
        fixed-expecteds (into {}
                              (map (fn [dom rng rest drest]
                                     [(if (or rest drest)
                                        :rest
                                        (count dom))
                                      {:dom dom
                                       :rng rng
                                       :rest rest
                                       :drest drest}])
                                   doms
                                   rngs
                                   rests
                                   drests))
        cmethod-specs
        (mapv
          (fn [method]
            (let [{:keys [dom rng rest drest]
                   :as expecteds}
                  (get fixed-expecteds (if (ast-u/method-rest-param method)
                                         :rest
                                         (count (ast-u/method-required-params method))))
                  _ (assert expecteds)]
              ;(prn "dom" (count dom))
              ;(prn "method args" (-> method ast-u/method-required-params count))
              (fn-method-one/check-fn-method1 
                method 
                (r/make-Function dom (or (when (r/Result? rng)
                                           (r/Result-type* rng))
                                         r/-any) 
                                 :rest rest 
                                 :drest drest
                                 :filter (when (r/Result? rng)
                                           (r/Result-filter* rng))
                                 :object (when (r/Result? rng)
                                           (r/Result-object* rng))
                                 :flow (when (r/Result? rng)
                                         (r/Result-flow* rng)))
                :ignore-rng (not rng))))
          methods)

        [fs cmethods] ((juxt #(map :ftype %)
                             #(mapv :cmethod %))
                       cmethod-specs)
        _ (assert (seq fs) fs)
        _ (assert (every? r/Function? fs) fs)
        ret-type (r/ret (wrap-poly (apply r/make-FnIntersection fs) frees-with-bnds dvar)
                        (fo/-FS fl/-top fl/-bot))]
    (assoc expr
           :methods cmethods
           ::t/cmethods cmethods
           u/expr-type ret-type)))

(defn gen-defaults [{:keys [methods] :as expr}]
  (let [;; :infer-locals are enabled for this namespace, this
        ;; var dereference is the dynamic type
        infer-locals?
        (-> (cu/expr-ns expr)
            find-ns
            meta
            :core.typed
            :experimental
            (contains? :infer-locals))]
    (apply merge-with (comp vec concat)
           (for [method methods]
             (let [fixed-arity (ast-u/fixed-arity method)
                   variadic? (ast-u/variadic-method? method)]
               {:doms [(vec (repeat fixed-arity (if infer-locals? 
                                                  (r/-unchecked nil)
                                                  r/-any)))]
                :rngs [nil]
                :rests [(when variadic?
                          (if infer-locals? 
                            (r/-unchecked nil)
                            r/-any))]
                :drests [nil]})))))

(defn all-defaults? [fn-anns poly]
  (let [defaults (and
                   (every? (fn [{:keys [dom]}]
                             (every? :default dom))
                           fn-anns)
                   (every? (comp :default :rng) fn-anns)
                   (every? (fn [{:keys [rest]}]
                             (or (:default rest)
                                 (nil? rest)))
                           fn-anns)
                   (every? (comp not :drest) fn-anns))]
    (and (not poly)
         defaults)))

(defn prepare-expecteds [expr fn-anns]
  (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
    {:doms
     (->> fn-anns
          (map :dom)
          (mapv (fn [dom]
                  (mapv (fn [{:keys [type default]}]
                          (prs/parse-type type))
                        dom))))
     :rngs (->> fn-anns
                (map :rng)
                (mapv (fn [{:keys [type default]}]
                        (when-not default
                          (r/make-Result (prs/parse-type type)
                                         (fo/-FS fl/-infer-top
                                                 fl/-infer-top)
                                         or/-no-object
                                         (r/-flow fl/-infer-top))))))
     :rests (->> fn-anns
                 (map :rest)
                 (mapv (fn [{:keys [type default] :as has-rest}]
                         (when has-rest
                           (prs/parse-type type)))))
     :drests (->> fn-anns
                  (map :drest)
                  (mapv (fn [{:keys [pretype bound] :as has-drest}]
                          (when has-drest
                            (r/DottedPretype1-maker
                              (prs/parse-type pretype)
                              bound)))))}))

(defn self-type [{:keys [doms rngs rests drests] :as expecteds}]
  (apply r/make-FnIntersection
         (map (fn [dom rng rest drest]
                {:pre [((some-fn nil? r/Result?) rng)
                       ((some-fn nil? r/Type?) rest)
                       ((some-fn nil? r/DottedPretype?) drest)
                       (every? r/Type? dom)]
                 :post [(r/Function? %)]}
                (r/make-Function dom (or (when rng (r/Result-type* rng)) r/-any) 
                                 :rest rest :drest drest))
              doms rngs rests drests)))

(defn parse-poly [bnds]
  {:pre [((some-fn nil? vector?) bnds)]}
  (prs/parse-unknown-binder bnds))

(defn wrap-poly [ifn frees-with-bnds dvar]
  (if (and (empty? frees-with-bnds)
           (not dvar))
    ifn
    (if dvar
      (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                   (map second (concat frees-with-bnds [dvar]))
                   ifn)
      (c/Poly* (map first frees-with-bnds)
               (map second frees-with-bnds)
               ifn))))

(defn check-core-fn-no-expected
  [check fexpr]
  {:pre [(#{:fn} (:op fexpr))]
   :post [(#{:fn} (:op %))
          (r/TCResult? (u/expr-type %))]}
  ;(prn "check-core-fn-no-expected")
  (let [self-name (cu/fn-self-name fexpr)
        _ (assert ((some-fn nil? symbol?) self-name))
        flat-expecteds (gen-defaults fexpr)]
    (lex/with-locals (when self-name
                       (let [this-type (self-type flat-expecteds)
                             ;_ (prn "this-type" this-type)
                             ]
                         {self-name this-type}))
      (check-anon
        fexpr
        flat-expecteds
        nil))))

(defn check-special-fn 
  [check {statements :statements fexpr :ret :as expr} expected]
  {:pre [((some-fn nil? r/TCResult?) expected)
         (#{3} (count statements))]}
  ;(prn "check-special-fn")
  (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
    (let [fexpr (-> fexpr
                    ana2/analyze-outer-root
                    ana2/run-pre-passes)
          statements (update statements 2 ana2/run-passes)
          [_ _ fn-ann-expr :as statements] statements
          _ (assert (#{:fn} (:op fexpr))
                    ((juxt :op :form) fexpr))
          fn-anns-quoted (ast-u/map-expr-at fn-ann-expr :ann)
          ;_ (prn "fn-anns-quoted" fn-anns-quoted)
          poly-quoted    (ast-u/map-expr-at fn-ann-expr :poly)
          ;; always quoted
          fn-anns (second fn-anns-quoted)
          ;; always quoted
          poly (second poly-quoted)
          _ (assert (vector? fn-anns) (pr-str fn-anns))
          self-name (cu/fn-self-name fexpr)
          _ (assert ((some-fn nil? symbol?) self-name)
                    self-name)
          ;_ (prn "self-name" self-name)
          [frees-with-bnds dvar] (parse-poly poly)
          new-bnded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds))
          new-dotted (when dvar [(r/make-F (first dvar))])
          flat-expecteds 
          (free-ops/with-bounded-frees new-bnded-frees
            (dvar/with-dotted new-dotted
              (prepare-expecteds expr fn-anns)))
          ;_ (prn "flat-expecteds" flat-expecteds)
          _ (assert ((some-fn nil? vector?) poly))

          good-expected? (fn [expected]
                           {:pre [((some-fn nil? r/TCResult?) expected)]
                            :post [(boolean? %)]}
                           (boolean
                             (when expected
                               (seq (fn-methods/function-types (r/ret-t expected))))))

          ;; If we have an unannotated fn macro and a good expected type, use the expected
          ;; type via check-fn, otherwise check against the expected type after a call to check-anon.
          cfexpr 
          (if (and (all-defaults? fn-anns poly) 
                   (good-expected? expected))
            (do ;(prn "using check-fn")
                (fn/check-fn fexpr expected))
            (let [;_ (prn "using anon-fn")
                  cfexpr (lex/with-locals (when self-name
                                            (let [this-type (self-type flat-expecteds)
                                                  ;_ (prn "this-type" this-type)
                                                  ]
                                              {self-name this-type}))
                           (free-ops/with-bounded-frees new-bnded-frees
                             (dvar/with-dotted new-dotted
                               (check-anon
                                 fexpr
                                 flat-expecteds
                                 {:frees-with-bnds frees-with-bnds
                                  :dvar dvar}))))]
              (update cfexpr u/expr-type below/maybe-check-below expected)))]
      (assoc expr
             :statements statements
             :ret cfexpr
             u/expr-type (u/expr-type cfexpr)))))
