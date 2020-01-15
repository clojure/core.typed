;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.fn-methods
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.check.fn-method-one :as fn-method1]
            [clojure.core.typed.checker.dvar-env :as dvar-env]))

(def function-type? (some-fn (every-pred r/Poly?
                                         (comp r/FnIntersection? r/Poly-body-unsafe*))
                             (every-pred r/PolyDots?
                                         (comp r/FnIntersection? r/PolyDots-body-unsafe*))
                             r/TCError?
                             r/FnIntersection?))

(def method? (some-fn ast-u/fn-method? ast-u/deftype-method?))
(def methods? 
  (fn [ms]
    (impl/impl-case
      :clojure ((con/vec-c? method?) ms)
      :cljs (every? method? ms))))

(def opt-map? (con/hmap-c? (con/optional :recur-target-fn) ifn?
                           (con/optional :validate-expected-fn) ifn?
                           (con/optional :self-name) (some-fn nil? symbol?)))

(def method-return?
  (con/hmap-c?
    :ifn function-type?
    :methods methods?
    :cmethods methods?))

(defn expected-for-method
  "Takes a :fn-method or :method AST node and a single Function arity type,
  and returns the Function if the :method node should be checked
  against the Function, otherwise returns nil."
  [{:keys [fixed-arity variadic?] :as method}
   {:keys [dom rest drest kws pdot prest] :as f}
   all-methods]
  {:pre [(method? method)
         (r/Function? f)]
   :post [((some-fn nil? r/Function?) %)]}
  ;; fn-method-u/*check-fn-method1-rest-type*, and check-fn-method1
  ;; actually distribute the types amongst the fixed and rest parameters
  (let [ndom (count dom)
        fixed-arity (if (= :method (:op method))
                      (inc fixed-arity)
                      fixed-arity)]
    (cond
      (or rest drest pdot prest)
      (cond
        (not variadic?) nil

        ; extra domains flow into the rest argument
        (<= fixed-arity ndom) f

        ;otherwise method doesn't fit
        :else nil)

      ; kw and drest functions must have exact fixed domain match
      (or kws drest)
      (cond
        (not variadic?) nil
        (== ndom fixed-arity) f
        :else nil)

      ; no variable arity
      (= nil rest drest kws pdot prest)
      (cond
        ;; ensure no other fixed arities would match this Function
        variadic? (when (empty? (filter (fn [m]
                                          (and (not (:variadic? m))
                                               (= ndom (:fixed-arity m))))
                                        all-methods))
                    ; extra domains flow into the rest argument
                    (when (<= fixed-arity ndom)
                      f))
        (== ndom fixed-arity) f
        :else nil))))

(defn check-fni 
  "Check a vector of :method AST nodes mthods against
  an expected type that is a possibly-polymorphic function
  intersection.
  
  Returns a vector in the same order as the passed in methods,
  but each method replaced with a vector of type checked methods."
  [expected mthods
   {:keys [recur-target-fn
           validate-expected-fn
           self-name]
    :as opt}]
  {:pre [(function-type? expected)
         (methods? mthods)
         (opt-map? opt)]
   :post [(method-return? %)]}
  ;(prn "check-fni" expected)
  (let [; unwrap polymorphic expected types
        [fin inst-frees bnds poly?] (cu/unwrap-poly expected)
        ; this should never fail due to function-type? check
        _ (assert (r/FnIntersection? fin))
        _ (when validate-expected-fn
            (validate-expected-fn fin))

        out-fn-matches (atom (vec (repeat (count (:types fin)) nil)))
        ;; cmethodss is a vector in the same order as the passed in methods,
        ;; but each method replaced with a vector of type checked methods."
        cmethodss
        (lex/with-locals (when-let [name self-name] ;self calls
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
              (let [;; ordered pairs from function type to a set of matching methods (integers).
                    fn-matches
                    (into []
                          (map (fn [t]
                                 (let [ms (into #{}
                                                (comp
                                                  (map-indexed (fn [i m]
                                                                 (when (expected-for-method m t mthods)
                                                                   i)))
                                                  (keep identity))
                                                mthods)]
                                   ;; it is a type error if no matching methods are found.
                                   (when (empty? ms)
                                     (binding [vs/*current-expr* (impl/impl-case
                                                                   :clojure (first mthods)
                                                                   ; fn-method is not printable in cljs
                                                                   :cljs vs/*current-expr*)
                                               vs/*current-env* (or (:env (first mthods)) vs/*current-env*)]
                                       (prs/with-unparse-ns (cu/expr-ns (first mthods))
                                         (err/tc-delayed-error (str "No matching arities: " (prs/unparse-type t))))))
                                   [t ms])))
                          (:types fin))]
                ;; if a method occurs more than once in the entire map, it will be
                ;; checked twice, so we disable rewriting for that method.
                (into []
                      (map-indexed
                        (fn [method-index m]
                          (let [expecteds
                                (keep
                                  (fn [[ifn-index [ifn-arity relevant-method-idxs]]]
                                    (when (contains? relevant-method-idxs method-index)
                                      [ifn-index ifn-arity]))
                                  (map-indexed vector fn-matches))
                                cmethods
                                (u/rewrite-when (== 1 (count expecteds))
                                  (mapv (fn [[ifn-index ifn-arity]]
                                          {:pre [(integer? ifn-index)
                                                 (r/Function? ifn-arity)]}
                                          (let [{:keys [cmethod ftype]}
                                                (fn-method1/check-fn-method1 m ifn-arity :recur-target-fn recur-target-fn)
                                                _ (assert (r/Function? ftype))
                                                union-Functions (fn [f1 f2]
                                                                  {:pre [(r/Function? f1)
                                                                         (r/Function? f2)]}
                                                                  (update f1 :rng c/union-Results (:rng f2)))
                                                _ (swap! out-fn-matches update ifn-index
                                                         (fn [old]
                                                           {:pre [(or (nil? old) (r/Function? old))]
                                                            :post [(r/Function? %)]}
                                                           (if old
                                                             (union-Functions old ftype)
                                                             ftype)))]
                                            cmethod))
                                        expecteds))]
                            cmethods)))
                      mthods)))))

        out-fn-matches @out-fn-matches
        ;; if we infer the body of any return types, we now gather those inferred types.
        inferred-ifn (apply r/make-FnIntersection 
                            (map-indexed (fn [i match]
                                           {:post [(r/Function? %)]}
                                           (or match
                                               (get (:types fin) i)))
                                           out-fn-matches))
        maybe-poly-inferred-ifn (case poly?
                                  :Poly (c/Poly* (map :name inst-frees) bnds inferred-ifn)
                                  :PolyDots (c/PolyDots* (map :name inst-frees) bnds inferred-ifn)
                                  nil inferred-ifn)]
     ;; if a method is checked only once, then it could have
     ;; been rewritten, so propagate it up to the rest of the
     ;; AST.
    {:methods (mapv 
                (fn [cmethods m]
                  (if (== 1 (count cmethods))
                    (nth cmethods 0)
                    m))
                cmethodss
                mthods)
     :ifn maybe-poly-inferred-ifn
     ;; flatten out all checked methods
     :cmethods (into []
                     (mapcat identity)
                     cmethodss)}))

(defn function-types [expected]
  {:pre [(r/Type? expected)]
   :post [(and (every? function-type? %)
               (vector? %))]}
  (let [exp (c/fully-resolve-type expected)
        ts (filterv function-type?
                    (if (r/Union? exp)
                      (:types exp)
                      [exp]))]
    ts))

; Check a sequence of methods against a (possibly polymorphic) function type.
;
; If this is a deftype method, provide a recur-target-fn to handle recur behaviour
; and validate-expected-fn to prevent expected types that include a rest argument.
;
; (ann check-fn-methods [Expr Type & :optional {:recur-target-fn (Nilable [Function -> RecurTarget])
;                                               :validate-expected-fn (Nilable [FnIntersection -> Any])}
;                        -> (Coll FnMethod)])
(defn check-fn-methods [mthods expected & {:as opt}]
  {:pre [(r/Type? expected)
         ((every-pred methods? seq) mthods)
         (opt-map? opt)]
   :post [(method-return? %)]}
  ;(prn "check-fn-methods")
  (let [ts (function-types expected)]
    (cond
      (empty? ts)
      (prs/with-unparse-ns (cu/expr-ns (first mthods))
        (err/tc-delayed-error (str (prs/unparse-type expected) " is not a function type")
                              :return {:methods mthods
                                       :ifn r/-error
                                       :cmethods []}))
      
      (== 1 (count ts))
      (check-fni (nth ts 0) mthods opt)

      ;; disable rewriting in case we recheck a method arity
      :else
      (binding [vs/*can-rewrite* nil]
        (let [method-returns (into []
                                   (map (fn [t] (check-fni t mthods opt)))
                                   ts)]
          {:methods mthods
           :ifn (apply c/Un (map :ifn method-returns))
           :cmethods (mapcat :cmethods method-returns)})))))
