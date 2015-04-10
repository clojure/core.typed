(ns clojure.core.typed.check.fn-methods
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.fn-method :as fn-method]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.check.fn-method-one :as fn-method1]
            [clojure.core.typed.dvar-env :as dvar-env]))

(def function-type? (some-fn (every-pred r/Poly?
                                         (comp r/FnIntersection? r/Poly-body-unsafe*))
                             (every-pred r/PolyDots?
                                         (comp r/FnIntersection? r/PolyDots-body-unsafe*))
                             r/FnIntersection?))

(def method? (some-fn ast-u/fn-method? ast-u/deftype-method?))
(def methods? (con/every-c? method?))

(def opt-map? (con/hmap-c? (con/optional :recur-target-fn) ifn?
                           (con/optional :validate-expected-fn) ifn?
                           (con/optional :self-name) (some-fn nil? symbol?)))

(defn check-Function
  "Check individual Function type against all methods"
  [mthods {:keys [dom rest drest kws prest pdot] :as f} {:keys [recur-target-fn]}]
  {:pre [((every-pred methods? seq) mthods)
         (r/Function? f)
         ((some-fn nil? ifn?) recur-target-fn)]
   :post [(methods? %)]}
  ;(prn "check-Function" f)
  (let [ndom (count dom)
        expected-for-method 
        (fn [{:keys [fixed-arity] :as method}]
          {:pre [(method? method)]
           :post [((some-fn nil? r/Function?) %)]}
          ;; fn-method-u/*check-fn-method1-rest-type*, and check-fn-method1
          ;; actually distribute the types amongst the fixed and rest parameters
          (let [variadic?   (ast-u/variadic-method? method)
                fixed-arity (ast-u/fixed-arity method)]
            (cond
              (or rest drest prest pdot)
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
              (= nil rest drest kws)
              (cond
                variadic? nil
                (== ndom fixed-arity) f
                :else nil))))

        maybe-check (fn [method]
                      {:pre [(method? method)]
                       :post [((some-fn nil? method?) %)]}
                      (when-let [fe (expected-for-method method)]
                        ;(prn "inner expected in check-Function" fe)
                        (let [{:keys [cmethod]} (fn-method1/check-fn-method1
                                                  method 
                                                  fe
                                                  :recur-target-fn recur-target-fn)]
                          (assert (method? cmethod))
                          cmethod)))
        ms (->> mthods
                (map maybe-check)
                (filter identity)
                vec)]
    ;(prn "checked ms" (count ms))
    (when (empty? ms)
      (binding [vs/*current-expr* (impl/impl-case
                                    :clojure (first mthods)
                                    ; fn-method is not printable in cljs
                                    :cljs vs/*current-expr*)
                vs/*current-env* (or (:env (first mthods)) vs/*current-env*)]
        (prs/with-unparse-ns (cu/expr-ns (first mthods))
          (err/tc-delayed-error (str "No matching arities: " (prs/unparse-type f))))))
    ms))


(defn check-fni [exp mthods
                 {:keys [recur-target-fn
                         validate-expected-fn
                         self-name]
                  :as opt}]
  {:pre [(function-type? exp)
         (methods? mthods)
         (opt-map? opt)]
   :post [(methods? %)]}
  ;(prn "check-fni" exp)
  (let [; unwrap polymorphic expected types
        [fin inst-frees bnds poly?] (cu/unwrap-poly exp)
        ; this should never fail due to function-type? check
        _ (assert (r/FnIntersection? fin))
        _ (when validate-expected-fn
            (validate-expected-fn fin))
        ;collect all inferred Functions
        cmethods
        (lex/with-locals (when-let [name self-name] ;self calls
                           {name exp})
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
              (vec
                (mapcat (fn [f]
                          {:pre [(r/Function? f)]
                           ;returns a collection of fn-method's
                           :post [(methods? %)]}
                          (check-Function
                            mthods
                            f
                            {:recur-target-fn recur-target-fn}))
                        (:types fin))))))]
    cmethods))

(defn function-types [expected]
  {:pre [(r/Type? expected)]
   :post [(every? function-type? %)]}
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
   :post [(methods? %)]}
  ;(prn "check-fn-methods")
  (let [ts (function-types expected)]
    (if (empty? ts)
      (prs/with-unparse-ns (cu/expr-ns (first mthods))
        (err/tc-delayed-error (str (prs/unparse-type expected) " is not a function type")
                              :return []))
      (vec
        (mapcat (fn [t] (check-fni t mthods opt))
                ts)))))
