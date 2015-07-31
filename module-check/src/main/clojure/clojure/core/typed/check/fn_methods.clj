(ns clojure.core.typed.check.fn-methods
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.check.utils :as cu]
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
    :methods methods?
    :cmethods methods?))

(defn expected-for-method
  "Takes a :method AST node and a single Function arity type,
  and returns the Function if the :method node should be checked
  against the Function, otherwise returns nil."
  [{:keys [fixed-arity] :as method}
   {:keys [dom rest drest kws] :as f}]
  {:pre [(method? method)
         (r/Function? f)]
   :post [((some-fn nil? r/Function?) %)]}
  ;; fn-method-u/*check-fn-method1-rest-type*, and check-fn-method1
  ;; actually distribute the types amongst the fixed and rest parameters
  (let [ndom (count dom)
        variadic?   (ast-u/variadic-method? method)
        fixed-arity (ast-u/fixed-arity method)]
    (cond
      (or rest drest)
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
              (let [;; map from function type to a set of matching methods (integers).
                    fn-matches
                    (into {}
                          (map (fn [t]
                                 (let [ms (into #{}
                                                (comp
                                                  (map-indexed (fn [i m]
                                                                 (when (expected-for-method m t)
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
                        (fn [i m]
                          (let [expecteds (keep 
                                            (fn [[t es]]
                                              (when (es i)
                                                t))
                                            fn-matches)]
                            (u/rewrite-when (== 1 (count expecteds))
                              (mapv (comp
                                      :cmethod
                                      #(fn-method1/check-fn-method1 m % :recur-target-fn recur-target-fn))
                                    expecteds)))))
                      mthods)))))]
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
                                       :cmethods []}))
      
      (== 1 (count ts))
      (check-fni (nth ts 0) mthods opt)

      ;; disable rewriting in case we recheck a method arity
      :else
      (binding [vs/*can-rewrite* nil]
        {:methods mthods
         :cmethods
         (into []
               (mapcat (fn [t] (check-fni t mthods opt)))
               ts)}))))
