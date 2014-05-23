(ns clojure.core.typed.check.fn-methods
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.check.fn-method :as fn-method]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.dvar-env :as dvar-env]
            )
  )

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
   :post [(-> % :fni r/Type?)]}
  ; FIXME Unions of functions are not supported yet
  (let [;; FIXME This is trying to be too smart, should be a simple cond with Poly/PolyDots cases

        ; try and unwrap type enough to find function types
        exp (c/fully-resolve-type expected)
        ; unwrap polymorphic expected types
        [fin inst-frees bnds poly?] (cu/unwrap-poly exp)
        ; once more to make sure (FIXME is this needed?)
        fin (c/fully-resolve-type fin)
        ;ensure a function type
        _ (when-not (r/FnIntersection? fin)
            (err/int-error
              (str (pr-str (prs/unparse-type fin)) " is not a function type")))
        _ (when validate-expected-fn
            (validate-expected-fn fin))
        ;collect all inferred Functions
        {:keys [inferred-fni cmethods]}
                     (lex/with-locals (when-let [name self-name] ;self calls
                                        (when-not expected 
                                          (err/int-error (str "Recursive functions require full annotation")))
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
                           (let [method-infos (mapv (fn [method]
                                                      {:post [(seq %)]}
                                                      (fn-method/check-fn-method 
                                                        method 
                                                        fin
                                                        :recur-target-fn recur-target-fn))
                                                    methods)]
                             {:cmethods (vec (mapcat #(map :cmethod %) method-infos))
                              :inferred-fni (apply r/make-FnIntersection (mapcat #(map :ftype %) method-infos))}))))
        _ (assert (r/Type? inferred-fni))
        ;rewrap in Poly or PolyDots if needed
        pfni (cu/rewrap-poly inferred-fni inst-frees bnds poly?)]
    {:cmethods cmethods
     :fni pfni}))
