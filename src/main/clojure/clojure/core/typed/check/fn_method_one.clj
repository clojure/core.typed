(ns clojure.core.typed.check.fn-method-one
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.open-result :as open-result]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.check.fn-method-utils :as fn-method-u]
            [clojure.core.typed.check.multi-utils :as multi-u]
            [clojure.core.typed.check.funapp :as funapp]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.check.isa :as isa]
            [clojure.core.typed.update :as update]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.check.recur-utils :as recur-u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]))

;check method is under a particular Function, and return inferred Function
; if ignore-rng is true, otherwise return expression with original expected type.
;
; check-fn-method1 exposes enough wiring to support the differences in deftype
; methods and normal methods via `fn`.
;
; # Differences in recur behaviour
;
; deftype methods do *not* pass the first parameter (usually `this`) when calling `recur`.
;
; eg. (my-method [this a b c] (recur a b c))
;
; The behaviour of generating a RecurTarget type for recurs is exposed via the :recur-target-fn
;
;
;[MethodExpr Function -> {:ftype Function :cmethod Expr}]
(defn check-fn-method1 [method {:keys [dom rest drest kws prest pdot] :as expected}
                        & {:keys [recur-target-fn ignore-rng]}]
  {:pre [(r/Function? expected)]
   :post [(r/Function? (:ftype %))
          (-> % :cmethod ::t/ftype r/Function?)
          (:cmethod %)]}
  (impl/impl-case
    :clojure (assert (#{:fn-method :method} (:op method))
                     (:op method))
    ; is there a better :op check here?
    :cljs (assert method))
  #_(prn "checking syntax:" (ast-u/emit-form-fn method))
  ;(prn "check-fn-method1" "ignore-rng" ignore-rng)
  (u/p :check/check-fn-method1
  (let [body ((ast-u/method-body-kw) method)
        required-params (ast-u/method-required-params method)
        rest-param (ast-u/method-rest-param method)

        param-obj (comp #(obj/-path nil %)
                        :name)
        ; Difference from Typed Racket:
        ;
        ; Because types can contain abstracted names, we instantiate
        ; the expected type in the range before using it.
        ;
        ; eg. Checking against this function type:
        ;      [Any Any
        ;       -> (HVec [(U nil Class) (U nil Class)]
        ;                :objects [{:path [Class], :id 0} {:path [Class], :id 1}])]
        ;     means we need to instantiate the HVec type to the actual argument
        ;     names with open-Result.
        ;
        ;     If the actual function method is (fn [a b] ...) we check against:
        ;
        ;       (HVec [(U nil Class) (U nil Class)]
        ;              :objects [{:path [Class], :id a} {:path [Class], :id b}])
        expected-rng (when-not ignore-rng
                       (apply r/ret
                              (open-result/open-Result 
                                (:rng expected)
                                (map param-obj
                                     (concat required-params 
                                             (when rest-param [rest-param]))))))
        ;_ (prn "expected-rng" expected-rng)
        ;ensure Function fits method
        _ (when-not ((if (or rest drest kws prest pdot) <= =) (count required-params) (count dom))
            (err/int-error (str "Checking method with incorrect number of expected parameters"
                              ", expected " (count dom) " required parameter(s) with"
                              (if rest " a " " no ") "rest parameter, found " (count required-params)
                              " required parameter(s) and" (if rest-param " a " " no ")
                              "rest parameter.")))

        _ (when-not (or (not rest-param)
                        (some identity [drest rest kws prest pdot]))
            (err/int-error (str "No type for rest parameter")))

        ;;unhygienic version
        ;        ; Update filters that reference bindings that the params shadow.
        ;        ; Abstracting references to parameters is handled later in abstract-result, but
        ;        ; suffers from bugs due to un-hygienic macroexpansion (see `abstract-result`).
        ;        ; In short, don't shadow parameters if you want meaningful filters.
        ;        props (mapv (fn [oldp]
        ;                      (reduce (fn [p sym]
        ;                                {:pre [(fl/Filter? p)
        ;                                       (symbol? sym)]}
        ;                                (subst-filter p sym obj/-empty true))
        ;                              oldp (map :sym required-params)))
        ;                    (:props (lex/lexical-env)))

        props (:props (lex/lexical-env))
        crequired-params (map (fn [p t] (assoc p u/expr-type (r/ret t)))
                              required-params
                              (concat dom 
                                      (repeat (or rest (:pre-type drest) prest (:pre-type pdot)))))
        _ (assert (every? (comp r/TCResult? u/expr-type) crequired-params))
        fixed-entry (map (juxt :name (comp r/ret-t u/expr-type)) crequired-params)
        ;_ (prn "checking function:" (prs/unparse-type expected))
        check-fn-method1-rest-type fn-method-u/*check-fn-method1-rest-type*
        _ (assert check-fn-method1-rest-type "No check-fn bound for rest type")
        crest-param (when rest-param
                      (assoc rest-param
                             u/expr-type (r/ret (check-fn-method1-rest-type (drop (count crequired-params) dom)
                                                                            :rest rest
                                                                            :drest drest
                                                                            :kws kws
                                                                            :prest prest
                                                                            :pdot pdot))))
        rest-entry (when crest-param
                     [[(:name crest-param) (r/ret-t (u/expr-type crest-param))]])
        ;_ (prn "rest entry" rest-entry)
        _ (assert ((con/hash-c? symbol? r/Type?) (into {} fixed-entry))
                  (into {} fixed-entry))
        _ (assert ((some-fn nil? (con/hash-c? symbol? r/Type?)) (when rest-entry
                                                              (into {} rest-entry))))

        ; if this fn method is a multimethod dispatch method, then infer
        ; a new filter that results from being dispatched "here"
        mm-filter (when-let [{:keys [dispatch-fn-type dispatch-val-ret]} multi-u/*current-mm*]
                    (u/p :check/check-fn-method1-inner-mm-filter-calc
                    (assert (and dispatch-fn-type dispatch-val-ret))
                    (assert (not (or drest rest rest-param)))
                    (let [disp-app-ret (funapp/check-funapp nil nil 
                                                     (r/ret dispatch-fn-type)
                                                     (map r/ret dom (repeat (fo/-FS fl/-top fl/-top)) 
                                                          (map param-obj required-params))
                                                     nil)
                          ;_ (prn "disp-app-ret" disp-app-ret)
                          ;_ (prn "disp-fn-type" (prs/unparse-type dispatch-fn-type))
                          ;_ (prn "dom" dom)
                          isa-ret (isa/tc-isa? disp-app-ret dispatch-val-ret nil)
                          then-filter (-> isa-ret r/ret-f :then)
                          _ (assert then-filter)]
                      then-filter)))
        ;_ (prn "^^^ mm-filter")

        ;_ (prn "funapp1: inferred mm-filter" mm-filter)

        env (let [env (-> (lex/lexical-env)
                          ;add mm-filter
                          (assoc-in [:props] (set (concat props (when mm-filter [mm-filter]))))
                          ;add parameters to scope
                          ;IF UNHYGIENIC order important, (fn [a a & a]) prefers rightmost name
                          (update-in [:l] merge (into {} fixed-entry) (into {} rest-entry)))
                  flag (atom true :validator con/boolean?)
                  env (if mm-filter
                        (let [t (update/env+ env [mm-filter] flag)]
                          t)
                        env)]
              (when-not @flag
                (err/int-error "Unreachable method: Local inferred to be bottom when applying multimethod filter"))
              env)

        check-fn-method1-checkfn fn-method-u/*check-fn-method1-checkfn*
        _ (assert check-fn-method1-checkfn "No check-fn bound for method1")
        ; rng before adding new filters
        crng-nopass
        (u/p :check/check-fn-method1-chk-rng-pass1
        (binding [multi-u/*current-mm* nil]
          (var-env/with-lexical-env env
            (let [rec (or ; if there's a custom recur behaviour, use the provided
                          ; keyword argument to generate the RecurTarget.
                          (when recur-target-fn
                            (recur-target-fn expected))
                          ; Otherwise, assume we are checking a regular `fn` method
                          (recur-u/RecurTarget-maker dom rest drest nil))
                  _ (assert (recur-u/RecurTarget? rec))]
              (recur-u/with-recur-target rec
                (check-fn-method1-checkfn body expected-rng))))))

        ; Apply the filters of computed rng to the environment and express
        ; changes to the lexical env as new filters, and conjoin with existing filters.

        then-env (u/p :check/check-fn-method1-env+-rng
                      (let [{:keys [then]} (-> crng-nopass u/expr-type r/ret-f)]
                        (if (fl/NoFilter? then)
                          env
                          (update/env+ env [then] (atom true)))))
        new-then-props (reduce (fn [fs [sym t]]
                                 {:pre [((con/set-c? fl/Filter?) fs)]}
                                 (if (= t (get-in env [:l sym]))
                                   ;type hasn't changed, no new propositions
                                   fs
                                   ;new type, add positive proposition
                                   (conj fs (fo/-filter-at t (lex/lookup-alias sym :env env)))))
                               #{}
                               (:l then-env))

        crng (u/p :check/check-fn-method1-add-rng-filters
               (update-in crng-nopass [u/expr-type :fl :then] 
                          (fn [f]
                            (apply fo/-and f new-then-props))))
        ;_ (prn "crng" (u/expr-type crng))
        rest-param-name (when rest-param
                          (:name rest-param))

        ftype (fn-method-u/FnResult->Function
                (fn-method-u/FnResult-maker
                  fixed-entry
                  (when (and kws rest-param)
                    [rest-param-name kws])
                  (when (and rest rest-param)
                    [rest-param-name rest])
                  (when (and drest rest-param)
                    [rest-param-name drest])
                  (when (and prest rest-param)
                    [rest-param-name prest])
                  (when (and pdot rest-param)
                    [rest-param-name pdot])
                  (u/expr-type crng)))
                        
        cmethod (-> (assoc method
                           (ast-u/method-body-kw) crng
                           ::t/ftype ftype)
                    (ast-u/reconstruct-arglist crequired-params crest-param))
        _ (assert (vector? (:params cmethod)))
        _ (assert (every? (comp r/TCResult? u/expr-type) (:params cmethod)))]
     {:ftype ftype
      :cmethod cmethod})))
