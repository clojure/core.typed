(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify CLJS specials

(def new-specials '#{defprotocol typed.core/ann-form-cljs})

(.doReset #'cljs.analyzer/specials (set/union cljs/specials new-specials))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special forms

(defmacro ann-form-cljs [form tsyn]
  `form)

(defmacro cljs-ann [vname tsyn]
  `(cljs-ann* '~vname '~tsyn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Envs

(defonce CLJS-VAR-ENV (atom {}))
(set-validator! CLJS-VAR-ENV (hash-c? symbol? Type?))

(defn cljs-ann* [vname tsyn]
  (let [vtype (parse-type tsyn)]
    (swap! CLJS-VAR-ENV assoc vname vtype)
    [vname (unparse-type vtype)]))

(defn type-of [vname]
  (let [t (@CLJS-VAR-ENV vname)]
    (if t
      t
      (throw (Exception. (str "Untyped var: " vname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(defrecord SymbolCLJS []
  "A type for Clojurescript symbols"
  [])

(defrecord BooleanCLJS []
  "A type for Clojurescript boolean values"
  [])

(declare-type SymbolCLJS)
(declare-type BooleanCLJS)


;; Parse type

(defmethod parse-type-symbol 'SymbolCLJS [_] (->SymbolCLJS))
(defmethod parse-type-symbol 'BooleanCLJS [_] (->BooleanCLJS))

(defmethod parse-type-list 'predicate-cljs
  [[_ t-syn]]
  (let [on-type (parse-type t-syn)]
    (make-FnIntersection
      (make-Function [-any] (->BooleanCLJS) nil nil
                     :filter (-FS (-filter on-type 0)
                                  (-not-filter on-type 0))))))

;; Unparse-type 

(defmethod unparse-type* SymbolCLJS
  [_]
  'SymbolCLJS)

(defmethod unparse-type* BooleanCLJS
  [_]
  'BooleanCLJS)

;; fold

(add-default-fold-case BooleanCLJS ret-first)
(add-default-fold-case SymbolCLJS ret-first)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(defmethod subtype* [Value SymbolCLJS ::clojurescript]
  [{:keys [val] :as s} t]
  (if (symbol? val)
    *current-env*
    (type-error s t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing new special forms

(defmethod cljs/parse 'defprotocol
  [op env [psym & doc+methods :as form] name]
  {:op :defprotocol
   :env env
   :form form})

(defmethod cljs/parse 'typed.core/ann-form-cljs
  [op env [_ form tsyn :as as] name]
  (assert (= 3 (count as)))
  {:op :ann-form-cljs
   :env env
   :form form
   :the-expr (cljs/analyze env form)
   :expected (parse-type tsyn)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check CLJS AST

(defmulti check-cljs (fn [expr & [expected]] (:op expr)))

(defmethod check-cljs :constant
  [{:keys [form] :as expr} & [expected]]
  (let [t (->Value form)
        _ (when expected
            (subtype t (ret-t expected)))]
    (assoc expr
           expr-type (ret t))))

(defmethod check-cljs :vector
  [{:keys [items] :as expr} & [expected]]
  (assert (not expected))
  (let [citems (mapv check-cljs items)]
    (assoc expr
           expr-type (ret (->HeterogeneousVector (mapv (comp ret-t expr-type) citems))))))

(defmethod check-cljs :map
  [{mkeys :keys mvals :vals :as expr} & [expected]]
  (assert (not expected))
  (let [ckeys (mapv check-cljs mkeys)
        cvals (mapv check-cljs mvals)
        ;only handle keyword keys for now
        _ (assert (every? (every-pred Value? #(keyword? (.val %)))
                          (map (comp ret-t expr-type) ckeys)))]
    (assoc expr
           expr-type (ret (->HeterogeneousMap (zipmap (map (comp ret-t expr-type) ckeys)
                                                      (map (comp ret-t expr-type) cvals)))))))

(defmethod check-cljs :def
  [{:keys [init] vname :name :as expr} & [expected]]
  (assert init "declare NYI")
  (assert (not expected))
  (let [ann-type (type-of vname)
        cinit (check-cljs init expected)
        _ (assert (subtype? (-> cinit expr-type ret-t)
                            ann-type)
                  (str "Var definition did not match annotation." \n
                       " Expected: " (unparse-type ann-type) \n
                       " Actual" (unparse-type ann-type)))]
    (assoc expr
           ;FIXME should really be Var, change when protocols are implemented
           expr-type (ret -any))))

;doesn't do anything right now
(defmethod check-cljs :defprotocol
  [{:keys [init] vname :name :as expr} & [expected]]
  (assoc expr
         expr-type (ret (->SymbolCLJS))))

(defmethod check-cljs :ann-form-cljs
  [{:keys [the-expr] given-type :expected :as expr} & [expected]]
  (let [cform (check-cljs the-expr (ret given-type))
        _ (assert (subtype? (-> cform expr-type ret-t) given-type)
                  (str "Annotation does not match actual type:"\n
                       "Expected: " (unparse-type given-type)\n
                       "Actual: " (unparse-type (-> cform expr-type ret-t))))
        _ (when expr-type
            (assert (subtype? given-type (ret-t expected))))]
    (assoc expr
           expr-type (ret given-type))))

(defmethod check-cljs :invoke
  [{fexpr :f :keys [args] :as expr} & [expected]]
  (let [cfexpr (check-cljs fexpr)
        cargs (mapv check-cljs args)
        ftype (expr-type cfexpr)
        argtys (map expr-type cargs)
        actual (check-funapp ftype argtys expected)]
    (assoc expr
           expr-type actual)))

(defmethod check-cljs :var
  [{{vname :name} :info :as expr} & [expected]]
  (assoc expr
         expr-type (ret (type-of vname))))

(defmethod check-cljs :do
  [{:keys [ret statements] :as expr} & [expected]]
  (let [cstatements (mapv check-cljs statements)
        cret (check-cljs ret expected)]
    (assoc expr
           expr-type (expr-type cret))))
           

(defmethod check-cljs :fn
  [{:keys [name max-fixed-arity methods variadic] :as expr} & [expected]]
  (binding [*check-fn-method1-checkfn* check-cljs]
    (assoc expr
           expr-type
           (check-fn (-> ;conform to what `check-fn` expects for now
                       expr
                       (dissoc :variadic)
                       (assoc :variadic-method variadic)
                       (update-in [:methods] #(map (fn [{:keys [params max-fixed-arity variadic ret statements] :as cljs-m}]
                                                     {:required-params (map (fn [p] {:sym p}) params),
                                                      :rest-param (when variadic
                                                                    {:sym variadic})
                                                      ;transform body into a `do`
                                                      :body {:op :do, :ret ret, :statements statements}})
                                                   %)))
                     (or expected
                         (ret (make-FnIntersection
                                (make-Function [] -any -any))))))))

;; Debug

(defn ana-cljs [env form]
  (binding [cljs/*cljs-ns* (-> env :ns :name)]
    (cljs/analyze env form)))

(comment
  ;; TODO there's a bug in the docstring for cljs.analyzer/analyze: it says :ns is a symbol, when instead it's {:name nsym}
  (def denv {:locals {} :context :statement :ns {:name 'cljs.user}})

(cljs/analyze denv 1)
  (cf-cljs 1)

(cljs/analyze denv [1])
  (cf-cljs [1])

(cljs/analyze denv {:a 1})
(cf-cljs {:a 1})

(cljs-ann user/a Any)
  (@CLJS-VAR-ENV 'user/a)

(cljs/analyze denv '(def a 1))
(cf-cljs (def a 1))

; defprotocol doesn't macroexpand because we've added 'defprotocol as a special
(cljs/macroexpand-1 {} '(defprotocol A))
(cljs/analyze denv '(defprotocol A))
(cf-cljs (defprotocol A))


(ana-cljs denv '(ns cljs.user (:require-macros [typed.core :as typ])))
(cljs/macroexpand-1 {} '(ann-form-cljs 'a SymbolCLJS))
(cf-cljs (ann-form-cljs 'a SymbolCLJS))

  ;occurrence typing
(cljs-ann cljs.core/symbol? (predicate-cljs SymbolCLJS))
(ana-cljs denv 'cljs.core/symbol?)
(cf-cljs (cljs.core/symbol? 'a))

; do                                         
(ana-cljs denv '(do 1 2 ))

; fn
(ana-cljs denv '(fn [a] 12 1 ))
(cf-cljs (fn []))
(cf-cljs (fn [a]))
(cf-cljs (fn [a b c]))
(cf-cljs (fn [a b c]) [BooleanCLJS BooleanCLJS Any -> nil])

(cf-cljs (fn [a b c] a) [BooleanCLJS BooleanCLJS Any -> BooleanCLJS])
  )
