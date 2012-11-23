(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify CLJS specials

(assert cljs/specials)
(def orig-specials cljs/specials)

(def new-specials (set/union 
                    orig-specials
                    '#{cljs.core/defprotocol deftype typed.core/ann-form-cljs
                       defprotocol
                       cljs.core/extend-type}))

(defmacro with-altered-specials
  [& body]
  `(try
     (alter-var-root #'cljs.analyzer/specials (constantly new-specials))
     ~@body
     (finally
       (alter-var-root #'cljs.analyzer/specials (constantly orig-specials)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special forms

(defmacro ann-form-cljs [form tsyn]
  `form)

(declare cljs-ann*)

(defmacro cljs-ann [vname tsyn]
  `'~(cljs-ann* vname tsyn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Envs

(defonce CLJS-VAR-ENV (atom {}))
(set-validator! CLJS-VAR-ENV (hash-c? symbol? Type?))

(defn cljs-ann* [vname tsyn]
  (let [vtype (parse-type tsyn)
        var (if (namespace vname)
              (symbol vname)
              (symbol (str cljs/*cljs-ns*) (str vname)))]
    (swap! CLJS-VAR-ENV assoc var vtype)
    [var (unparse-type vtype)]))

(defonce CLJS-PROTOCOL-ENV (atom {}))

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

(defmethod subtype* [Value BooleanCLJS ::clojurescript]
  [{:keys [val] :as s} t]
  (if ((some-fn true? false?) val)
    *current-env*
    (type-error s t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing new special forms

(defmethod cljs/parse 'defprotocol
  [op env [_ psym & doc+methods :as form] name]
  {:op :defprotocol
   :env env
   :form form})

(defmethod cljs/parse 'cljs.core/extend-type
  [op env [_ t & impls :as form] name]
   (let [parse-impl (fn [m] {:name (first m)
                             :method (cljs/analyze env `(~'fn ~@(rest m)))
                             :form m})
         impl-map-syn ;from cljs/core.clj
         (loop [ret {} s impls]
           (if (seq s)
             (recur (assoc ret (first s) (take-while seq? (next s)))
                    (drop-while seq? (next s)))
             ret))
         impl-map (into {} (for [[k v] impl-map-syn]
                             [k (map parse-impl v)]))]
     {:op :extend-type
      :t t
      :impl-map impl-map
      :form form
      :env env}))

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

(defmethod check-cljs :no-op
  [expr & [expected]]
  (assoc expr
         expr-type (ret -any)))

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
  [{:keys [init env] vname :name :as expr} & [expected]]
  (assert init "declare NYI")
  (assert (not expected))
  (let [ann-type (binding [*var-annotations* CLJS-VAR-ENV
                           *current-env* env]
                   (type-of vname))
        cinit (check-cljs init expected)
        _ (assert (subtype? (-> cinit expr-type ret-t)
                            ann-type)
                  (str "Var definition did not match annotation."
                       " Expected: " (unparse-type ann-type)
                       ", Actual: " (unparse-type (-> cinit expr-type ret-t))))]
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
  [{{vname :name} :info :keys [env] :as expr} & [expected]]
  (assoc expr
         expr-type (ret (binding [*var-annotations* CLJS-VAR-ENV
                                  *current-env* env]
                          (type-of vname)))))

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

(defmethod check-cljs :deftype*
  [expr & [expected]]
  (assert (not expected))
  (assoc expr
         expr-type (ret -any)))

(defmethod check-cljs :set!
  [{:keys [target val] :as expr} & [expected]]
  (assert (not expected))
  (let [ctarget (check-cljs target)
        cval (check-cljs val)]
    (assoc expr
           expr-type (ret -any))))

(defn check-field 
  [{:keys [target field val] :as expr} & [expected]]
  (assert false))

(defmethod check-cljs :dot
  [{:keys [field method] :as expr} & [expected]]
  #_((if field check-field (throw (Exception. "NYI")))
    expr expected)
  (assoc expr
         expr-type (ret -any)))

(defmethod check-cljs :if
  [{:keys [test then else] :as expr} & [expected]]
  (let [ctest (check-cljs test)]
    (assoc expr
           expr-type (binding [*check-if-checkfn* check-cljs]
                       (check-if (expr-type ctest) then else)))))

(defmethod check-cljs :let
  [{:keys [loop bindings statements ret env] :as expr} & [expected]]
  (let [;; conform to Clojure `analyze` for now
        bindings (mapv #(let [n (:name %)]
                          {:local-binding (-> % (dissoc :name) (assoc :sym n))})
                       bindings)
        body {:op :do, :statements statements, :ret ret :env env}]
  (binding [*check-let-checkfn* check-cljs]
    (if loop
      (assert false) #_(check-let bindings body expr true expected)
      (check-let bindings body expr false expected)))))

(defmethod check-cljs :ns
  [expr & [expected]]
  (assoc expr
         expr-type (ret -any)))

;; Debug

(defn ana-cljs [env form]
  (with-altered-specials
    (binding [cljs/*cljs-ns* cljs/*cljs-ns*]
      (cljs/analyze env form))))

(comment
  ;; TODO there's a bug in the docstring for cljs.analyzer/analyze: it says :ns is a symbol, when instead it's {:name nsym}
  (def denv {:locals {} :context :statement :ns {:name 'cljs.user}})

(cljs/analyze denv 1)
  (cf-cljs 1)

(cljs/analyze denv [1])
  (cf-cljs [1])

(cljs/analyze denv {:a 1})
(cf-cljs {:a 1})

(cljs/analyze denv (cljs-ann cljs.user/a Any))
  (@CLJS-VAR-ENV 'cljs.user/a)

(cljs/analyze denv '(def a 1))
(cf-cljs (def a 1))

; defprotocol doesn't macroexpand because we've added 'defprotocol as a special
  (with-altered-specials
    (prn cljs/specials)
    (cljs/macroexpand-1 {} '(defprotocol A)))
(cljs/analyze (cljs/empty-env) '(defprotocol A))
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

(ana-cljs denv '(fn [a] a))
(cf-cljs (fn [a b c] a) [BooleanCLJS BooleanCLJS Any -> BooleanCLJS])

; deftype
(ana-cljs denv '(deftype A [b] cljs.core/ASeq))
  (cljs/macroexpand-1 (cljs/empty-env)
                      '(deftype A [b] 
                         cljs.core/ASeq 
                         cljs.core/IFn
                         (invoke [this a b] a)))
  (cljs/macroexpand-1 (cljs/empty-env)
                      '(cljs.core/extend-type A 
                                              cljs.core/ASeq 
                                              cljs.core/ISeq 
                                              (first [this] this)
                                              #_cljs.core/IFn #_(invoke ([this a b] a))))
(cljs/analyze (cljs/empty-env) '(deftype A [b] 
                  cljs.core/ASeq 
                  cljs.core/IFn
                  (invoke [this a b] a)))
  (cf-cljs (deftype A [b] 
             cljs.core/ASeq 
             cljs.core/IFn
             (invoke [this a b] a)))
(ana-cljs denv '(set! o -a 1))
(ana-cljs denv '(set! o 1))
(cf-cljs (set! o -a 1))
(ana-cljs denv '(.-cljs$lang$type 1))
(cf-cljs (.-cljs$lang$type 1))
(ana-cljs denv '(.-cljs$lang$type 1))
(cf-cljs (set! cljs.core/symbol? 1))


  (ana-cljs denv '(if 1 2 3))
  (cf-cljs (if 1 2 3))

  (ana-cljs denv '(let [a 2] a))
  (cf-cljs (let [a 2] a))


  ;ns
  (cf-cljs (ns my-ns (:require [cljs.core :as s])))

  (check-cljs-ns typed.test.logic)
  )
