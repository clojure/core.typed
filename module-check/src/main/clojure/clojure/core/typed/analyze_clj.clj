(ns ^:skip-wiki clojure.core.typed.analyze-clj
  (:refer-clojure :exclude [macroexpand-1 get-method eval])
  (:require [clojure.tools.analyzer :as ta]
            [clojure.pprint :as pp]
            [clojure.tools.analyzer.env :as ta-env]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.utils :as ta-utils]
            [clojure.tools.analyzer.passes :as passes]
            [clojure.tools.analyzer.passes.source-info :as source-info]
            [clojure.tools.analyzer.passes.cleanup :as cleanup]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.tools.analyzer.passes.trim :as trim]
            [clojure.tools.analyzer.passes.jvm.warn-on-reflection :as warn-reflect]
            [clojure.core.typed.analyzer2.jvm :as jana2]
            [clojure.core.typed.analyzer2 :as ana2]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.tools.analyzer.passes.jvm.validate :as validate]
            [clojure.java.io :as io]
            [clojure.reflect :as reflect]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed :as T]
            [clojure.core.typed :as t]
            [clojure.core.cache :as cache]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.errors :as err]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core :as core]
            [clojure.core.typed.rules :as rules]
            [clojure.core.typed.expand :as expand]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.ns-deps :as dep]
            [clojure.core.typed.ns-deps-utils :as dep-u])
  (:import (clojure.tools.analyzer.jvm ExceptionThrown)))

(def custom-expansions? false)

; Updated for Clojure 1.8
;  https://github.com/clojure/clojure/commit/7f79ac9ee85fe305e4d9cbb76badf3a8bad24ea0
(T/ann ^:no-check *typed-macros* (T/Map T/Any T/Any))
(def ^:dynamic *typed-macros*
 (merge
  {#'clojure.core/ns 
   (fn [&form &env name & references]
     (let [process-reference
           (fn [[kname & args]]
             `(~(symbol "clojure.core" (clojure.core/name kname))
                        ~@(map #(list 'quote %) args)))
           docstring  (when (string? (first references)) (first references))
           references (if docstring (next references) references)
           name (if docstring
                  (vary-meta name assoc :doc docstring)
                  name)
           metadata   (when (map? (first references)) (first references))
           references (if metadata (next references) references)
           name (if metadata
                  (vary-meta name merge metadata)
                  name)
           gen-class-clause (first (filter #(= :gen-class (first %)) references))
           gen-class-call
           (when gen-class-clause
             (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
           references (remove #(= :gen-class (first %)) references)
           ;ns-effect (clojure.core/in-ns name)
           name-metadata (meta name)]
       ;; core.typed side effect
       (prn "ns form" &form)
       (let [prs-ns (dep-u/ns-form-name &form)
             deps   (dep-u/ns-form-deps &form)
             tdeps (set (filter dep-u/should-check-ns? deps))]
         ;; is this line needed?
         (dep/add-ns-deps prs-ns tdeps))
       `(do
          ::T/special-collect
          ::core/ns
          {:form '~&form}
          (T/tc-ignore
            (clojure.core/in-ns '~name)
            ~@(when name-metadata
                `((.resetMeta (clojure.lang.Namespace/find '~name) ~name-metadata)))
            (with-loading-context
              ~@(when gen-class-call (list gen-class-call))
              ~@(when (and (not= name 'clojure.core) (not-any? #(= :refer-clojure (first %)) references))
                  `((clojure.core/refer '~'clojure.core)))
              ~@(map process-reference references))
            (if (.equals '~name 'clojure.core) 
              nil
              (do (dosync (commute @#'clojure.core/*loaded-libs* (T/inst conj T/Symbol T/Any) '~name)) nil)))
          ;; so core.typed knows `ns` always returns nil
          nil)))
   ;; add positional information for destructured bindings
   #'clojure.core/loop
   (fn [&form &env bindings & body]
     (@#'clojure.core/assert-args
       (vector? bindings) "a vector for its binding"
       (even? (count bindings)) "an even number of forms in binding vector")
     (let [db (destructure bindings)]
       (if (= db bindings)
         `(loop* ~bindings ~@body)
         (let [vs (take-nth 2 (drop 1 bindings))
               bs (take-nth 2 bindings)
               gs (map (fn [b] (if (symbol? b) 
                                 b 
                                 ;; preserve positional metadata
                                 (with-meta 
                                   (gensym) 
                                   (meta b))))
                       bs)
               bfs (reduce (fn [ret [b v g]]
                             (if (symbol? b)
                               (conj ret g v)
                               (conj ret g v b g)))
                           [] (map vector bs vs gs))]
           `(let ~bfs
              (loop* ~(vec (interleave gs gs))
                     (let ~(vec (interleave bs gs))
                       ~@body)))))))

   ;; setting the :macro metadata on a var is a runtime side effect that
   ;; core.typed cannot see. Here, we tc-ignore the body of macros manually.
   #'clojure.core/defmacro
   (fn [&form &env & args]
     `(T/tc-ignore
        ~(apply @#'core/defmacro &form &env args)))
   }
  (when-not custom-expansions?
    {#'clojure.core/for
     (fn [&form &env seq-exprs body-expr]
       (@#'T/for &form &env seq-exprs body-expr))})))

(T/ann ^:no-check typed-macro-lookup [T/Any :-> T/Any])
(defn typed-macro-lookup [var]
  {:post [(ifn? %)]}
  (or (get *typed-macros* var)
      (when custom-expansions?
        (when (expand/custom-expansion? (coerce/var->symbol var))
          (fn [form locals & _args_]
            (expand/expand-macro form {:vsym (coerce/var->symbol var)
                                       :locals locals}))))
      var))

;; copied from tools.analyze.jvm to insert `*typed-macros*`
(T/ann ^:no-check macroexpand-1 
       (T/IFn [T/Any -> T/Any] 
              [T/Any (T/Map T/Any T/Any) -> T/Any]))
(defn macroexpand-1
  "If form represents a macro form or an inlinable function, returns its expansion,
   else returns form."
  ([form] (macroexpand-1 form (taj/empty-env)))
  ([form env]
   ;(prn "macroexpand-1" form)
     (ta-env/ensure (taj/global-env)
       (cond

        (seq? form)
        (let [[op & args] form]
          (if (taj/specials op)
            form
            (let [v (ta-utils/resolve-sym op env)
                  m (meta v)
                  local? (-> env :locals (get op))
                  macro? (and (not local?) (:macro m)) ;; locals shadow macros
                  inline-arities-f (:inline-arities m)
                  inline? (or
                            (when custom-expansions?
                              (when (and (not local?) (var? v))
                                (let [vsym (coerce/var->symbol v)]
                                  (when (expand/custom-inline? vsym)
                                    (fn [& _args_]
                                      (expand/expand-inline form {:vsym vsym}))))))
                            (and (not local?)
                                 (or (not inline-arities-f)
                                     (inline-arities-f (count args)))
                                 (:inline m)))
                  t (:tag m)]
              (cond

               macro?
               (let [res (apply (typed-macro-lookup v) form (:locals env) (rest form))] ; (m &form &env & args)
                 (taj/update-ns-map!)
                 (if (ta-utils/obj? res)
                   (vary-meta res merge (meta form))
                   res))

               inline?
               (let [res (apply inline? args)]
                 (taj/update-ns-map!)
                 (if (ta-utils/obj? res)
                   (vary-meta res merge
                              (and t {:tag t})
                              (meta form))
                   res))

               :else
               (taj/desugar-host-expr form env)))))

        (symbol? form)
        (taj/desugar-symbol form env)

        :else
        form))))

;; Syntax Expected -> ChkAST

(defn unanalyzed-expr [form]
  {:op :const
   :type :nil
   :form form
   ::unanalyzed true})

(T/ann ^:no-check special-form? [T/Any :-> T/Any])
(defn special-form? [mform]
  (and (seq? mform)
       (= 'do (first mform))
       (or (= (second mform) spec/special-form)
           (= (second mform) ::T/special-collect))))

(declare eval-ast)
(T/ann ^:no-check eval-ast [(T/Map T/Any T/Any) (T/Map T/Any T/Any) :-> T/Any])
(T/ann ^:no-check analyze+eval [T/Any :-> T/Any])
(defn analyze+eval
  "Like analyze but evals the form after the analysis and attaches the
   returned value in the :result field of the AST node.
   If evaluating the form will cause an exception to be thrown, the exception
   will be caught and the :result field will hold an ExceptionThrown instance
   with the exception in the \"e\" field.

   Useful when analyzing whole files/namespaces.
  
  Mandatory keyword arguments
   :expected Takes :expected option, the expected static type or nil.

  Optional keyword arguments
   :eval-fn  Takes :eval-fn option that takes an AST and an option map and returns an 
             evaluated and possibly type-checked AST.
   :stop-analysis   an atom that, when set to true, will stop the next form from analysing.
                    This is helpful if in a top-level do and one of the do statements 
                    has a type error and is not evaluated."
  ([form] (analyze+eval form (taj/empty-env) {}))
  ([form env] (analyze+eval form env {}))
  ([form env {:keys [eval-fn stop-analysis] :or {eval-fn eval-ast} :as opts}]
   {:pre [(map? env)]}
     (ta-env/ensure (taj/global-env)
       (taj/update-ns-map!) 
       ;(prn "analyze+eval" form *ns* (ns-aliases *ns*))
       (let [[mform raw-forms] (binding [ta/macroexpand-1 (get-in opts [:bindings #'ta/macroexpand-1] 
                                                                  ;; use custom macroexpand-1
                                                                  macroexpand-1)]
                                 (loop [form form raw-forms []]
                                   (let [mform (ta/macroexpand-1 form env)]
                                     (if (= mform form)
                                       [mform (seq raw-forms)]
                                       (recur mform (conj raw-forms form))))))]
         (if (and (seq? mform) (= 'do (first mform)) (next mform)
                  ;; if this is a typed special form like an ann-form, don't treat like
                  ;; a top-level do.
                  (not (special-form? mform)))
           ;; handle the Gilardi scenario.
           ;; we don't track exceptional control flow on a top-level do, which
           ;; probably won't be an issue.
           (let [[statements ret] (ta-utils/butlast+last (rest mform))
                 statements-expr (mapv (fn [s] 
                                         (if (some-> stop-analysis deref)
                                           (unanalyzed-expr s)
                                           (analyze+eval s (-> env
                                                               (ta-utils/ctx :statement)
                                                               (assoc :ns (ns-name *ns*)))
                                                         (dissoc opts :expected))))
                                       statements)
                 ret-expr (if (some-> stop-analysis deref)
                            (unanalyzed-expr ret)
                            ;; NB: in TAJ 0.3.0 :ns doesn't do anything.
                            ;; later versions rebind *ns*.
                            (analyze+eval ret (assoc env :ns (ns-name *ns*)) opts))]
             (-> {:op         :do
                  :top-level  true
                  :form       mform
                  :statements statements-expr
                  :ret        ret-expr
                  :children   [:statements :ret]
                  :env        env
                  :result     (:result ret-expr)
                  ;; could be nil if ret is unanalyzed
                  u/expr-type (u/expr-type ret-expr)
                  :raw-forms  raw-forms}
               source-info/source-info))
           (merge (if (some-> stop-analysis deref)
                    (unanalyzed-expr mform)
                    ;; rebinds *ns* during analysis
                    ;; FIXME unclear which map needs to have *ns*, especially post TAJ 0.3.0
                    (eval-fn (p/p :analyze-clj/analyze-no-eval
                               (taj/analyze mform (assoc env :ns (ns-name *ns*))
                                            (-> opts 
                                                (dissoc :bindings-atom)
                                                (assoc-in [:bindings #'*ns*] *ns*))))
                             opts))
                  {:raw-forms raw-forms}))))))

;; reflect-validated from eastwood
;========================
(T/ann ^:no-check reflect-validated [(T/Map T/Any T/Any) :-> T/Any])
(defmulti reflect-validated 
  {:pass-info {:walk :any :depends #{#'validate/validate}}}
  :op)

(T/ann ^:no-check arg-type-str [(t/Seqable (t/U nil Class)) :-> t/Str])
(defn arg-type-str [arg-types]
  (str/join ", "
            (map #(if (nil? %) "nil" (.getName ^Class %)) arg-types)))

(defn get-ctor [ast]
  (let [cls (:val (:class ast))
        arg-type-vec (mapv :tag (:args ast))
        arg-type-arr (into-array Class arg-type-vec)]
;;    (println (format "dbgx: get-ctor cls=%s arg-types=%s"
;;                     cls (arg-type-str arg-type-vec)))
    (try
      (.getConstructor ^Class cls arg-type-arr)
      (catch NoSuchMethodException e
        (try
          (.getDeclaredConstructor ^Class cls arg-type-arr)
          (catch NoSuchMethodException e
            {:class cls, :arg-types arg-type-vec}))))))

(defn get-field [ast]
  (let [cls (:class ast)
        fld-name (name (:field ast))]
    (try
      (.getField ^Class cls fld-name)
      (catch NoSuchFieldException e
        (try
          (.getDeclaredField ^Class cls fld-name)
          (catch NoSuchFieldException e
            {:class cls, :field-name fld-name}))))))

(defn get-method [ast]
  (let [cls (:class ast)
        method-name (name (:method ast))
        arg-type-vec (mapv :tag (:args ast))
        arg-type-arr (into-array Class arg-type-vec)]
;;    (println (format "dbgx: get-method cls=%s method=%s arg-types=%s"
;;                     cls method-name (arg-type-str arg-type-vec)))
    (when (some nil? arg-type-vec)
      (println (format "Error: Bad arg-type nil for method named %s for class %s, full arg type list (%s).  ast pprinted below for debugging tools.analyzer:"
                       method-name
                       (.getName ^Class cls)
                       (arg-type-str arg-type-vec)))
      #_(util/pprint-ast-node ast))
    (try
      (.getMethod ^Class cls method-name arg-type-arr)
      (catch NoSuchMethodException e
        (try
          (.getDeclaredMethod ^Class cls method-name arg-type-arr)
          (catch NoSuchMethodException e
            {:class cls, :method-name method-name,
             :arg-types arg-type-vec}))))))


(defn void-method? [^java.lang.reflect.Method m]
  (let [ret-val (.getGenericReturnType m)]
    (= ret-val Void/TYPE)))

(defmethod reflect-validated :default [ast] ast)

(defmethod reflect-validated :new [ast]
  (if (:validated? ast)
    (assoc ast :reflected-ctor (@#'reflect/constructor->map (get-ctor ast)))
    ast))

(defmethod reflect-validated :instance-field [ast]
  (assoc ast :reflected-field (@#'reflect/field->map (get-field ast))))

(defmethod reflect-validated :instance-call [ast]
  (if (:validated? ast)
    (assoc ast :reflected-method (@#'reflect/method->map (get-method ast)))
    ast))

(defmethod reflect-validated :static-field [ast]
  (assoc ast :reflected-field (@#'reflect/field->map (get-field ast))))

(defmethod reflect-validated :static-call [ast]
  (if (:validated? ast)
    (assoc ast :reflected-method (@#'reflect/method->map (get-method ast)))
    ast))
;========================

(def typed-passes
  (-> jana2/default-passes
      ; this pass is manually inserted as we check
      ; in functions like clojure.core.typed.check.utils/FieldExpr->Field
      ;(conj #'reflect-validated)
      (disj
        ;; trim conflicts with current approach of special typed forms implemented
        ;; as `do` nodes with constants.
        ;; also, seems to rewrite (let [] ...) as (do ...), which conflicts
        ;; with the Gilardi scenario, especially in the expansion of (binding [...] ...)
        #'trim/trim
        ;; We either rewrite reflective calls or throw a type error.
        ;; Reflective calls in untyped (tc-ignore'd) code will be signalled
        ;; by the Clojure compiler once evaluated.
        #'warn-reflect/warn-on-reflection)))

(def typed-schedule
  (passes/schedule typed-passes #_{:debug? true}))

(comment
  (clojure.pprint/pprint
    (passes/schedule typed-passes {:debug? true}))
  )

(defn run-passes [ast]
  (typed-schedule ast))

;; (All [x ...] [-> '{(Var x) x ...})])
(defn thread-bindings []
  (t/tc-ignore
    {#'ana2/macroexpand-1 macroexpand-1
     ;#'jana2/run-passes run-passes
     }))

;; bindings is an atom that records any side effects during macroexpansion. Useful
;; for nREPL middleware.
(defn analyze1
  ([form] (analyze1 form (taj/empty-env) {}))
  ([form env] (analyze1 form env {}))
  ([form env {:keys [bindings-atom] :as opts}]
   {:pre [((some-fn nil? con/atom?) bindings-atom)]}
   (u/trace "Analyze1 form" *file* form)
   (let [old-bindings (or (some-> bindings-atom deref) {})]
     (with-bindings old-bindings
       ;(prn "analyze1 namespace" *ns*)
       (let [ana (jana2/analyze+eval 
                   form (or env (taj/empty-env))
                   (->
                     (merge-with merge opts 
                                 {:bindings (thread-bindings)
                                  :special-form? special-form?})
                     (assoc
                       ;; if this is a typed special form like an ann-form, don't treat like
                       ;; a top-level do.
                       :additional-gilardi-condition 
                       (fn [mform]
                         (not (special-form? mform)))
                       ;; propagate inner types to outer `do`
                       :annotate-do
                       (fn [a _ ret]
                         ;; could be nil if ret is unanalyzed
                         (assoc a u/expr-type (u/expr-type ret)))
                       ;; don't propagate expected type to `do` statements
                       :statement-opts-fn
                       (fn [opts]
                         (dissoc opts :expected))
                       :analyze-env-fn
                       (fn [env]
                         (assoc env :ns (ns-name *ns*)))
                       :analyze-opts-fn
                       (fn [opts]
                         (-> opts 
                             (dissoc :bindings-atom)
                             (assoc-in [:bindings #'*ns*] *ns*))))))]
         ;; only record vars that were already bound
         (when bindings-atom
           (reset! bindings-atom (select-keys (get-thread-bindings) (keys old-bindings))))
         ana)))))

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (binding [*ns* (or (find-ns nsym)
                     *ns*)]
    (analyze1 form)))

(def reread-with-tr (comp tr/read readers/indexing-push-back-reader print-str))

;(defn ast-for-str
;  "Returns an AST node for the string, using tools.reader."
;  [form-str]
;  (analyze1 (-> form-str readers/indexing-push-back-reader tr/read) (taj/empty-env)))

(defn ast-for-form
  "Returns an AST node for the form"
  ([form] (ast-for-form form {}))
  ([form opt]
   (analyze1 form (taj/empty-env) opt)))

(t/ann ^:no-check ast-for-file [t/Str -> t/Any])
(defn ast-for-file
  "Returns a vector of AST nodes contained
  in the given file"
  [p]
  {:pre [(string? p)]}
  (let [pres (io/resource p)
        _ (when-not (instance? java.net.URL pres)
            (err/int-error (str "Cannot find file: " p)))
        file (-> pres io/reader slurp)
        reader (readers/indexing-push-back-reader file 1 p)
        eof  (reify)
        reader-opts (if (.endsWith ^String p ".cljc")
                      {:eof eof :read-cond :allow
                       :features #{(impl/impl-case
                                     :clojure :clj
                                     :cljs (assert nil "Not allowed"))}}
                      {:eof eof})
        asts (binding [*ns* *ns*
                       *file* p]
               (loop [asts []]
                 (let [form (tr/read reader-opts reader)]
                   (if (not= eof form)
                     (let [a (analyze1 form (taj/empty-env)
                                       {:eval-fn eval-ast})]
                       (recur (conj asts a)))
                     asts))))]
    asts))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [((some-fn symbol? #(instance? clojure.lang.Namespace %)) 
          nsym)]
   :post [(vector? %)]}
  (u/p :analyze/ast-for-ns
       ;(prn "ast-for-ns" nsym)
   (let [nsym (or (when (instance? clojure.lang.Namespace nsym)
                    (ns-name nsym))
                  ; don't call ns-name on symbols in case the namespace
                  ; doesn't exist yet
                  nsym)
         _ (assert (symbol? nsym))
         cache vs/*analyze-ns-cache*]
     (if (and cache (cache/has? cache nsym))
       (-> cache
           (cache/hit nsym)
           (cache/lookup nsym))
       ;copied basic approach from tools.emitter.jvm
       (let [p (coerce/ns->file nsym)
             asts (ast-for-file p)]
         (when cache
           (cache/miss cache nsym asts))
         asts)))))

; eval might already be monkey-patched, eval' avoids infinite looping
(defn eval' [frm]
  (. clojure.lang.Compiler (eval frm)))

(defn eval-ast [ast opts]
  ;; based on jvm/analyze+eval
  (let [; FIXME don't allow mixing of runtime inference and custom expansions,
        ; since we want to evaluate the modified AST in runtime inference.
        frm (if custom-expansions?
              (:original-form opts)
              (emit-form/emit-form ast))
        ;_ (prn "form" frm)
        #_#_
        _ (binding [;*print-meta* true
                    ;*print-dup* true
                    ;*print-length* 6
                    ;*print-level* 6
                    ]
            (prn "form")
            (pp/pprint frm))
        ;; TODO support `handle-evaluation-exception`
        result (eval' frm)]  ;; eval the emitted form rather than directly the form to avoid double macroexpansion
    (merge ast {:result result})))

