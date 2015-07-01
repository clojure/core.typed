(ns ^:skip-wiki clojure.core.typed.analyze-clj
  (:refer-clojure :exclude [macroexpand-1 get-method])
  (:require [clojure.core.typed.deps.clojure.tools.analyzer :as ta]
            [clojure.core.typed.deps.clojure.tools.analyzer.env :as ta-env]
            [clojure.core.typed.deps.clojure.tools.analyzer.jvm :as taj]
            [clojure.core.typed.deps.clojure.tools.analyzer.utils :as ta-utils]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes :as passes]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.source-info :as source-info]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.cleanup :as cleanup]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.trim :as trim]
            [clojure.core.typed.deps.clojure.tools.reader :as tr]
            [clojure.core.typed.deps.clojure.tools.reader.reader-types :as readers]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm.validate :as validate]
            [clojure.java.io :as io]
            [clojure.reflect :as reflect]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed :as T]
            [clojure.core.typed.deps.clojure.core.cache :as cache]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.errors :as err]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core :as core])
  (:import (clojure.core.typed.deps.clojure.tools.analyzer.jvm ExceptionThrown)))

(alter-meta! *ns* assoc :skip-wiki true)

(def typed-macros
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
           ]
       `(do
          ::T/special-collect
          ::core/ns
          {:form '~&form}
          (clojure.core/in-ns '~name)
          (with-loading-context
            ~@(when gen-class-call (list gen-class-call))
            ~@(when (and (not= name 'clojure.core) (not-any? #(= :refer-clojure (first %)) references))
                `((clojure.core/refer '~'clojure.core)))
            ~@(map process-reference references))
          (if (.equals '~name 'clojure.core) 
            nil
            (do (dosync (commute @#'clojure.core/*loaded-libs* (T/inst conj T/Symbol T/Any) '~name)) nil)))))
   })

;; copied from tools.analyze.jvm to insert `typed-macros`
(defn macroexpand-1
  "If form represents a macro form or an inlineable function,returns its expansion,
   else returns form."
  ([form] (macroexpand-1 form (taj/empty-env)))
  ([form env]
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
                  inline? (and (not local?)
                               (or (not inline-arities-f)
                                   (inline-arities-f (count args)))
                               (:inline m))
                  t (:tag m)]
              (cond

               macro?
               (let [res (apply (typed-macros v v) form (:locals env) (rest form))] ; (m &form &env & args)
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

(defn special-form? [mform]
  (and (seq? mform)
       (= 'do (first mform))
       (or (= (second mform) spec/special-form)
           (= (second mform) ::T/special-collect))))

(declare eval-ast)
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
   :eval-fn  Takes :eval-fn option that takes an option map and an AST and returns an 
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
                    (eval-fn opts (taj/analyze mform (assoc env :ns (ns-name *ns*))
                                               (-> opts 
                                                   (dissoc :bindings-atom)
                                                   (assoc-in [:bindings #'*ns*] *ns*)))))
                  {:raw-forms raw-forms}))))))

;; reflect-validated from eastwood
;========================
(defmulti reflect-validated 
  {:pass-info {:walk :any :depends #{#'validate/validate}}}
  :op)

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
  (-> taj/default-passes
      ;(conj #'reflect-validated)
      ;; conflicts with current approach of special typed forms implemented
      ;; as `do` nodes with constants.
      (disj #'trim/trim)))

(def typed-schedule
  (passes/schedule typed-passes #_{:debug? true}))

(defn run-passes [ast]
  (typed-schedule ast))

(defn thread-bindings []
  {#'ta/macroexpand-1 macroexpand-1
   #'taj/run-passes run-passes})

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
       (let [ana (analyze+eval form (or env (taj/empty-env))
                               (merge-with merge opts {:bindings (thread-bindings)}))]
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
        asts (binding [*ns* *ns*
                       *file* p]
               (loop [asts []]
                 (let [form (tr/read reader false eof)]
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

(defn eval-ast [opts ast]
  ;; based on jvm/analyze+eval
  ;(let [frm (emit-form/emit-form ast)
  ;      result (try (eval frm)  ;; eval the emitted form rather than directly the form to avoid double macroexpansion
  ;                  (catch Exception e
  ;                    (ExceptionThrown. e)))]
  ;  (merge ast {:result result})))
  (let [frm (emit-form/emit-form ast)
        ;_ (prn "form" frm)
        result (eval frm)]  ;; eval the emitted form rather than directly the form to avoid double macroexpansion
    (merge ast {:result result})))

