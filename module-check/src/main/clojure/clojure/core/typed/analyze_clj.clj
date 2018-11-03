;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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
            [clojure.core.typed.analyzer2.pre-analyze :as pre]
            [clojure.core.typed.analyzer2.jvm.pre-analyze :as jpre]
            [clojure.core.typed.analyzer2.passes.beta-reduce :as beta-reduce]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.tools.analyzer.passes.jvm.validate :as validate]
            [clojure.java.io :as io]
            [clojure.reflect :as reflect]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.core.typed :as T]
            [clojure.core.typed :as t]
            [clojure.core.cache :as cache]
            [clojure.core.typed.special-form :as spec]
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
       ;(prn "ns form" &form)
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

   #'clojure.core/for
   (fn [&form &env seq-exprs body-expr]
     (@#'T/for &form &env seq-exprs body-expr))}))

(def ^:dynamic *analyze-env* nil)

(defn custom-expansion-opts []
  (let [analyze (fn [form & [env]]
                  (#'ana2/run-passes (pre/pre-analyze-form form (or env *analyze-env*))))]
    {:internal-error (fn [s & [opts]]
                       ;; TODO opts (line numbers, blame form etc.)
                       (let [;; can't access check.utils ns from here (circular deps)
                             #_#_opts (update opts :expected cu/maybe-map->TCResult)]
                         (apply err/int-error s (apply concat opts))))
     :splice-seqable-form (fn [form & [env]]
                            (some->> (beta-reduce/splice-seqable-expr (analyze form (or env *analyze-env*)))
                                     (mapv (fn [e]
                                             (let [form (emit-form/emit-form (:expr e))]
                                               (-> e
                                                   (dissoc :expr)
                                                   (assoc :form form)))))))
     :analyze-env *analyze-env*
     :analyze analyze
     :emit-form emit-form/emit-form}))

(T/ann ^:no-check typed-macro-lookup [T/Any :-> T/Any])
(defn typed-macro-lookup [var]
  {:post [(ifn? %)]}
  (or (when vs/*custom-expansions*
        (let [vsym (coerce/var->symbol var)]
          (when (expand/custom-expansion? vsym)
            (fn [form locals & _args_]
              (expand/expand-macro form 
                                   (merge (custom-expansion-opts)
                                          {:vsym vsym
                                           :locals locals}))))))
      (get *typed-macros* var)
      var))

;; copied from tools.analyzer.jvm to insert `*typed-macros*`
(T/ann ^:no-check macroexpand-1 
       (T/IFn [T/Any -> T/Any] 
              [T/Any (T/Map T/Any T/Any) -> T/Any]))
(defn macroexpand-1
  "If form represents a macro form or an inlinable function, returns its expansion,
   else returns form."
  ([form] (macroexpand-1 form (taj/empty-env)))
  ([form env]
   ;(prn "macroexpand-1" form)
   (binding [*analyze-env* env]
    (ta-env/ensure (taj/global-env)
      (cond
        (seq? form)
        (let [[op & args] form]
          (if (taj/specials op)
            form
            (let [v (ana2/resolve-sym op env)
                  m (meta v)
                  local? (-> env :locals (get op))
                  macro? (and (not local?) (:macro m)) ;; locals shadow macros
                  inline-arities-f (:inline-arities m)
                  ;; disable :inline with custom expansions to avoid arity errors
                  ;; in symbolic execution.
                  inline? (if vs/*custom-expansions*
                            (when (and (not local?) (var? v))
                              (let [vsym (coerce/var->symbol v)]
                                (when (expand/custom-inline? vsym)
                                  (fn [& _args_]
                                    (expand/expand-inline form
                                                          (merge (custom-expansion-opts)
                                                                 {:vsym vsym}))))))
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
        form)))))

(T/ann ^:no-check special-form? [T/Any :-> T/Any])
(defn special-form? [mform]
  (and (seq? mform)
       (= 'do (first mform))
       (or (= (second mform) spec/special-form)
           (= (second mform) ::T/special-collect))))

(declare eval-ast)
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

(declare scheduled-passes-for-custom-expansions)

;; (All [x ...] [-> '{(Var x) x ...})])
(defn thread-bindings [& [opts]]
  (t/tc-ignore
    {Compiler/LOADER     (clojure.lang.RT/makeClassLoader)
     #'ana2/macroexpand-1 macroexpand-1
     #'ana2/scheduled-passes (if vs/*custom-expansions*
                               @scheduled-passes-for-custom-expansions
                               jana2/scheduled-default-passes)
     #'pre/pre-parse      jpre/pre-parse
     #'ana2/var?          var?
     #'ana2/create-var    taj/create-var
     #'ana2/resolve-ns    jana2/resolve-ns
     #'ana2/resolve-sym   jana2/resolve-sym
     #'*ns*               (the-ns (or (-> opts :env :ns)
                                      *ns*))}))

(defn will-custom-expand? [form env]
  (boolean
    (when vs/*custom-expansions*
      (when (seq? form)
        (let [[op & args] form]
          (when-not (taj/specials op)
            (let [v (ana2/resolve-sym op env)
                  m (meta v)
                  local? (-> env :locals (get op))
                  macro? (and (not local?) (:macro m)) ;; locals shadow macros
                  vsym (when (var? v)
                         (coerce/var->symbol v))]
              (or (when (and (not local?) vsym)
                    (expand/custom-inline? vsym))
                  (when (and macro? vsym)
                    (expand/custom-expansion? vsym))))))))))

(comment
  (clojure.pprint/pprint
    (jana2/schedule (conj jana2/default-passes
                          #'beta-reduce/push-invoke
                          )
                    {:debug? true}))
  )

(def scheduled-passes-for-custom-expansions
  (delay
    (jana2/schedule (conj jana2/default-passes
                          ;#'beta-reduce/push-invoke
                          ))))

;; bindings is an atom that records any side effects during macroexpansion. Useful
;; for nREPL middleware.
(defn analyze1
  ([form] (analyze1 form (taj/empty-env) {}))
  ([form env] (analyze1 form env {}))
  ([form env {:keys [bindings-atom analyze-bindings-fn] :as opts}]
   {:pre [((some-fn nil? con/atom?) bindings-atom)
          ((some-fn nil? ifn?) analyze-bindings-fn)]}
   (u/trace "Analyze1 form" *file* form)
   (let [old-bindings (or (some-> bindings-atom deref) {})
         analyze-fn (fn [form env opts]
                      (let [env (assoc env :ns (ns-name *ns*))
                            opts (-> opts
                                     (dissoc :bindings-atom)
                                     (assoc-in [:bindings #'*ns*] *ns*))]
                        (jana2/analyze form env opts)))]
     (with-bindings old-bindings
       ;(prn "analyze1 namespace" *ns*)
       (let [ana (jana2/analyze+eval 
                   form (or env (taj/empty-env))
                   (->
                     (merge-with merge opts 
                                 {:bindings (if analyze-bindings-fn
                                              (analyze-bindings-fn)
                                              (thread-bindings))
                                  :special-form? special-form?})
                     (assoc
                       ;; if this is a typed special form like an ann-form, don't treat like
                       ;; a top-level do.
                       :additional-gilardi-condition 
                       (fn [mform env]
                         (not (special-form? mform)))
                       :stop-gildardi-check 
                       ;; cut off custom expansions to preserve :original-form's
                       (fn [mform env]
                         (will-custom-expand? mform env))
                       ;; propagate inner types to outer `do`
                       :annotate-do
                       (fn [a _ ret]
                         ;; could be nil if ret is unanalyzed
                         (assoc a u/expr-type (u/expr-type ret)))
                       ;; don't propagate expected type to `do` statements
                       :statement-opts-fn
                       (fn [opts]
                         (dissoc opts :expected))
                       :analyze-fn analyze-fn)))]
         ;; only record vars that were already bound
         (when bindings-atom
           (reset! bindings-atom (select-keys (get-thread-bindings) (keys old-bindings))))
         ana)))))

(defn ast-for-form
  "Returns an AST node for the form"
  ([form] (ast-for-form form {}))
  ([form opt]
   (analyze1 form (taj/empty-env) opt)))

; eval might already be monkey-patched, eval' avoids infinite looping
(defn eval' [frm]
  (. clojure.lang.Compiler (eval frm)))

(defn eval-ast [ast opts]
  ;; based on jvm/analyze+eval
  (let [; FIXME don't allow mixing of runtime inference and custom expansions,
        ; since we want to evaluate the modified AST in runtime inference.
        frm (if vs/*custom-expansions*
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

