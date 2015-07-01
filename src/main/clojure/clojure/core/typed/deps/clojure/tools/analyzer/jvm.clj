;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.jvm
  "Analyzer for clojure code, extends tools.analyzer with JVM specific passes/forms"
  (:refer-clojure :exclude [macroexpand-1 macroexpand])
  (:require [clojure.core.typed.deps.clojure.tools.analyzer
             :as ana
             :refer [analyze analyze-in-env wrapping-meta analyze-fn-method]
             :rename {analyze -analyze}]

            [clojure.core.typed.deps.clojure.tools.analyzer
             [utils :refer [ctx resolve-var -source-info resolve-ns obj? dissoc-env]]
             [ast :refer [walk prewalk postwalk]]
             [env :as env :refer [*env*]]]

            [clojure.core.typed.deps.clojure.tools.analyzer.jvm.utils :refer :all :exclude [box specials]]

            [clojure.core.typed.deps.clojure.tools.analyzer.passes
             [source-info :refer [source-info]]
             [cleanup :refer [cleanup]]
             [elide-meta :refer [elide-meta elides]]
             [warn-earmuff :refer [warn-earmuff]]
             [collect :refer [collect collect-closed-overs]]
             [add-binding-atom :refer [add-binding-atom]]
             [uniquify :refer [uniquify-locals]]]

            [clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm
             [box :refer [box]]
             [constant-lifter :refer [constant-lift]]
             [annotate-branch :refer [annotate-branch]]
             [annotate-loops :refer [annotate-loops]]
             [annotate-methods :refer [annotate-methods]]
             [annotate-class-id :refer [annotate-class-id]]
             [annotate-internal-name :refer [annotate-internal-name]]
             [fix-case-test :refer [fix-case-test]]
             [clear-locals :refer [clear-locals]]
             [classify-invoke :refer [classify-invoke]]
             [validate :refer [validate]]
             [infer-tag :refer [infer-tag ensure-tag]]
             [annotate-tag :refer [annotate-tag]]
             [validate-loop-locals :refer [validate-loop-locals]]
             [analyze-host-expr :refer [analyze-host-expr]]
             [warn-on-reflection :refer [warn-on-reflection]]
             [emit-form :refer [emit-form]]]

            [clojure.java.io :as io]
            [clojure.core.typed.deps.clojure.tools.reader :as reader]
            [clojure.core.typed.deps.clojure.tools.reader.reader-types :as readers]

            [clojure.core.typed.deps.clojure.core.memoize :refer [memo-clear!]])
  (:import clojure.lang.IObj))

(def specials
  "Set of the special forms for clojure in the JVM"
  (into ana/specials
        '#{var monitor-enter monitor-exit clojure.core/import* reify* deftype* case*}))

(defmulti parse
  "Extension to tools.analyzer/-parse for JVM special forms"
  (fn [[op & rest] env] op))

(defmethod parse :default
  [form env]
  (ana/-parse form env))

(defn build-ns-map []
  (into {} (mapv #(vector (ns-name %)
                          {:mappings (ns-map %)
                           :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                                                {} (ns-aliases %))
                           :ns       (ns-name %)})
                 (all-ns))))

(defn update-ns-map! []
  (swap! *env* assoc-in [:namespaces] (build-ns-map)))

(defn global-env []
  (atom {:namespaces (build-ns-map)}))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         (ns-name *ns*)})

(defn desugar-host-expr [form env]
  (cond
   (symbol? form)
   (let [target (maybe-class (namespace form))]
     (if (and target (not (resolve-ns (symbol (namespace form)) env)))       ;; Class/field
       (with-meta (list '. target (symbol (str "-" (symbol (name form))))) ;; transform to (. Class -field)
         (meta form))
       form))

   (seq? form)
   (let [[op & expr] form]
     (if (symbol? op)
       (let [opname (name op)
             opns   (namespace op)]
         (cond

          (.startsWith opname ".") ; (.foo bar ..)
          (let [[target & args] expr
                target (if-let [target (and (not (get (:locals env) target))
                                            (maybe-class target))]
                         (with-meta (list 'clojure.core/identity target)
                           {:tag 'java.lang.Class})
                         target)
                args (list* (symbol (subs opname 1)) args)]
            (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) is
                                         (first args) args))  ;; a method call or a field access
              (meta form)))

          (and (maybe-class opns)
               (not (resolve-ns (symbol opns) env))) ; (class/field ..)
          (let [target (maybe-class opns)
                op (symbol opname)]
            (with-meta (list '. target (if (zero? (count expr))
                                         op
                                         (list* op expr)))
              (meta form)))

          (.endsWith opname ".") ;; (class. ..)
          (with-meta (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
            (meta form))

          :else form))
       form))

   :else form))

(defn macroexpand-1
  "If form represents a macro form or an inlineable function,
   returns its expansion, else returns form."
  ([form] (macroexpand-1 form (empty-env)))
  ([form env]
     (env/ensure (global-env)
       (if (seq? form)
         (let [[op & args] form]
           (if (specials op)
             form
             (let [v (resolve-var op env)
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
                (let [res (apply v form (:locals env) (rest form))] ; (m &form &env & args)
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge (meta form))
                    res))

                inline?
                (let [res (apply inline? args)]
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge
                               (and t {:tag t})
                               (meta form))
                    res))

                :else
                (desugar-host-expr form env)))))
         (desugar-host-expr form env)))))

(defn create-var
  "Creates a Var for sym and returns it.
   The Var gets interned in the env namespace."
  [sym {:keys [ns]}]
  (or (find-var (symbol (str ns) (name sym)))
      (intern ns (vary-meta sym merge
                            (let [{:keys [inline inline-arities]} (meta sym)]
                              (merge {}
                                     (when inline
                                       {:inline (eval inline)})
                                     (when inline-arities
                                       {:inline-arities (eval inline-arities)})))))))

(defmethod parse 'var
  [[_ var :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to var, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  (if-let [var (resolve-var var env)]
    {:op   :the-var
     :env  env
     :form form
     :var  var}
    (throw (ex-info (str "var not found: " var) {:var var}))))

(defmethod parse 'monitor-enter
  [[_ target :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to monitor-enter, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op       :monitor-enter
   :env      env
   :form     form
   :target   (-analyze target (ctx env :ctx/expr))
   :children [:target]})

(defmethod parse 'monitor-exit
  [[_ target :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to monitor-exit, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op       :monitor-exit
   :env      env
   :form     form
   :target   (-analyze target (ctx env :ctx/expr))
   :children [:target]})

(defmethod parse 'clojure.core/import*
  [[_ class :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to import*, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op    :import
   :env   env
   :form  form
   :class class})

(defn analyze-method-impls
  [[method [this & params :as args] & body :as form] env]
  (when-let [error-msg (cond
                        (not (symbol? method))
                        (str "Method method must be a symbol, had: " (class method))
                        (not (vector? args))
                        (str "Parameter listing should be a vector, had: " (class args))
                        (not (first args))
                        (str"Must supply at least one argument for 'this' in: " method))]
    (throw (ex-info error-msg
                    (merge {:form     form
                            :in       (:this env)
                            :method   method
                            :args     args}
                           (-source-info form env)))))
  (let [meth        (cons (vec params) body) ;; this is an implicit arg
        this-expr   {:name  this
                     :env   env
                     :form  this
                     :op    :binding
                     :o-tag (:this env)
                     :tag   (:this env)
                     :local :this}
        env         (assoc-in (dissoc env :this) [:locals this] (dissoc-env this-expr))
        method-expr (analyze-fn-method meth env)]
    (assoc (dissoc method-expr :variadic?)
      :op       :method
      :form     form
      :this     this-expr
      :name     (symbol (name method))
      :children (into [:this] (:children method-expr)))))

;; HACK
(defn -deftype [name class-name args interfaces]

  (doseq [arg [class-name (str class-name) name (str name)]
          f   [maybe-class members*]]
    (memo-clear! f [arg]))

  (let [interfaces (mapv #(symbol (.getName ^Class %)) interfaces)]
    (eval (list 'let []
                (list 'deftype* name class-name args :implements interfaces)
                (list 'import class-name)))))

(defmethod parse 'reify*
  [[_ interfaces & methods :as form] env]
  (let [interfaces (conj (disj (set (mapv maybe-class interfaces)) Object)
                         IObj)
        name (gensym "reify__")
        class-name (symbol (str (namespace-munge *ns*) "$" name))
        menv (assoc env :this class-name)
        methods (mapv #(assoc (analyze-method-impls % menv) :interfaces interfaces)
                      methods)]

    (-deftype name class-name [] interfaces)

    (wrapping-meta
     {:op         :reify
      :env        env
      :form       form
      :class-name class-name
      :methods    methods
      :interfaces interfaces
      :children   [:methods]})))

(defmethod parse 'deftype*
  [[_ name class-name fields _ interfaces & methods :as form] env]
  (let [interfaces (disj (set (mapv maybe-class interfaces)) Object)
        fields-expr (mapv (fn [name]
                            {:env     env
                             :form    name
                             :name    name
                             :mutable (let [m (meta name)]
                                        (or (and (:unsynchronized-mutable m)
                                                 :unsynchronized-mutable)
                                            (and (:volatile-mutable m)
                                                 :volatile-mutable)))
                             :local   :field
                             :op      :binding})
                          fields)
        menv (assoc env
               :context :ctx/expr
               :locals  (zipmap fields (map dissoc-env fields-expr))
               :this    class-name)
        methods (mapv #(assoc (analyze-method-impls % menv) :interfaces interfaces)
                      methods)]

    (-deftype name class-name fields interfaces)

    {:op         :deftype
     :env        env
     :form       form
     :name       name
     :class-name class-name ;; internal, don't use as a Class
     :fields     fields-expr
     :methods    methods
     :interfaces interfaces
     :children   [:fields :methods]}))

(defmethod parse 'case*
  [[_ expr shift mask default case-map switch-type test-type & [skip-check?] :as form] env]
  (let [[low high] ((juxt first last) (keys case-map)) ;;case-map is a sorted-map
        e (ctx env :ctx/expr)
        test-expr (-analyze expr e)
        [tests thens] (reduce (fn [[te th] [min-hash [test then]]]
                                (let [test-expr (ana/-analyze :const test e)
                                      then-expr (-analyze then env)]
                                  [(conj te {:op       :case-test
                                             :form     test
                                             :env      e
                                             :hash     min-hash
                                             :test     test-expr
                                             :children [:test]})
                                   (conj th {:op       :case-then
                                             :form     then
                                             :env      env
                                             :hash     min-hash
                                             :then     then-expr
                                             :children [:then]})]))
                              [[] []] case-map)
        default-expr (-analyze default env)]
    {:op          :case
     :form        form
     :env         env
     :test        (assoc test-expr :case-test true)
     :default     default-expr
     :tests       tests
     :thens       thens
     :shift       shift
     :mask        mask
     :low         low
     :high        high
     :switch-type switch-type
     :test-type   test-type
     :skip-check? skip-check?
     :children    [:test :tests :thens :default]}))


(defmethod parse 'catch
  [[_ etype ename & body :as form] env]
  (let [etype (if (= etype :default) Throwable etype)] ;; catch-all
    (ana/-parse `(catch ~etype ~ename ~@body) env)))

(defn ^:dynamic run-passes
  "Applies the following passes in the correct order to the AST:
   * uniquify
   * add-binding-atom
   * cleanup
   * source-info
   * elide-meta
   * warn-earmuff
   * collect
   * jvm.box
   * jvm.constant-lifter
   * jvm.annotate-branch
   * jvm.annotate-loops
   * jvm.annotate-class-id
   * jvm.annotate-internal-name
   * jvm.annotate-methods
   * jvm.fix-case-test
   * jvm.clear-locals
   * jvm.classify-invoke
   * jvm.validate
   * jvm.infer-tag
   * jvm.annotate-tag
   * jvm.validate-loop-locals
   * jvm.analyze-host-expr"
  [ast]
  (-> ast

    uniquify-locals
    add-binding-atom

    (prewalk (fn [ast]
               (-> ast
                 warn-earmuff
                 source-info
                 elide-meta
                 annotate-methods
                 fix-case-test
                 annotate-class-id
                 annotate-internal-name)))

    ((fn analyze [ast]
       (postwalk ast
                 (fn [ast]
                   (-> ast
                     annotate-tag
                     analyze-host-expr
                     infer-tag
                     validate
                     classify-invoke
                     constant-lift ;; needs to be run after validate so that :maybe-class is turned into a :const
                     (validate-loop-locals analyze))))))

    (prewalk (fn [ast]
               (-> ast
                 box
                 warn-on-reflection
                 annotate-loops  ;; needed for clear-locals to safely clear locals in a loop
                 annotate-branch ;; needed for clear-locals
                 ensure-tag)))

    ((collect {:what       #{:constants
                             :callsites}
               :where      #{:deftype :reify :fn}
               :top-level? false}))

    ;; needs to be run in a separate pass to avoid collecting
    ;; constants/callsites in :loop
    (collect-closed-overs {:what  #{:closed-overs}
                           :where #{:deftype :reify :fn :loop :try}
                           :top-level? false})

    ;; needs to be run after collect-closed-overs
    clear-locals))

(defn analyze
  "Returns an AST for the form that's compatible with what tools.emitter.jvm requires.

   Binds tools.analyzer/{macroexpand-1,create-var,parse} to
   tools.analyzer.jvm/{macroexpand-1,create-var,parse} and calls
   tools.analyzer/analyzer on form.

   If provided, opts should be a map of options to analyze, currently the only valid option
   is :bindings.
   If provided, :bindings should be a map of Var->value pairs that will be merged into the
   default bindings for tools.analyzer, useful to provide custom extension points.

   E.g.
   (analyze form env {:bindings  {#'ana/macroexpand-1 my-mexpand-1}})

   Calls `run-passes` on the AST."
  ([form] (analyze form (empty-env) {}))
  ([form env] (analyze form env {}))
  ([form env opts]
     (with-bindings (merge {clojure.lang.Compiler/LOADER (clojure.lang.RT/makeClassLoader)
                            #'ana/macroexpand-1          macroexpand-1
                            #'ana/create-var             create-var
                            #'ana/parse                  parse
                            #'ana/var?                   var?
                            #'elides                     (merge {:fn    #{:line :column :end-line :end-column :file :source}
                                                                 :reify #{:line :column :end-line :end-column :file :source}}
                                                                elides)}
                           (:bindings opts))
       (env/ensure (global-env)
         (run-passes (-analyze form env))))))

(deftype ExceptionThrown [e])

(defn butlast+last
  "Returns same value as (juxt butlast last), but slightly more
efficient since it only traverses the input sequence s once, not
twice."
  [s]
  (loop [butlast (transient [])
         s s]
    (if-let [xs (next s)]
      (recur (conj! butlast (first s)) xs)
      [(seq (persistent! butlast)) (first s)])))

(defn analyze+eval
  "Like analyze but evals the form after the analysis and attaches the
   returned value in the :result field of the AST node.
   If evaluating the form will cause an exception to be thrown, the exception
   will be caught and the :result field will hold an ExceptionThrown instance
   with the exception in the \"e\" field.

   Useful when analyzing whole files/namespaces."
  ([form] (analyze+eval form (empty-env) {}))
  ([form env] (analyze+eval form env {}))
  ([form env opts]
     (env/ensure (global-env)
       (update-ns-map!)
       (let [[mform raw-forms] (binding [ana/macroexpand-1 (get-in opts [:bindings #'ana/macroexpand-1] macroexpand-1)]
                                 (loop [form form raw-forms []]
                                   (let [mform (ana/macroexpand-1 form env)]
                                     (if (= mform form)
                                       [mform (seq raw-forms)]
                                       (recur mform (conj raw-forms form))))))]
         (if (and (seq? mform) (= 'do (first mform)) (next mform))
           ;; handle the Gilardi scenario
           (let [[statements ret] (butlast+last (rest mform))
                 statements-expr (mapv (fn [s] (analyze+eval s (-> env
                                                                   (ctx :statement)
                                                                   (assoc :ns (ns-name *ns*)))
                                                             opts))
                                       statements)
                 ret-expr (analyze+eval ret (assoc env :ns (ns-name *ns*)) opts)]
             (-> {:op         :do
                 :top-level  true
                 :form       mform
                 :statements statements-expr
                 :ret        ret-expr
                 :children   [:statements :ret]
                 :env        env
                 :result     (:result ret-expr)
                 :raw-forms  raw-forms}
               source-info))
           (let [a (analyze mform env opts)
                 frm (emit-form a)
                 result (try (eval frm) ;; eval the emitted form rather than directly the form to avoid double macroexpansion
                             (catch Exception e
                               (ExceptionThrown. e)))]
             (merge a
                    {:result    result
                     :raw-forms raw-forms})))))))

(defn analyze'
  "Like `analyze` but runs cleanup on the AST"
  ([form] (analyze' form (empty-env)))
  ([form env] (analyze' form env {}))
  ([form env opts]
     (prewalk (analyze form env opts) cleanup)))

(defn analyze+eval'
  "Like `analyze+eval` but runs cleanup on the AST"
  ([form] (analyze+eval' form (empty-env)))
  ([form env] (analyze+eval' form env {}))
  ([form env opts]
     (prewalk (analyze+eval form env opts) cleanup)))

(defn analyze-ns
  "Analyzes a whole namespace, returns a vector of the ASTs for all the
   top-level ASTs of that namespace.
   Evaluates all the forms."
  [ns]
  (env/ensure (global-env)
    (let [res (ns-resource ns)]
      (assert res (str "Can't find " ns " in classpath"))
      (let [filename (source-path res)
            path (res-path res)]
        (when-not (get-in *env* [::analyzed-clj path])
          (binding [*ns* *ns*]
            (with-open [rdr (io/reader res)]
              (let [pbr (readers/indexing-push-back-reader
                         (java.io.PushbackReader. rdr) 1 filename)
                    eof (Object.)
                    env (empty-env)]
                (loop []
                  (let [form (reader/read pbr nil eof)]
                    (when-not (identical? form eof)
                      (swap! *env* update-in [::analyzed-clj path]
                             (fnil conj [])
                             (analyze+eval form (assoc env :ns (ns-name *ns*))))
                      (recur))))))))
        (get-in @*env* [::analyzed-clj path])))))
