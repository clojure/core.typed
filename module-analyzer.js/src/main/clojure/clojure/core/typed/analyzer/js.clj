;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from tools.analyzer.js
(ns clojure.core.typed.analyzer.js
  "Analyzer for clojurescript code, extends tools.analyzer with JS specific passes/forms"
  (:refer-clojure :exclude [macroexpand-1 var? ns-resolve])
  (:require [clojure.core.typed.analyzer.common :as ana]
            [clojure.tools.analyzer.utils :refer [ctx -source-info dissoc-env mmerge update-vals] :as u]
            [clojure.tools.analyzer.ast :refer [prewalk postwalk]]
            [clojure.tools.analyzer.env :as env]
            [clojure.core.typed.analyzer.passes :as passes]
            [clojure.tools.analyzer.passes.source-info :refer [source-info]]
            [clojure.tools.analyzer.passes.elide-meta :refer [elide-meta elides]]
            [clojure.core.typed.analyzer.passes.uniquify :as uniquify2]
            [clojure.core.typed.analyzer.passes.js.infer-tag :refer [infer-tag]]
            [clojure.core.typed.analyzer.passes.js.validate :refer [validate]]
            [clojure.core.typed.analyzer.js.utils
             :refer [desugar-ns-specs validate-ns-specs ns-resource ns->relpath res-path]]
            [cljs.env :as cljs-env]
            [cljs.js-deps :as deps]
            [clojure.core :as core]
            cljs.tagged-literals)
  (:import cljs.tagged_literals.JSValue))

(def specials
  "Set of the special forms for ClojureScript"
  (into ana/specials '#{ns deftype* defrecord* js* case*}))

(def ^:dynamic *cljs-ns* 'cljs.user)

(defonce core-env (atom {}))

(defn global-env []
  (atom (merge (and cljs-env/*compiler* @cljs-env/*compiler*)
               {:namespaces          (merge '{goog {:mappings {}, :js-namespace true, :ns goog}
                                              Math {:mappings {}, :js-namespace true, :ns Math}}
                                            @core-env)
                :js-dependency-index (deps/js-dependency-index {})})))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/statement
   :locals     {}
   :ns         *cljs-ns*})

(defn ns-resolve [ns sym]
  (let [ns (if (string? ns)
             (symbol ns)
             ns)
        sym (if (string? sym)
              (symbol sym)
              sym)]
    (and (find-ns ns)
         (core/ns-resolve ns sym))))

(defn maybe-macro [sym {:keys [ns]}]
  (let [var (if-let [sym-ns (namespace sym)]
              (if-let [full-ns (get-in (env/deref-env)
                                       [:namespaces ns :macro-aliases (symbol sym-ns)])]
                (ns-resolve full-ns (name sym))
                (ns-resolve sym-ns (name sym)))
              (get-in (env/deref-env) [:namespaces ns :macro-mappings sym]))]
    (when (:macro (meta var))
      var)))

(defn resolve-sym [sym env]
  (or (u/resolve-sym sym env)
      (get-in env [:locals sym])))

(defn dotted-symbol? [form env]
  (let [n (name form)
        ns (namespace form)
        idx (.indexOf n ".")
        sym (and (pos? idx)
                 (symbol ns (.substring n 0 idx)))]
    (and (not= idx -1)
         (not (resolve-sym form env))
         (not= sym form)
         (resolve-sym sym env))))

(defn desugar-symbol [form env]
  (let [ns (namespace form)
        n (name form)
        form (symbol ns n)]
    (if (dotted-symbol? form env)
      (let [idx (.indexOf n ".")
            sym (symbol ns (.substring n 0 idx))]
        (list '. sym (symbol (str "-" (.substring n (inc idx) (count n))))))

      form)))

(defn desugar-host-expr [form env]
  (if (symbol? (first form))
    (let [[op & expr] form
          opname (name op)
          opns   (namespace op)]
      (cond

       ;; (.foo bar ..) -> (. bar foo ..)
       (= (first opname) \.)
       (let [[target & args] expr
             args (list* (symbol (subs opname 1)) args)]
         (list '. target (if (= 1 (count args))
                           (first args) args)))

       ;; (foo. ..) -> (new foo ..)
       (= (last opname) \.)
       (let [op-s (str op)]
         (list* 'new (symbol (subs op-s 0 (dec (count op-s)))) expr))

       ;; (var.foo ..) -> (. var foo ..)
       (dotted-symbol? op env)
       (let [idx (.indexOf opname ".")
             sym (symbol opns (.substring opname 0 idx))]
         (list '. sym (list* (symbol (.substring opname (inc idx) (count opname))) expr)))

       :else (list* op expr)))
    form))

(defn macroexpand-1 [form env]
  "If form represents a macro form returns its expansion, else returns form."
  (env/ensure (global-env)
    (if (seq? form)
      (let [op (first form)]
        (if (or (not (symbol? op))
                (specials op))
          form
          (if-let [clj-macro (and (not (-> env :locals (get op)))
                                  (maybe-macro op env))]
            (with-bindings (merge {#'*ns* (create-ns *cljs-ns*)}
                                  (when-not (thread-bound? #'*cljs-ns*)
                                    {#'*cljs-ns* *cljs-ns*}))
              (let [ret (apply clj-macro form env (rest form))] ; (m &form &env & args)
                (if (and (seq? ret)
                         (= 'js* (first ret)))
                  (vary-meta ret merge
                             (when (-> clj-macro meta :cljs.analyzer/numeric)
                               {:tag 'number}))
                  ret)))
            (with-meta (desugar-host-expr form env) (meta form)))))
      (with-meta (desugar-symbol form env) (meta form)))))

(defn create-var
  "Creates a var map for sym and returns it."
  [sym {:keys [ns]}]
  (with-meta {:op   :var
              :name sym
              :ns   ns}
    (meta sym)))

(defn var? [x]
  (= :var (:op x)))

(def ^:private ^:dynamic *deps-map* {:path [] :deps #{}})
(declare analyze-ns)

(defn ensure-loaded [ns {:keys [refer]}]
  (assert (not (contains? (:deps *deps-map*) ns))
          (str "Circular dependency detected :" (conj (:path *deps-map*) ns)))
  (binding [*deps-map* (-> *deps-map*
                         (update-in [:path] conj ns)
                         (update-in [:deps] conj ns))]
    (let [namespaces (-> (env/deref-env) :namespaces)]
      (or (and (get namespaces ns)
               (not (get-in namespaces [ns :js-namespace])))
          (and (get-in (env/deref-env) [:js-dependency-index (name ns)])
               (swap! env/*env* update-in [:namespaces ns] merge
                      {:ns           ns
                       :js-namespace true})
               (swap! env/*env* update-in [:namespaces ns :mappings] merge
                      (reduce (fn [m k] (assoc m k {:op   :js-var
                                                   :name k
                                                   :ns   ns}))
                              {} refer)))
          (analyze-ns ns)))))

(defn core-macros []
  (reduce-kv (fn [m k v]
               (if (:macro (meta v))
                 (assoc m k v)
                 m))
             {} (ns-interns 'clojure.tools.analyzer.js.cljs.core)))

(defn populate-env
  [{:keys [import require require-macros refer-clojure]} ns-name env]
  (let [imports (reduce-kv (fn [m prefix suffixes]
                             (merge m (into {} (mapv (fn [s] [s {:op   :js-var
                                                                :ns   prefix
                                                                :name s}]) suffixes)))) {} import)
        require-aliases (reduce (fn [m [ns {:keys [as]}]]
                                  (if as
                                    (assoc m as ns)
                                    m)) {} require)
        require-mappings (reduce (fn [m [ns {:keys [refer] :as spec}]]
                                   (ensure-loaded ns spec)
                                   (reduce #(assoc %1 %2 (get-in (env/deref-env)
                                                                 [:namespaces ns :mappings %2])) m refer))
                                 {} require)
        core-mappings (apply dissoc (get-in (env/deref-env) [:namespaces 'cljs.core :mappings]) (:exclude refer-clojure))
        macro-aliases (reduce (fn [m [ns {:keys [as]}]]
                                (if as
                                  (assoc m as ns)
                                  m)) {} require-macros)
        core-macro-mappings (apply dissoc (core-macros) (:exclude refer-clojure))
        macro-mappings (reduce (fn [m [ns {:keys [refer]}]]
                                 (core/require ns)
                                 (reduce #(let [m (ns-resolve ns (name %2))]
                                            (if (:macro (meta m))
                                              (assoc %1 %2 m)
                                              %1)) m refer))
                               {} require-macros)]

    (swap! env/*env* assoc-in [:namespaces ns-name]
           {:ns             ns-name
            :mappings       (merge core-mappings require-mappings imports)
            :aliases        require-aliases
            :macro-mappings (merge core-macro-mappings macro-mappings)
            :macro-aliases  macro-aliases})))

(def default-passes
  "Set of passes that will be run by default on the AST by #'run-passes"
  #{#'uniquify2/uniquify-locals

    #'source-info
    #'elide-meta

    #'validate
    #'infer-tag})

(def scheduled-default-passes
  (delay
    (passes/schedule default-passes)))

(comment
  (clojure.pprint/pprint
    (passes/schedule default-passes
                     {:debug? true}))
  )

(declare parse)

(defn analyze
  "Returns an AST for the form.

   Binds tools.analyzer/{macroexpand-1,create-var,parse} to
   tools.analyzer.js/{macroexpand-1,create-var,parse} and analyzes the form.

   If provided, opts should be a map of options to analyze, currently the only valid
   options are :bindings and :passes-opts.
   If provided, :bindings should be a map of Var->value pairs that will be merged into the
   default bindings for tools.analyzer, useful to provide custom extension points.
   If provided, :passes-opts should be a map of pass-name-kw->pass-config-map pairs that
   can be used to configure the behaviour of each pass.

   E.g.
   (analyze form env {:bindings  {#'ana/macroexpand-1 my-mexpand-1}})

   Calls `run-passes` on the AST."
  ([form] (analyze form (empty-env) {}))
  ([form env] (analyze form env {}))
  ([form env opts]
     (with-bindings (merge {#'ana/macroexpand-1 macroexpand-1
                            #'ana/create-var    create-var
                            #'ana/scheduled-passes    @scheduled-default-passes
                            #'ana/parse         parse
                            #'ana/var?          var?
                            #'elides            (-> elides
                                                  (update-in [:all] into #{:line :column :end-line :end-column :file :source})
                                                  (assoc-in [:fn] #{:cljs.analyzer/type :cljs.analyzer/protocol-impl :cljs.analyzer/protocol-inline}))}
                           (when-not (thread-bound? #'*cljs-ns*)
                             {#'*cljs-ns* *cljs-ns*})
                           (:bindings opts))
       (env/ensure (global-env)
         (swap! env/*env* mmerge {:passes-opts (:passes-opts opts)})
         (ana/run-passes (ana/unanalyzed form env))))))

; (U ':deftype ':defrecord) Any Config -> AST
(defn parse-type
  [op [_ name fields pmasks body :as form] {:keys [ns] :as env}]
  (let [fields-expr (mapv (fn [name]
                            {:env     env
                             :form    name
                             :name    name
                             :mutable (:mutable (meta name))
                             :local   :field
                             :op      :binding})
                          fields)
        protocols (-> name meta :protocols)

        _ (swap! env/*env* assoc-in [:namespaces ns :mappings name]
                 {:op        :var
                  :type      true
                  :name      name
                  :ns        ns
                  :fields    fields
                  :protocols protocols})

        body-expr (ana/unanalyzed
                    body
                    (assoc env :locals (zipmap fields (map dissoc-env fields-expr))))]

    {:op        op
     :env       env
     :form      form
     :name      name
     :fields    fields-expr
     :body      body-expr
     :pmasks    pmasks
     :protocols protocols
     :children  [:fields :body]}))

;; no ~{foo} support since cljs itself doesn't use it anywhere
(defn parse-js*
  [[_ jsform & args :as form] env]
  (when-not (string? jsform)
    (throw (ex-info "Invalid js* form"
                    (merge {:form form}
                           (-source-info form env)))))
  (let [segs  (loop [segs [] ^String s jsform]
                (let [idx (.indexOf s "~{")]
                  (if (= -1 idx)
                    (conj segs s)
                    (recur (conj segs (subs s 0 idx))
                           (subs s (inc (.indexOf s "}" idx)))))))
        exprs (mapv #(ana/unanalyzed % (ctx env :ctx/expr)) args)]
    (merge
     {:op       :js
      :env      env
      :form     form
      :segs     segs}
     (when args
       {:args     exprs
        :children [:args]}))))

(defn parse-case*
  [[_ test tests thens default :as form] env]
  (assert (symbol? test) "case* must switch on symbol")
  (assert (every? vector? tests) "case* tests must be grouped in vectors")
  (let [expr-env (ctx env :expr)
        test-expr (ana/unanalyzed test expr-env)
        nodes (mapv (fn [tests then]
                      {:op       :case-node
                       ;; no :form, this is a synthetic grouping node
                       :env      env
                       :tests    (mapv (fn [test]
                                         {:op       :case-test
                                          :form     test
                                          :env      expr-env
                                          :test     (ana/unanalyzed test expr-env)
                                          :children [:test]})
                                       tests)
                       :then     {:op       :case-then
                                  :form     test
                                  :env      env
                                  :then     (ana/unanalyzed then env)
                                  :children [:then]}
                       :children [:tests :then]})
                    tests thens)
        default-expr (ana/unanalyzed default env)]
    (assert (every? (fn [t] (and (= :const (-> t :test :op))
                           ((some-fn number? string?) (:form t))))
               (mapcat :tests nodes))
            "case* tests must be numbers or strings")
    {:op       :case
     :form     form
     :env      env
     :test     (assoc test-expr :case-test true)
     :nodes    nodes
     :default  default-expr
     :children [:test :nodes :default]}))

(defn parse-ns
  [[_ name & args :as form] env]
  (when-not (symbol? name)
    (throw (ex-info (str "Namespaces must be named by a symbol, had: "
                         (.getName ^Class (class name)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [[docstring & args] (if (string? (first args))
                             args
                             (cons nil args))
        [metadata & args]  (if (map? (first args))
                             args
                             (cons {} args))
        name (vary-meta name merge metadata)
        ns-opts (doto (desugar-ns-specs args form env)
                  (validate-ns-specs form env)
                  (populate-env name env))]
    (set! *cljs-ns* name)
    (merge
     {:op      :ns
      :env     env
      :form    form
      :name    name
      :depends (set (keys (:require ns-opts)))}
     (when docstring
       {:doc docstring})
     (when metadata
       {:meta metadata}))))

(defn parse-def
  [[_ sym & rest :as form] env]
  (let [ks #{:ns :name :doc :arglists :file :line :column}
        meta (meta sym)
        m (merge {}
                 (update-vals (select-keys meta ks) (fn [x] (list 'quote x)))
                 (when (:test meta)
                   {:test `(.-cljs$lang$test ~sym)}))]
    (ana/analyze-form (with-meta `(def ~(with-meta sym m) ~@rest) (meta form)) env)))

;; can it be :literal ?
(defn parse-js-value
  [form env]
  (let [val (.val ^JSValue form)
        items-env (ctx env :expr)]
    (if (map? val)
      ;; keys should always be symbols/kewords, do we really need to analyze them?
      {:op       :js-object
       :env      env
       :keys     (mapv (ana/unanalyzed-in-env items-env) (keys val))
       :vals     (mapv (ana/unanalyzed-in-env items-env) (vals val))
       :form     form
       :children [:keys :vals]}
      {:op       :js-array
       :env      env
       :items    (mapv (ana/unanalyzed-in-env items-env) val)
       :form     form
       :children [:items]})))

(defn parse
  "Extension to clojure.core.typed.analyzer/-parse for JS special forms"
  [form env]
  (cond
    (instance? JSValue form) (parse-js-value form env)
    :else
    ((case (first form)
       deftype*   #(parse-type :deftype %1 %2)
       defrecord* #(parse-type :defrecord %1 %2)
       case*      parse-case*
       ns         parse-ns
       def        parse-def
       js*        parse-js*
       #_:else    ana/-parse)
     form env)))
