;; adapted from tools.analyzer.jvm
(ns clojure.core.typed.analyzer2.jvm.pre-analyze
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer.utils :as u]
            [clojure.tools.analyzer.jvm.utils :as ju]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer :as ta]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.tools.analyzer.passes :as passes]
            [clojure.tools.analyzer.passes.trim :as trim]
            [clojure.tools.analyzer.passes.jvm.box :as box]
            [clojure.tools.analyzer.passes.jvm.warn-on-reflection :as warn-on-reflection]
            [clojure.tools.analyzer.passes.warn-earmuff :as warn-earmuff]
            [clojure.tools.analyzer.passes.uniquify :as uniquify]
            [clojure.tools.analyzer.passes.add-binding-atom :as add-binding-atom]
            [clojure.tools.analyzer.passes.jvm.fix-case-test :as fix-case-test]
            [clojure.tools.analyzer.passes.jvm.infer-tag :as infer-tag]
            [clojure.tools.analyzer.passes.jvm.annotate-tag :as annotate-tag]
            [clojure.tools.analyzer.passes.jvm.annotate-host-info :as annotate-host-info]
            [clojure.tools.analyzer.passes.jvm.analyze-host-expr :as analyze-host-expr]
            [clojure.tools.analyzer.passes.jvm.validate :as validate]
            [clojure.tools.analyzer.passes.jvm.validate-loop-locals :as validate-loop-locals]
            [clojure.tools.analyzer.passes.jvm.validate-recur :as validate-recur]
            [clojure.tools.analyzer.passes.elide-meta :as elide-meta]
            [clojure.tools.analyzer.passes.source-info :as source-info]
            [clojure.tools.analyzer.passes.jvm.constant-lifter :as constant-lift]
            [clojure.tools.analyzer.passes.jvm.classify-invoke :as classify-invoke]
            [clojure.core.typed.analyzer2 :as ana]
            [clojure.core.typed.analyzer2.pre-analyze :as pre]
            [clojure.core.memoize :as memo])
  (:import (clojure.lang IObj RT Var)))

(defn pre-parse-monitor-enter
  [[_ target :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to monitor-enter, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  {:op       :monitor-enter
   :env      env
   :form     form
   :target   (pre/pre-analyze-child target (u/ctx env :ctx/expr))
   :children [:target]})

(defn pre-parse-monitor-exit
  [[_ target :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to monitor-exit, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  {:op       :monitor-exit
   :env      env
   :form     form
   :target   (pre/pre-analyze-child target (u/ctx env :ctx/expr))
   :children [:target]})

(defn pre-parse-import*
  [[_ class :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to import*, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  {:op    :import
   :env   env
   :form  form
   :class class})

(defn pre-analyze-method-impls
  [[method [this & params :as args] & body :as form] env]
  (when-let [error-msg (cond
                        (not (symbol? method))
                        (str "Method method must be a symbol, had: " (class method))
                        (not (vector? args))
                        (str "Parameter listing should be a vector, had: " (class args))
                        (not (first args))
                        (str "Must supply at least one argument for 'this' in: " method))]
    (throw (ex-info error-msg
                    (merge {:form     form
                            :in       (:this env)
                            :method   method
                            :args     args}
                           (u/-source-info form env)))))
  (let [meth        (cons (vec params) body) ;; this is an implicit arg
        this-expr   {:name  this
                     :env   env
                     :form  this
                     :op    :binding
                     :o-tag (:this env)
                     :tag   (:this env)
                     :local :this}
        env         (assoc-in (dissoc env :this) [:locals this] (u/dissoc-env this-expr))
        method-expr (pre/pre-analyze-fn-method meth env)]
    (assoc (dissoc method-expr :variadic?)
      :op       :method
      :form     form
      :this     this-expr
      :name     (symbol (name method))
      :children (into [:this] (:children method-expr)))))

;; HACK
(defn -deftype [cname class-name args interfaces]

  (doseq [arg [class-name cname]]
    (memo/memo-clear! ju/members* [arg])
    (memo/memo-clear! ju/members* [(str arg)]))

  (let [interfaces (mapv #(symbol (.getName ^Class %)) interfaces)]
    (eval (list 'let []
                (list 'deftype* cname class-name args :implements interfaces)
                (list 'import class-name))))
  
  (when-let [env env/*env*]
    (swap! env update-in [:namespaces (ns-name *ns*) :mappings]
           merge (select-keys (ns-map *ns*) [(symbol (name cname))]))))

(defn pre-parse-reify*
  [[_ interfaces & methods :as form] env]
  (let [interfaces (conj (disj (set (mapv ju/maybe-class interfaces)) Object)
                         IObj)
        name (gensym "reify__")
        class-name (symbol (str (namespace-munge *ns*) "$" name))
        menv (assoc env :this class-name)
        methods (mapv #(assoc (pre-analyze-method-impls % menv) :interfaces interfaces)
                      methods)]

    (-deftype name class-name [] interfaces)

    (pre/pre-wrapping-meta
     {:op         :reify
      :env        env
      :form       form
      :class-name class-name
      :methods    methods
      :interfaces interfaces
      :children   [:methods]})))

(defn parse-opts+methods [methods]
  (loop [opts {} methods methods]
    (if (keyword? (first methods))
      (recur (assoc opts (first methods) (second methods)) (nnext methods))
      [opts methods])))

(defn pre-parse-deftype*
  [[_ name class-name fields _ interfaces & methods :as form] env]
  (let [interfaces (disj (set (mapv ju/maybe-class interfaces)) Object)
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
               :locals  (zipmap fields (map u/dissoc-env fields-expr))
               :this    class-name)
        [opts methods] (parse-opts+methods methods)
        methods (mapv #(assoc (pre-analyze-method-impls % menv) :interfaces interfaces)
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

(defn pre-parse-case*
  [[_ expr shift mask default case-map switch-type test-type & [skip-check?] :as form] env]
  (let [[low high] ((juxt first last) (keys case-map)) ;;case-map is a sorted-map
        e (u/ctx env :ctx/expr)
        test-expr (pre/pre-analyze-child expr e)
        [tests thens] (reduce (fn [[te th] [min-hash [test then]]]
                                (let [test-expr (pre/pre-analyze-const test e)
                                      then-expr (pre/pre-analyze-child then env)]
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
        default-expr (pre/pre-analyze-child default env)]
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

(defn pre-parse
  "Extension to clojure.core.typed.analyzer2.pre-analyze/-pre-parse for JVM special forms"
  [form env]
  ((case (first form)
     monitor-enter        pre-parse-monitor-enter
     monitor-exit         pre-parse-monitor-exit
     clojure.core/import* pre-parse-import*
     reify*               pre-parse-reify*
     deftype*             pre-parse-deftype*
     case*                pre-parse-case*
     #_:else              pre/-pre-parse)
   form env))

;; should this be in an implementation agnostic ns?
(defn pre-analyze
  ""
  [ast]
  {:post [(not= :unanalyzed (:op %))]}
  (case (:op ast)
    :unanalyzed (let [{:keys [form env ::pre/config]} ast
                      ast (-> form
                              (pre/pre-analyze-form env)
                              ;TODO rename to ::pre/inherited
                              (assoc ::pre/config config))]
                    ast)
    ast))
