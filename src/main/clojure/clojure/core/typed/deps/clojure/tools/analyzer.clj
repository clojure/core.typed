;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer
  "Analyzer for clojure code, host agnostic.

   Entry point:
   * analyze

   Platform implementers must provide dynamic bindings for:
   * macroexpand-1
   * parse
   * create-var
   * var?

   Setting up the global env is also required, see clojure.core.typed.deps.clojure.tools.analyzer.env

   See clojure.core.typed.deps.clojure.tools.analyzer.core-test for an example on how to setup the analyzer."
  (:refer-clojure :exclude [macroexpand-1 macroexpand var? record?])
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer :all]
            [clojure.core.typed.deps.clojure.tools.analyzer.env :as env])
  (:import (clojure.lang Symbol IPersistentVector IPersistentMap IPersistentSet ISeq IType IRecord)))

(derive :ctx/return :ctx/expr)

(defmulti -analyze-form (fn [form _] (class form)))

(declare analyze-symbol
         analyze-vector
         analyze-map
         analyze-set
         analyze-seq
         analyze-const)

(def ^:dynamic analyze-form
  "Like analyze, but does not mark the form with :top-level true"
  -analyze-form)

(defmethod -analyze-form Symbol
  [form env]
  (analyze-symbol form env))

(defmethod -analyze-form IPersistentVector
  [form env]
  (analyze-vector form env))

(defmethod -analyze-form IPersistentMap
  [form env]
  (analyze-map form env))

(defmethod -analyze-form IPersistentSet
  [form env]
  (analyze-set form env))

(defmethod -analyze-form ISeq
  [form env]
  (if-let [form (seq form)]
    (analyze-seq form env)
    (analyze-const form env)))

(defmethod -analyze-form IType
  [form env]
  (analyze-const form env :type))

(prefer-method -analyze-form IType IPersistentMap)
(prefer-method -analyze-form IType IPersistentVector)
(prefer-method -analyze-form IType IPersistentSet)
(prefer-method -analyze-form IType ISeq)

(defmethod -analyze-form IRecord
  [form env]
  (analyze-const form env :record))

(prefer-method -analyze-form IRecord IPersistentMap)
(prefer-method -analyze-form IRecord IPersistentVector)
(prefer-method -analyze-form IRecord IPersistentSet)
(prefer-method -analyze-form IRecord ISeq)

(defmethod -analyze-form :default
  [form env]
  (analyze-const form env))

(defn analyze
  "Given a form to analyze and an environment, a map containing:
   * :locals     a map from binding symbol to AST of the binding value
   * :context    a keyword describing the form's context from the :ctx/* hierarchy.
    ** :ctx/expr      the form is an expression: its value is used
    ** :ctx/return    the form is an expression in return position, derives :ctx/expr
    ** :ctx/statement the value of the form is not used
   * :ns         a symbol representing the current namespace of the form to be
                 analyzed

   returns an AST for that form.

   Every node in the AST is a map that is *guaranteed* to have the following keys:
   * :op   a keyword describing the AST node
   * :form the form represented by the AST node
   * :env  the environment map of the AST node

   Additionaly if the AST node contains sub-nodes, it is guaranteed to have:
   * :children a vector of the keys of the AST node mapping to the sub-nodes,
               ordered, when that makes sense

   It is considered a node either the top-level node (marked with :top-level true)
   or a node that can be reached via :children; if a node contains a node-like
   map that is not reachable by :children, there's no guarantee that such a map
   will contain the guaranteed keys."

  [form env]
  (assoc (analyze-form form env) :top-level true))

(defn empty-env
  "Returns an empty env"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         'user})

(defn analyze-in-env
  "Takes an env map and returns a function that analyzes a form in that env"
  [env]
  (fn [form] (analyze-form form env)))

(def ^{:dynamic  true
       :arglists '([form env])
       :doc      "If form represents a macro form, returns its expansion,
                  else returns form."}
  macroexpand-1)

(def ^{:dynamic  true
       :arglists '([[op & args] env])
       :doc      "Multimethod that dispatches on op, should default to -parse"}
  parse)

(def ^{:dynamic  true
       :arglists '([sym env])
       :doc      "Creates a var for sym and returns it"}
  create-var)

(def ^{:dynamic  true
       :arglists '([obj])
       :doc      "Returns true if obj represent a var form as returned by create-var"}
  var?)

;; this node wraps non-quoted collections literals with metadata attached
;; to them, the metadata will be evaluated at run-time, not treated like a constant
(defn wrapping-meta
  [{:keys [form env] :as expr}]
  (let [meta (meta form)]
    (if (and (obj? form)
             (seq meta))
      {:op       :with-meta
       :env      env
       :form     form
       :meta     (analyze-form meta (ctx env :ctx/expr))
       :expr     (assoc-in expr [:env :context] :ctx/expr)
       :children [:meta :expr]}
      expr)))

(defn analyze-const
  [form env & [type]]
  (let [type (or type (classify form))]
    (merge
     {:op       :const
      :env      env
      :type     type
      :literal? true
      :val      form
      :form     form}
     (when-let [m (and (obj? form)
                       (not-empty (meta form)))]
       {:meta     (analyze-const m (ctx env :ctx/expr) :map) ;; metadata on a constant literal will not be evaluated at
        :children [:meta]}))))                               ;; runtime, this is also true for metadata on quoted collection literals

(defn analyze-vector
  [form env]
  (let [items-env (ctx env :ctx/expr)
        items (mapv (analyze-in-env items-env) form)]
    (wrapping-meta
     {:op       :vector
      :env      env
      :items    items
      :form     form
      :children [:items]})))

(defn analyze-map
  [form env]
  (let [kv-env (ctx env :ctx/expr)
        [keys vals] (reduce-kv (fn [[keys vals] k v]
                                 [(conj keys k) (conj vals v)])
                               [[] []] form)
        ks (mapv (analyze-in-env kv-env) keys)
        vs (mapv (analyze-in-env kv-env) vals)]
    (wrapping-meta
     {:op       :map
      :env      env
      :keys     ks
      :vals     vs
      :form     form
      :children [:keys :vals]})))

(defn analyze-set
  [form env]
  (let [items-env (ctx env :ctx/expr)
        items (mapv (analyze-in-env items-env) form)]
    (wrapping-meta
     {:op       :set
      :env      env
      :items    items
      :form     form
      :children [:items]})))

(def specials
  "Set of special forms common to every clojure variant"
  '#{do if new quote set! try var
     catch throw finally def .
     let* letfn* loop* recur fn*})

(defn macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer
   represents a macro form, then returns it."
  [form env]
  (loop [form form]
    (let [mform (macroexpand-1 form env)]
      (if (= mform form)
        mform
        (recur mform)))))

(defn analyze-symbol
  [sym env]
  (let [mform (macroexpand-1 sym env)] ;; t.a.j/macroexpand-1 macroexpands Class/Field into (. Class Field)
    (if (= mform sym)
      (merge (if-let [{:keys [mutable children] :as local-binding} (-> env :locals sym)] ;; locals shadow globals
               (merge (dissoc local-binding :init)                                      ;; avoids useless passes later
                      {:op          :local
                       :assignable? (boolean mutable)
                       :children    (vec (remove #{:init} children))})
               (if-let [var (let [v (resolve-sym sym env)]
                              (and (var? v) v))]
                 (let [m (meta var)]
                   {:op          :var
                    :assignable? (dynamic? var m) ;; we cannot statically determine if a Var is in a thread-local context
                    :var         var              ;; so checking whether it's dynamic or not is the most we can do
                    :meta        m})
                 (if-let [maybe-class (namespace sym)] ;; e.g. js/foo.bar or Long/MAX_VALUE
                   (let [maybe-class (symbol maybe-class)]
                     {:op    :maybe-host-form
                      :class maybe-class
                      :field (symbol (name sym))})
                   {:op    :maybe-class ;; e.g. java.lang.Integer or Long
                    :class mform})))
             {:env  env
              :form mform})
      (-> (analyze-form mform env)
        (update-in [:raw-forms] (fnil conj ()) sym)))))

(defn analyze-seq
  [form env]
  (let [op (first form)]
    (when (nil? op)
      (throw (ex-info "Can't call nil"
                      (merge {:form form}
                             (-source-info form env)))))
    (let [mform (macroexpand-1 form env)]
      (if (= form mform) ;; function/special-form invocation
        (parse mform env)
        (-> (analyze-form mform env)
          (update-in [:raw-forms] (fnil conj ())
                     (vary-meta form assoc ::resolved-op (resolve-sym op env))))))))

(defn parse-do
  [[_ & exprs :as form] env]
  (let [statements-env (ctx env :ctx/statement)
        [statements ret] (loop [statements [] [e & exprs] exprs]
                           (if (seq exprs)
                             (recur (conj statements e) exprs)
                             [statements e]))
        statements (mapv (analyze-in-env statements-env) statements)
        ret (analyze-form ret env)]
    {:op         :do
     :env        env
     :form       form
     :statements statements
     :ret        ret
     :children   [:statements :ret]}))

(defn parse-if
  [[_ test then else :as form] env]
  (let [formc (count form)]
    (when-not (or (= formc 3) (= formc 4))
      (throw (ex-info (str "Wrong number of args to if, had: " (dec (count form)))
                      (merge {:form form}
                             (-source-info form env))))))
  (let [test-expr (analyze-form test (ctx env :ctx/expr))
        then-expr (analyze-form then env)
        else-expr (analyze-form else env)]
    {:op       :if
     :form     form
     :env      env
     :test     test-expr
     :then     then-expr
     :else     else-expr
     :children [:test :then :else]}))

(defn parse-new
  [[_ class & args :as form] env]
  (when-not (>= (count form) 2)
    (throw (ex-info (str "Wrong number of args to new, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [args-env (ctx env :ctx/expr)
        args (mapv (analyze-in-env args-env) args)]
    {:op          :new
     :env         env
     :form        form
     :class       (analyze-form class (assoc env :locals {})) ;; avoid shadowing
     :args        args
     :children    [:class :args]}))

(defn parse-quote
  [[_ expr :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to quote, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [const (analyze-const expr env)]
    {:op       :quote
     :expr     const
     :form     form
     :env      env
     :literal? true
     :children [:expr]}))

(defn parse-set!
  [[_ target val :as form] env]
  (when-not (= 3 (count form))
    (throw (ex-info (str "Wrong number of args to set!, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [target (analyze-form target (ctx env :ctx/expr))
        val (analyze-form val (ctx env :ctx/expr))]
    {:op       :set!
     :env      env
     :form     form
     :target   target
     :val      val
     :children [:target :val]}))

(defn analyze-body [body env]
  ;; :body is used by emit-form to remove the artificial 'do
  (assoc (parse (cons 'do body) env) :body? true))

(defn valid-binding-symbol? [s]
  (and (symbol? s)
       (not (namespace s))
       (not (re-find #"\." (name s)))))

(defn ^:private split-with' [pred coll]
  (loop [take [] drop coll]
    (if (seq drop)
      (let [[el & r] drop]
        (if (pred el)
          (recur (conj take el) r)
          [(seq take) drop]))
      [(seq take) ()])))

(declare parse-catch)
(defn parse-try
  [[_ & body :as form] env]
  (let [catch? (every-pred seq? #(= (first %) 'catch))
        finally? (every-pred seq? #(= (first %) 'finally))
        [body tail'] (split-with' (complement (some-fn catch? finally?)) body)
        [cblocks tail] (split-with' catch? tail')
        [[fblock & fbs :as fblocks] tail] (split-with' finally? tail)]
    (when-not (empty? tail)
      (throw (ex-info "Only catch or finally clause can follow catch in try expression"
                      (merge {:expr tail
                              :form form}
                             (-source-info form env)))))
    (when-not (empty? fbs)
      (throw (ex-info "Only one finally clause allowed in try expression"
                      (merge {:expr fblocks
                              :form form}
                             (-source-info form env)))))
    (let [env' (assoc env :in-try true)
          body (analyze-body body env')
          cenv (ctx env' :ctx/expr)
          cblocks (mapv #(parse-catch % cenv) cblocks)
          fblock (when-not (empty? fblock)
                   (analyze-body (rest fblock) (ctx env :ctx/statement)))]
      (merge {:op      :try
              :env     env
              :form    form
              :body    body
              :catches cblocks}
             (when fblock
               {:finally fblock})
             {:children (into [:body :catches]
                              (when fblock [:finally]))}))))

(defn parse-catch
  [[_ etype ename & body :as form] env]
  (when-not (valid-binding-symbol? ename)
    (throw (ex-info (str "Bad binding form: " ename)
                    (merge {:sym ename
                            :form form}
                           (-source-info form env)))))
  (let [env (dissoc env :in-try)
        local {:op    :binding
               :env   env
               :form  ename
               :name  ename
               :local :catch}]
    {:op          :catch
     :class       (analyze-form etype (assoc env :locals {}))
     :local       local
     :env         env
     :form        form
     :body        (analyze-body body (assoc-in env [:locals ename] (dissoc-env local)))
     :children    [:class :local :body]}))

(defn parse-throw
  [[_ throw :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to throw, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op        :throw
   :env       env
   :form      form
   :exception (analyze-form throw (ctx env :ctx/expr))
   :children  [:exception]})

(defn validate-bindings
  [[op bindings & _ :as form] env]
  (when-let [error-msg
             (cond
              (not (vector? bindings))
              (str op " requires a vector for its bindings, had: "
                   (class bindings))

              (not (even? (count bindings)))
              (str op " requires an even number of forms in binding vector, had: "
                   (count bindings)))]
    (throw (ex-info error-msg
                    (merge {:form     form
                            :bindings bindings}
                           (-source-info form env))))))

(defn parse-letfn*
  [[_ bindings & body :as form] env]
  (validate-bindings form env)
  (let [bindings (apply array-map bindings) ;; pick only one local with the same name, if more are present.
        fns      (keys bindings)]
    (when-let [[sym] (seq (remove valid-binding-symbol? fns))]
      (throw (ex-info (str "Bad binding form: " sym)
                      (merge {:form form
                              :sym  sym}
                             (-source-info form env)))))
    (let [binds (reduce (fn [binds name]
                          (assoc binds name
                                 {:op    :binding
                                  :env   env
                                  :name  name
                                  :form  name
                                  :local :letfn}))
                        {} fns)
          e (update-in env [:locals] merge binds) ;; pre-seed locals
          binds (reduce-kv (fn [binds name bind]
                             (assoc binds name
                                    (merge bind
                                           {:init     (analyze-form (bindings name)
                                                                    (ctx e :ctx/expr))
                                            :children [:init]})))
                           {} binds)
          e (update-in env [:locals] merge (update-vals binds dissoc-env))
          body (analyze-body body e)]
      {:op       :letfn
       :env      env
       :form     form
       :bindings (vec (vals binds)) ;; order is irrelevant
       :body     body
       :children [:bindings :body]})))

(defn analyze-let
  [[op bindings & body :as form] {:keys [context loop-id] :as env}]
  (validate-bindings form env)
  (let [loop? (= 'loop* op)]
    (loop [bindings bindings
           env (ctx env :ctx/expr)
           binds []]
      (if-let [[name init & bindings] (seq bindings)]
        (if (not (valid-binding-symbol? name))
          (throw (ex-info (str "Bad binding form: " name)
                          (merge {:form form
                                  :sym  name}
                                 (-source-info form env))))
          (let [init-expr (analyze-form init env)
                bind-expr {:op       :binding
                           :env      env
                           :name     name
                           :init     init-expr
                           :form     name
                           :local    (if loop? :loop :let)
                           :children [:init]}]
            (recur bindings
                   (assoc-in env [:locals name] (dissoc-env bind-expr))
                   (conj binds bind-expr))))
        (let [body-env (assoc env :context (if loop? :ctx/return context))
              body (analyze-body body (merge body-env
                                             (when loop?
                                               {:loop-id     loop-id
                                                :loop-locals (count binds)})))]
          {:body     body
           :bindings binds
           :children [:bindings :body]})))))

(defn parse-let*
  [form env]
  (into {:op   :let
         :form form
         :env  env}
        (analyze-let form env)))

(defn parse-loop*
  [form env]
  (let [loop-id (gensym "loop_") ;; can be used to find matching recur
        env (assoc env :loop-id loop-id)]
    (into {:op      :loop
           :form    form
           :env     env
           :loop-id loop-id}
          (analyze-let form env))))

(defn parse-recur
  [[_ & exprs :as form] {:keys [context loop-locals loop-id]
                         :as env}]
  (when-let [error-msg
             (cond
              (not (isa? context :ctx/return))
              "Can only recur from tail position"

              (not (= (count exprs) loop-locals))
              (str "Mismatched argument count to recur, expected: " loop-locals
                   " args, had: " (count exprs)))]
    (throw (ex-info error-msg
                    (merge {:exprs exprs
                            :form  form}
                           (-source-info form env)))))

  (let [exprs (mapv (analyze-in-env (ctx env :ctx/expr)) exprs)]
    {:op          :recur
     :env         env
     :form        form
     :exprs       exprs
     :loop-id     loop-id
     :children    [:exprs]}))

(defn analyze-fn-method [[params & body :as form] {:keys [locals local] :as env}]
  (when-not (vector? params)
    (throw (ex-info "Parameter declaration should be a vector"
                    (merge {:params params
                            :form   form}
                           (-source-info form env)
                           (-source-info params env)))))
  (when (not-every? valid-binding-symbol? params)
    (throw (ex-info (str "Params must be valid binding symbols, had: "
                         (mapv class params))
                    (merge {:params params
                            :form   form}
                           (-source-info form env)
                           (-source-info params env))))) ;; more specific
  (let [variadic? (boolean (some '#{&} params))
        params-names (if variadic? (conj (pop (pop params)) (peek params)) params)
        env (dissoc env :local)
        arity (count params-names)
        params-expr (mapv (fn [name id]
                            {:env       env
                             :form      name
                             :name      name
                             :variadic? (and variadic?
                                             (= id (dec arity)))
                             :op        :binding
                             :arg-id    id
                             :local     :arg})
                          params-names (range))
        fixed-arity (if variadic?
                      (dec arity)
                      arity)
        loop-id (gensym "loop_")
        body-env (into (update-in env [:locals]
                                  merge (zipmap params-names (map dissoc-env params-expr)))
                       {:context     :ctx/return
                        :loop-id     loop-id
                        :loop-locals (count params-expr)})
        body (analyze-body body body-env)]
    (when variadic?
      (let [x (drop-while #(not= % '&) params)]
        (when (contains? #{nil '&} (second x))
          (throw (ex-info "Invalid parameter list"
                          (merge {:params params
                                  :form   form}
                                 (-source-info form env)
                                 (-source-info params env)))))
        (when (not= 2 (count x))
          (throw (ex-info (str "Unexpected parameter: " (first (drop 2 x))
                               " after variadic parameter: " (second x))
                          (merge {:params params
                                  :form   form}
                                 (-source-info form env)
                                 (-source-info params env)))))))
    (merge
     {:op          :fn-method
      :form        form
      :loop-id     loop-id
      :env         env
      :variadic?   variadic?
      :params      params-expr
      :fixed-arity fixed-arity
      :body        body
      :children    [:params :body]}
     (when local
       {:local (dissoc-env local)}))))

(defn parse-fn*
  [[op & args :as form] env]
  (wrapping-meta
   (let [[n meths] (if (symbol? (first args))
                     [(first args) (next args)]
                     [nil (seq args)])
         name-expr {:op    :binding
                    :env   env
                    :form  n
                    :local :fn
                    :name  n}
         e (if n (assoc (assoc-in env [:locals n] (dissoc-env name-expr)) :local name-expr) env)
         once? (-> op meta :once boolean)
         menv (assoc (dissoc e :in-try) :once once?)
         meths (if (vector? (first meths)) (list meths) meths) ;;turn (fn [] ...) into (fn ([]...))
         methods-exprs (mapv #(analyze-fn-method % menv) meths)
         variadic (seq (filter :variadic? methods-exprs))
         variadic? (boolean variadic)
         fixed-arities (seq (map :fixed-arity (remove :variadic? methods-exprs)))
         max-fixed-arity (when fixed-arities (apply max fixed-arities))]
     (when (>= (count variadic) 2)
       (throw (ex-info "Can't have more than 1 variadic overload"
                       (merge {:variadics (mapv :form variadic)
                               :form      form}
                              (-source-info form env)))))
     (when (not= (seq (distinct fixed-arities)) fixed-arities)
       (throw (ex-info "Can't have 2 or more overloads with the same arity"
                       (merge {:form form}
                              (-source-info form env)))))
     (when (and variadic?
                (not-every? #(<= (:fixed-arity %)
                           (:fixed-arity (first variadic)))
                       (remove :variadic? methods-exprs)))
       (throw (ex-info "Can't have fixed arity overload with more params than variadic overload"
                       (merge {:form form}
                              (-source-info form env)))))
     (merge {:op              :fn
             :env             env
             :form            form
             :variadic?       variadic?
             :max-fixed-arity max-fixed-arity
             :methods         methods-exprs
             :once            once?}
            (when n
              {:local name-expr})
            {:children (conj (if n [:local] []) :methods)}))))

(defn parse-def
  [[_ sym & expr :as form] {:keys [ns] :as env}]
  (when (not (symbol? sym))
    (throw (ex-info (str "First argument to def must be a symbol, had: " (class sym))
                    (merge {:form form}
                           (-source-info form env)))))
  (when (and (namespace sym)
             (not= *ns* (the-ns (symbol (namespace sym)))))
    (throw (ex-info "Cannot def namespace qualified symbol"
                    (merge {:form form
                            :sym sym}
                           (-source-info form env)))))
  (let [pfn (fn
              ([])
              ([init]
                 {:init init})
              ([doc init]
                 {:pre [(string? doc)]}
                 {:init init :doc doc}))
        args (apply pfn expr)

        doc (or (:doc args) (-> sym meta :doc))
        arglists (when-let [arglists (:arglists (meta sym))]
                   (second arglists)) ;; drop quote

        sym (with-meta (symbol (name sym))
              (merge (meta sym)
                     (when arglists
                       {:arglists arglists})
                     (when doc
                       {:doc doc})
                     (-source-info form env)))

        var (create-var sym env) ;; interned var will have quoted arglists, replaced on evaluation
        _ (env/deref-env) ;; make sure *env* is bound
        _ (swap! env/*env* assoc-in [:namespaces ns :mappings sym] var)

        meta (merge (meta sym)
                    (when arglists
                      {:arglists (list 'quote arglists)}))

        meta-expr (when meta (analyze-form meta (ctx env :ctx/expr))) ;; meta on def sym will be evaluated

        args (when-let [[_ init] (find args :init)]
               (assoc args :init (analyze-form init (ctx env :ctx/expr))))
        init? (:init args)
        children (into (into [] (when meta [:meta]))
                       (when init? [:init]))]

    (merge {:op   :def
            :env  env
            :form form
            :name sym
            :var  var}
           (when meta
             {:meta meta-expr})
           args
           (when-not (empty? children)
             {:children children}))))

(defn parse-dot
  [[_ target & [m-or-f & args] :as form] env]
  (when-not (>= (count form) 3)
    (throw (ex-info (str "Wrong number of args to ., had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  (let [[m-or-f field?] (if (and (symbol? m-or-f)
                                 (= \- (first (name m-or-f))))
                          [(-> m-or-f name (subs 1) symbol) true]
                          [(if args (cons m-or-f args) m-or-f) false])
        target-expr (analyze-form target (ctx env :ctx/expr))
        call? (and (not field?) (seq? m-or-f))]

    (when (and call? (not (symbol? (first m-or-f))))
      (throw (ex-info (str "Method name must be a symbol, had: " (class (first m-or-f)))
                      (merge {:form   form
                              :method m-or-f}
                             (-source-info form env)))))
    (merge {:form   form
            :env    env
            :target target-expr}
           (cond
            call?
            {:op       :host-call
             :method   (symbol (name (first m-or-f)))
             :args     (mapv (analyze-in-env (ctx env :ctx/expr)) (next m-or-f))
             :children [:target :args]}

            field?
            {:op          :host-field
             :assignable? true
             :field       (symbol (name m-or-f))
             :children    [:target]}

            :else
            {:op          :host-interop ;; either field access or no-args method call
             :assignable? true
             :m-or-f      (symbol (name m-or-f))
             :children    [:target]}))))

(defn parse-invoke
  [[f & args :as form] env]
  (let [fenv (ctx env :ctx/expr)
        fn-expr (analyze-form f fenv)
        args-expr (mapv (analyze-in-env fenv) args)
        m (meta form)]
    (merge {:op   :invoke
            :form form
            :env  env
            :fn   fn-expr
            :args args-expr}
           (when (seq m)
             {:meta m}) ;; meta on invoke form will not be evaluated
           {:children [:fn :args]})))

(defn parse-var
  [[_ var :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to var, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  (if-let [var (resolve-sym var env)]
    {:op   :the-var
     :env  env
     :form form
     :var  var}
    (throw (ex-info (str "var not found: " var) {:var var}))))

(defn -parse
  "Takes a form and an env map and dispatches on the head of the form, that is
   a special form."
  [form env]
  ((case (first form)
     do      parse-do
     if      parse-if
     new     parse-new
     quote   parse-quote
     set!    parse-set!
     try     parse-try
     throw   parse-throw
     def     parse-def
     .       parse-dot
     let*    parse-let*
     letfn*  parse-letfn*
     loop*   parse-loop*
     recur   parse-recur
     fn*     parse-fn*
     var     parse-var
     #_:else parse-invoke)
   form env))
