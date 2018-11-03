;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from tools.analyzer
(ns clojure.core.typed.analyzer2.pre-analyze
  (:require [clojure.tools.analyzer.utils :as u]
            [clojure.tools.analyzer.env :as env]
            [clojure.core.typed.analyzer2 :as ana])
  (:import (clojure.lang Symbol IPersistentVector IPersistentMap IPersistentSet ISeq IType IRecord)))

(defmulti -pre-analyze-form (fn [form _] (class form)))

(declare pre-analyze-symbol
         pre-analyze-vector
         pre-analyze-map
         pre-analyze-set
         pre-analyze-seq
         pre-analyze-const)

(def ^:dynamic pre-analyze-form
  "Like pre-analyze, but does not mark the form with :top-level true"
  -pre-analyze-form)

(defmethod -pre-analyze-form Symbol
  [form env]
  (pre-analyze-symbol form env))

(defmethod -pre-analyze-form IPersistentVector
  [form env]
  (pre-analyze-vector form env))

(defmethod -pre-analyze-form IPersistentMap
  [form env]
  (pre-analyze-map form env))

(defmethod -pre-analyze-form IPersistentSet
  [form env]
  (pre-analyze-set form env))

(defmethod -pre-analyze-form ISeq
  [form env]
  (if-let [form (seq form)]
    (pre-analyze-seq form env)
    (pre-analyze-const form env)))

(defmethod -pre-analyze-form IType
  [form env]
  (pre-analyze-const form env :type))

(prefer-method -pre-analyze-form IType IPersistentMap)
(prefer-method -pre-analyze-form IType IPersistentVector)
(prefer-method -pre-analyze-form IType IPersistentSet)
(prefer-method -pre-analyze-form IType ISeq)

(defmethod -pre-analyze-form IRecord
  [form env]
  (pre-analyze-const form env :record))

(prefer-method -pre-analyze-form IRecord IPersistentMap)
(prefer-method -pre-analyze-form IRecord IPersistentVector)
(prefer-method -pre-analyze-form IRecord IPersistentSet)
(prefer-method -pre-analyze-form IRecord ISeq)

(defmethod -pre-analyze-form :default
  [form env]
  (pre-analyze-const form env))

(defn pre-analyze
  "Given a top-level form to analyze and an environment, a map containing:
   * :locals     a map from binding symbol to AST of the binding value
   * :context    a keyword describing the form's context from the :ctx/* hierarchy.
    ** :ctx/expr      the form is an expression: its value is used
    ** :ctx/return    the form is an expression in return position, derives :ctx/expr
    ** :ctx/statement the value of the form is not used
   * :ns         a symbol representing the current namespace of the form to be
                 analyzed

   returns one level of the AST for that form, with all children
   stubbed out with :unanalyzed nodes."
  [form env]
  (assoc (pre-analyze-form form env) :top-level true))

(defn pre-analyze-child
  [form env]
  {:op :unanalyzed
   :form form
   :env env
   ;; ::config will be inherited by whatever node
   ;; this :unanalyzed node becomes when analyzed
   ::config {}})

(defn pre-analyze-child-in-env
  "Takes an env map and returns a function that analyzes a form in that env"
  [env]
  (fn [form] (pre-analyze-child form env)))

(def ^{:dynamic  true
       :arglists '([[op & args] env])
       :doc      "Function that dispatches on op, should default to -pre-parse"}
  pre-parse)

;; this node wraps non-quoted collections literals with metadata attached
;; to them, the metadata will be evaluated at run-time, not treated like a constant
(defn pre-wrapping-meta
  [{:keys [form env] :as expr}]
  (let [meta (meta form)]
    (if (and (u/obj? form)
             (seq meta))
      {:op       :with-meta
       :env      env
       :form     form
       :meta     (pre-analyze-child meta (u/ctx env :ctx/expr))
       :expr     (assoc-in expr [:env :context] :ctx/expr)
       :children [:meta :expr]}
      expr)))

(defn pre-analyze-const
  [form env & [type]]
  (let [type (or type (u/classify form))]
    (merge
     {:op       :const
      :env      env
      :type     type
      :literal? true
      :val      form
      :form     form}
     (when-let [m (and (u/obj? form)
                       (not-empty (meta form)))]
       {:meta     (pre-analyze-const m (u/ctx env :ctx/expr) :map) ;; metadata on a constant literal will not be evaluated at
        :children [:meta]}))))                               ;; runtime, this is also true for metadata on quoted collection literals

(defn pre-analyze-vector
  [form env]
  (let [items-env (u/ctx env :ctx/expr)
        items (mapv (pre-analyze-child-in-env items-env) form)]
    (pre-wrapping-meta
     {:op       :vector
      :env      env
      :items    items
      :form     form
      :children [:items]})))

(defn pre-analyze-map
  [form env]
  (let [kv-env (u/ctx env :ctx/expr)
        [keys vals] (reduce-kv (fn [[keys vals] k v]
                                 [(conj keys k) (conj vals v)])
                               [[] []] form)
        ks (mapv (pre-analyze-child-in-env kv-env) keys)
        vs (mapv (pre-analyze-child-in-env kv-env) vals)]
    (pre-wrapping-meta
     {:op       :map
      :env      env
      :keys     ks
      :vals     vs
      :form     form
      :children [:keys :vals]})))

(defn pre-analyze-set
  [form env]
  (let [items-env (u/ctx env :ctx/expr)
        items (mapv (pre-analyze-child-in-env items-env) form)]
    (pre-wrapping-meta
     {:op       :set
      :env      env
      :items    items
      :form     form
      :children [:items]})))

(defn pre-analyze-symbol
  [sym env]
  (let [mform (ana/macroexpand-1 sym env)] ;; t.a.j/macroexpand-1 macroexpands Class/Field into (. Class Field)
    (if (= mform sym)
      (merge (if-let [{:keys [mutable children] :as local-binding} (-> env :locals sym)] ;; locals shadow globals
               (merge local-binding
                      {:op          :local
                       :assignable? (boolean mutable)
                       ;; don't walk :init, but keep in AST
                       :children    (vec (remove #{:init} children))})
               (if-let [var (let [v (ana/resolve-sym sym env)]
                              (and (ana/var? v) v))]
                 (let [m (meta var)]
                   {:op          :var
                    :assignable? (u/dynamic? var m) ;; we cannot statically determine if a Var is in a thread-local context
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
      ;; FIXME not 100% sure if this is the correct pre-analyze-* call
      (-> (pre-analyze-form mform env)
        (update-in [:raw-forms] (fnil conj ()) sym)))))

(defn pre-analyze-seq
  [form env]
  ;(prn "pre-analyze-seq" form)
  (let [op (first form)]
    (when (nil? op)
      (throw (ex-info "Can't call nil"
                      (merge {:form form}
                             (u/-source-info form env)))))
    (let [mform (ana/macroexpand-1 form env)]
      (if (= form mform) ;; function/special-form invocation
        (pre-parse mform env)
        (-> (pre-analyze-form mform env)
            (update-in [:raw-forms] (fnil conj ())
                       (vary-meta form assoc ::resolved-op (ana/resolve-sym op env))))))))

(defn pre-parse-do
  [[_ & exprs :as form] env]
  (let [statements-env (u/ctx env :ctx/statement)
        [statements ret] (loop [statements [] [e & exprs] exprs]
                           (if (seq exprs)
                             (recur (conj statements e) exprs)
                             [statements e]))
        statements (mapv (pre-analyze-child-in-env statements-env) statements)
        ret (pre-analyze-child ret env)]
    {:op         :do
     :env        env
     :form       form
     :statements statements
     :ret        ret
     :children   [:statements :ret]}))

(defn pre-parse-if
  [[_ test then else :as form] env]
  (let [formc (count form)]
    (when-not (or (= formc 3) (= formc 4))
      (throw (ex-info (str "Wrong number of args to if, had: " (dec (count form)))
                      (merge {:form form}
                             (u/-source-info form env))))))
  (let [test-expr (pre-analyze-child test (u/ctx env :ctx/expr))
        then-expr (pre-analyze-child then env)
        else-expr (pre-analyze-child else env)]
    {:op       :if
     :form     form
     :env      env
     :test     test-expr
     :then     then-expr
     :else     else-expr
     :children [:test :then :else]}))

(defn pre-parse-new
  [[_ class & args :as form] env]
  (when-not (>= (count form) 2)
    (throw (ex-info (str "Wrong number of args to new, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [args-env (u/ctx env :ctx/expr)
        args (mapv (pre-analyze-child-in-env args-env) args)]
    {:op          :new
     :env         env
     :form        form
     :class       (pre-analyze-form class (assoc env :locals {})) ;; avoid shadowing
     :args        args
     :children    [:class :args]}))

(defn pre-parse-quote
  [[_ expr :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to quote, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [const (pre-analyze-const expr env)]
    {:op       :quote
     :expr     const
     :form     form
     :env      env
     :literal? true
     :children [:expr]}))

(defn pre-parse-set!
  [[_ target val :as form] env]
  (when-not (= 3 (count form))
    (throw (ex-info (str "Wrong number of args to set!, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [target (pre-analyze-child target (u/ctx env :ctx/expr))
        val (pre-analyze-child val (u/ctx env :ctx/expr))]
    {:op       :set!
     :env      env
     :form     form
     :target   target
     :val      val
     :children [:target :val]}))

(defn pre-analyze-body [body env]
  ;; :body is used by emit-form to remove the artificial 'do
  (assoc (pre-parse (cons 'do body) env) :body? true))

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

(declare pre-parse-catch)
(defn pre-parse-try
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
                             (u/-source-info form env)))))
    (when-not (empty? fbs)
      (throw (ex-info "Only one finally clause allowed in try expression"
                      (merge {:expr fblocks
                              :form form}
                             (u/-source-info form env)))))
    (let [env' (assoc env :in-try true)
          body (pre-analyze-body body env')
          cenv (u/ctx env' :ctx/expr)
          cblocks (mapv #(pre-parse-catch % cenv) cblocks)
          fblock (when-not (empty? fblock)
                   (pre-analyze-body (rest fblock) (u/ctx env :ctx/statement)))]
      (merge {:op      :try
              :env     env
              :form    form
              :body    body
              :catches cblocks}
             (when fblock
               {:finally fblock})
             {:children (into [:body :catches]
                              (when fblock [:finally]))}))))

(defn pre-parse-catch
  [[_ etype ename & body :as form] env]
  (when-not (valid-binding-symbol? ename)
    (throw (ex-info (str "Bad binding form: " ename)
                    (merge {:sym ename
                            :form form}
                           (u/-source-info form env)))))
  (let [env (dissoc env :in-try)
        local {:op    :binding
               :env   env
               :form  ename
               :name  ename
               :local :catch}]
    {:op          :catch
     :class       (pre-analyze-child etype (assoc env :locals {}))
     :local       local
     :env         env
     :form        form
     :body        (pre-analyze-body body (assoc-in env [:locals ename] (u/dissoc-env local)))
     :children    [:class :local :body]}))

(defn pre-parse-throw
  [[_ throw :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to throw, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  {:op        :throw
   :env       env
   :form      form
   :exception (pre-analyze-child throw (u/ctx env :ctx/expr))
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
                           (u/-source-info form env))))))

(defn pre-parse-letfn*
  [[_ bindings & body :as form] env]
  (validate-bindings form env)
  (let [bindings (apply array-map bindings) ;; pick only one local with the same name, if more are present.
        fns      (keys bindings)]
    (when-let [[sym] (seq (remove valid-binding-symbol? fns))]
      (throw (ex-info (str "Bad binding form: " sym)
                      (merge {:form form
                              :sym  sym}
                             (u/-source-info form env)))))
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
                                           {:init     (pre-analyze-child (bindings name)
                                                                         (u/ctx e :ctx/expr))
                                            :children [:init]})))
                           {} binds)
          e (update-in env [:locals] merge (u/update-vals binds u/dissoc-env))
          body (pre-analyze-body body e)]
      {:op       :letfn
       :env      env
       :form     form
       :bindings (vec (vals binds)) ;; order is irrelevant
       :body     body
       :children [:bindings :body]})))

(defn pre-analyze-let
  [[op bindings & body :as form] {:keys [context loop-id] :as env}]
  (validate-bindings form env)
  (let [loop? (= 'loop* op)]
    (loop [bindings bindings
           env (u/ctx env :ctx/expr)
           binds []]
      (if-let [[name init & bindings] (seq bindings)]
        (if (not (valid-binding-symbol? name))
          (throw (ex-info (str "Bad binding form: " name)
                          (merge {:form form
                                  :sym  name}
                                 (u/-source-info form env))))
          (let [init-expr (pre-analyze-child init env)
                bind-expr {:op       :binding
                           :env      env
                           :name     name
                           :init     init-expr
                           :form     name
                           :local    (if loop? :loop :let)
                           :children [:init]}]
            (recur bindings
                   (assoc-in env [:locals name] (u/dissoc-env bind-expr))
                   (conj binds bind-expr))))
        (let [body-env (assoc env :context (if loop? :ctx/return context))
              body (pre-analyze-body body (merge body-env
                                             (when loop?
                                               {:loop-id     loop-id
                                                :loop-locals (count binds)})))]
          {:body     body
           :bindings binds
           :children [:bindings :body]})))))

(defn pre-parse-let*
  [form env]
  (into {:op   :let
         :form form
         :env  env}
        (pre-analyze-let form env)))

(defn pre-parse-loop*
  [form env]
  (let [loop-id (gensym "loop_") ;; can be used to find matching recur
        env (assoc env :loop-id loop-id)]
    (into {:op      :loop
           :form    form
           :env     env
           :loop-id loop-id}
          (pre-analyze-let form env))))

(defn pre-parse-recur
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
                           (u/-source-info form env)))))

  (let [exprs (mapv (pre-analyze-child-in-env (u/ctx env :ctx/expr)) exprs)]
    {:op          :recur
     :env         env
     :form        form
     :exprs       exprs
     :loop-id     loop-id
     :children    [:exprs]}))

(defn pre-analyze-fn-method [[params & body :as form] {:keys [locals local] :as env}]
  (when-not (vector? params)
    (throw (ex-info "Parameter declaration should be a vector"
                    (merge {:params params
                            :form   form}
                           (u/-source-info form env)
                           (u/-source-info params env)))))
  (when (not-every? valid-binding-symbol? params)
    (throw (ex-info (str "Params must be valid binding symbols, had: "
                         (mapv class params))
                    (merge {:params params
                            :form   form}
                           (u/-source-info form env)
                           (u/-source-info params env))))) ;; more specific
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
                                  merge (zipmap params-names (map u/dissoc-env params-expr)))
                       {:context     :ctx/return
                        :loop-id     loop-id
                        :loop-locals (count params-expr)})
        body (pre-analyze-body body body-env)]
    (when variadic?
      (let [x (drop-while #(not= % '&) params)]
        (when (contains? #{nil '&} (second x))
          (throw (ex-info "Invalid parameter list"
                          (merge {:params params
                                  :form   form}
                                 (u/-source-info form env)
                                 (u/-source-info params env)))))
        (when (not= 2 (count x))
          (throw (ex-info (str "Unexpected parameter: " (first (drop 2 x))
                               " after variadic parameter: " (second x))
                          (merge {:params params
                                  :form   form}
                                 (u/-source-info form env)
                                 (u/-source-info params env)))))))
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
       {:local (u/dissoc-env local)}))))

(defn pre-parse-fn*
  [[op & args :as form] env]
  (pre-wrapping-meta
   (let [[n meths] (if (symbol? (first args))
                     [(first args) (next args)]
                     [nil (seq args)])
         name-expr {:op    :binding
                    :env   env
                    :form  n
                    :local :fn
                    :name  n}
         e (if n (assoc (assoc-in env [:locals n] (u/dissoc-env name-expr)) :local name-expr) env)
         once? (-> op meta :once boolean)
         menv (assoc (dissoc e :in-try) :once once?)
         meths (if (vector? (first meths)) (list meths) meths) ;;turn (fn [] ...) into (fn ([]...))
         methods-exprs (mapv #(pre-analyze-fn-method % menv) meths)
         variadic (seq (filter :variadic? methods-exprs))
         variadic? (boolean variadic)
         fixed-arities (seq (map :fixed-arity (remove :variadic? methods-exprs)))
         max-fixed-arity (when fixed-arities (apply max fixed-arities))]
     (when (>= (count variadic) 2)
       (throw (ex-info "Can't have more than 1 variadic overload"
                       (merge {:variadics (mapv :form variadic)
                               :form      form}
                              (u/-source-info form env)))))
     (when (not= (seq (distinct fixed-arities)) fixed-arities)
       (throw (ex-info "Can't have 2 or more overloads with the same arity"
                       (merge {:form form}
                              (u/-source-info form env)))))
     (when (and variadic?
                (not-every? #(<= (:fixed-arity %)
                           (:fixed-arity (first variadic)))
                       (remove :variadic? methods-exprs)))
       (throw (ex-info "Can't have fixed arity overload with more params than variadic overload"
                       (merge {:form form}
                              (u/-source-info form env)))))
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

(defn pre-parse-def
  [[_ sym & expr :as form] {:keys [ns] :as env}]
  (when (not (symbol? sym))
    (throw (ex-info (str "First argument to def must be a symbol, had: " (class sym))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (when (and (namespace sym)
             (not= *ns* (the-ns (symbol (namespace sym)))))
    (throw (ex-info "Cannot def namespace qualified symbol"
                    (merge {:form form
                            :sym sym}
                           (u/-source-info form env)))))
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
                     (u/-source-info form env)))

        var (ana/create-var sym env) ;; interned var will have quoted arglists, replaced on evaluation
        _ (env/deref-env) ;; make sure *env* is bound
        _ (swap! env/*env* assoc-in [:namespaces ns :mappings sym] var)

        meta (merge (meta sym)
                    (when arglists
                      {:arglists (list 'quote arglists)}))

        meta-expr (when meta (pre-analyze-child meta (u/ctx env :ctx/expr))) ;; meta on def sym will be evaluated

        args (when-let [[_ init] (find args :init)]
               (assoc args :init (pre-analyze-child init (u/ctx env :ctx/expr))))
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

(defn pre-parse-dot
  [[_ target & [m-or-f & args] :as form] env]
  (when-not (>= (count form) 3)
    (throw (ex-info (str "Wrong number of args to ., had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (let [[m-or-f field?] (if (and (symbol? m-or-f)
                                 (= \- (first (name m-or-f))))
                          [(-> m-or-f name (subs 1) symbol) true]
                          [(if args (cons m-or-f args) m-or-f) false])
        target-expr (pre-analyze-child target (u/ctx env :ctx/expr))
        call? (and (not field?) (seq? m-or-f))]

    (when (and call? (not (symbol? (first m-or-f))))
      (throw (ex-info (str "Method name must be a symbol, had: " (class (first m-or-f)))
                      (merge {:form   form
                              :method m-or-f}
                             (u/-source-info form env)))))
    (merge {:form   form
            :env    env
            :target target-expr}
           (cond
            call?
            {:op       :host-call
             :method   (symbol (name (first m-or-f)))
             :args     (mapv (pre-analyze-child-in-env (u/ctx env :ctx/expr)) (next m-or-f))
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

(defn pre-parse-invoke
  [[f & args :as form] env]
  (let [fenv (u/ctx env :ctx/expr)
        fn-expr (pre-analyze-child f fenv)
        args-expr (mapv (pre-analyze-child-in-env fenv) args)
        m (meta form)]
    (merge {:op   :invoke
            :form form
            :env  env
            :fn   fn-expr
            :args args-expr}
           (when (seq m)
             {:meta m}) ;; meta on invoke form will not be evaluated
           {:children [:fn :args]})))

(defn pre-parse-var
  [[_ var :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to var, had: " (dec (count form)))
                    (merge {:form form}
                           (u/-source-info form env)))))
  (if-let [var (ana/resolve-sym var env)]
    {:op   :the-var
     :env  env
     :form form
     :var  var}
    (throw (ex-info (str "var not found: " var) {:var var}))))

(defn -pre-parse
  "Takes a form and an env map and dispatches on the head of the form, that is
   a special form."
  [form env]
  ((case (first form)
     do      pre-parse-do
     if      pre-parse-if
     new     pre-parse-new
     quote   pre-parse-quote
     set!    pre-parse-set!
     try     pre-parse-try
     throw   pre-parse-throw
     def     pre-parse-def
     .       pre-parse-dot
     let*    pre-parse-let*
     letfn*  pre-parse-letfn*
     loop*   pre-parse-loop*
     recur   pre-parse-recur
     fn*     pre-parse-fn*
     var     pre-parse-var
     #_:else pre-parse-invoke)
   form env))
