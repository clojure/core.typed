;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns clojure-analyzer.compiler
  (:refer-clojure :exclude [macroexpand-1])
  (:import (java.io InputStreamReader PushbackReader)
           (clojure.lang RT))
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as string]))

(def initial-namespaces '{clojure.core {:name clojure.core}
                          clojure.user {:name clojure.user}})

(defonce namespaces (atom initial-namespaces))

(defn reset-namespaces! []
  (swap! namespaces (constantly initial-namespaces)))

(def ^:dynamic *analyzer-ns* 'clojure.user)
(def ^:dynamic *analyzer-warn-on-undeclared* false)

(defn confirm-var-exists [env prefix suffix]
  (when *analyzer-warn-on-undeclared*
    (let [crnt-ns (-> env :ns :name)]
      (when (= prefix crnt-ns)
        (when-not (-> @namespaces crnt-ns :defs suffix)
          (binding [*out* *err*]
            (println
              (str "WARNING: Use of undeclared Var " prefix "/" suffix
                   (when (:line env)
                     (str " at line " (:line env)))))))))))

(defn resolve-ns-alias [env name]
  (let [sym (symbol name)]
    (or (get (:requires (:ns env)) sym)
        (find-ns sym))))

(defn resolve-class-alias [env name]
  (let [sym (symbol name)]
    (or (get (:imports (:ns env)) sym)
        (when (class? (resolve sym))
          (resolve sym)))))

(defn resolve-existing [env sym]
  (let [s (str sym)
        lb (-> env :locals sym)]
    (cond
      ;; local binding reference
      lb {:op :local :info {:name (:name lb)}}

      ;; aliased or qualified var reference
      (when-let [ns (namespace sym)]
        (resolve-ns-alias env ns))
      (let [ns (namespace sym)
            full-ns (resolve-ns-alias env ns)]
        (confirm-var-exists env full-ns (symbol (name sym)))
        {:op :var :info {:ns {:name full-ns
                              :alias (symbol ns)}
                         :name (symbol (str full-ns "/" (name sym)))}})

      ;; aliased or fully qualified Java field/method reference
      (when-let [ns (namespace sym)]
        (resolve-class-alias env ns))
      (let [ns (namespace sym)
            class-obj (resolve-class-alias env ns)]
        {:op :field-or-method :info {:name (symbol (str ns "/" (name sym)))
                                     :field-or-method {:name (symbol (name sym))
                                                       :class class-obj}}})

      ;; fully qualified Java Class reference
      (.contains s ".")
      (let [_ (assert (not (.contains s "/")))
            classobj (resolve sym)
            _ (assert (class? classobj))]
        {:op :class :info {:class sym}})

      ;; aliased Java Class reference
      (get-in env [:imports sym])
      {:op :class :info {:name sym
                         :class (get-in env [:imports sym])}}

      ;; use'd var reference (unqualified)
      (get-in env [:uses sym])
      {:op :var :info {:name (symbol (str (get-in env [:uses sym]) "/" (name sym)))}}

      :else
      (let [full-ns (-> env :ns :name)
            _ (assert full-ns)]
        (if-let [class-obj (get-in env [:imports sym])]
          ;; 
          {:op :class :info {:name full-ns
                             :class class-obj}}
          (do (confirm-var-exists env full-ns sym)
            {:op :var :info {:name (symbol (str full-ns "/" (name sym)))}}))))))

(defn resolve-var [env sym]
  (let [s (str sym)
        lb (-> env :locals sym)
        nm 
        (cond
         lb (:name lb)
       
         (namespace sym)
         (let [ns (namespace sym)]
           (symbol (str (resolve-ns-alias env ns) "/" (name sym))))

         (.contains s ".")
         (let [idx (.indexOf s ".")
               prefix (symbol (subs s 0 idx))
               suffix (subs s idx)
               lb (-> env :locals prefix)]
           (if lb
             (symbol (str (:name lb) suffix))
             sym))

         :else
         (let [full-ns (-> env :ns :name)
               _ (assert full-ns)]
           (symbol (str full-ns "/" (name sym)))))]
    {:name nm}))

(declare analyze analyze-symbol analyze-seq)

(def ^:dynamic *specials* '#{defmacro if def fn* do let* loop* throw try* recur new set! ns deftype* defrecord* . & quote})

(def ^:dynamic *recur-frames* nil)

(defmacro disallowing-recur [& body]
  `(binding [*recur-frames* (cons nil *recur-frames*)] ~@body))

(defn analyze-block
  "returns {:statements .. :ret .. :children ..}"
  [env exprs]
  (let [statements (disallowing-recur
                     (seq (map #(analyze (assoc env :context :statement) %) (butlast exprs))))
        ret (if (<= (count exprs) 1)
              (analyze env (first exprs))
              (analyze (assoc env :context (if (= :statement (:context env)) :statement :return)) (last exprs)))]
    {:statements statements :ret ret :children (vec (cons ret statements))}))

(defmulti parse (fn [op & rest] op))

(defmethod parse 'defmacro
  [op env form name]
  {:env env :op :defmacro :form form})

(defmethod parse 'if
  [op env [_ test then else :as form] name]
  (let [test-expr (disallowing-recur (analyze (assoc env :context :expr) test))
        then-expr (analyze env then)
        else-expr (analyze env else)]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :children [test-expr then-expr else-expr]}))

(defmethod parse 'throw
  [op env [_ throw :as form] name]
  (let [throw-expr (disallowing-recur (analyze (assoc env :context :expr) throw))]
    {:env env :op :throw :form form
     :throw throw-expr
     :children [throw-expr]}))

(defmethod parse 'try*
  [op env [_ & body :as form] name]
  (let [body (vec body)
        catchenv (update-in env [:context] #(if (= :expr %) :return %))
        tail (peek body)
        fblock (when (and (seq? tail) (= 'finally (first tail)))
                  (rest tail))
        finally (when fblock
                  (analyze-block
                   (assoc env :context :statement)
                   fblock))
        body (if finally (pop body) body)
        tail (peek body)
        cblock (when (and (seq? tail)
                          (= 'catch (first tail)))
                 (rest tail))
        name (first cblock)
        locals (:locals catchenv)
        locals (if name
                 (assoc locals name {:name name})
                 locals)
        catch (when cblock
                (analyze-block (assoc catchenv :locals locals) (rest cblock)))
        body (if name (pop body) body)
        try (when body
              (analyze-block (if (or name finally) catchenv env) body))]
    (when name (assert (not (namespace name)) "Can't qualify symbol in catch"))
    {:env env :op :try* :form form
     :try try
     :finally finally
     :name name
     :catch catch
     :children [try {:name name} catch finally]}))

(defmethod parse 'def
  [op env form name]
  (let [pfn (fn ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form)
        sym (:sym args)]
    (assert (not (namespace sym)) "Can't def ns-qualified name")
    (let [name (:name (resolve-var (dissoc env :locals) sym))
          init-expr (when (contains? args :init) (disallowing-recur
                                                  (analyze (assoc env :context :expr) (:init args) sym)))
          export-as (when-let [export-val (-> sym meta :export)]
                      (if (= true export-val) name export-val))
          doc (or (:doc args) (-> sym meta :doc))]
      (swap! namespaces assoc-in [(-> env :ns :name) :defs sym] name)
      (merge {:env env :op :def :form form
              :name name :doc doc :init init-expr}
             (when init-expr {:children [init-expr]})
             (when export-as {:export export-as})))))

(defn- analyze-fn-method [env locals meth]
  (let [params (first meth)
        variadic (boolean (some '#{&} params))
        params (remove '#{&} params)
        fixed-arity (count (if variadic (butlast params) params))
        body (next meth)
        locals (reduce (fn [m name] (assoc m name {:name name})) locals params)
        recur-frame {:names (vec params) :flag (atom nil)}
        block (binding [*recur-frames* (cons recur-frame *recur-frames*)]
                (analyze-block (assoc env :context :return :locals locals) body))]
    
    (merge {:env env :variadic variadic :params params :max-fixed-arity fixed-arity :recurs @(:flag recur-frame)} block)))

(defmethod parse 'fn*
  [op env [_ & args] name]
  (let [[name meths] (if (symbol? (first args))
                       [(first args) (next args)]
                       [name (seq args)])
        ;;turn (fn [] ...) into (fn ([]...))
        meths (if (vector? (first meths)) (list meths) meths)
        locals (:locals env)
        locals (if name (assoc locals name {:name name}) locals)
        menv (if (> (count meths) 1) (assoc env :context :expr) env)
        methods (map #(analyze-fn-method menv locals %) meths)
        max-fixed-arity (apply max (map :max-fixed-arity methods))
        variadic (boolean (some :variadic methods))]
    ;;(assert (= 1 (count methods)) "Arity overloading not yet supported")
    ;;todo - validate unique arities, at most one variadic, variadic takes max required args
    {:env env :op :fn :name name :methods methods :variadic variadic :recur-frames *recur-frames*
     :max-fixed-arity max-fixed-arity}))

(defmethod parse 'do
  [op env [_ & exprs] _]
  (merge {:env env :op :do} (analyze-block env exprs)))

(defn analyze-let
  [encl-env [_ bindings & exprs :as form] is-loop]
  (assert (and (vector? bindings) (even? (count bindings))) "bindings must be vector of even number of elements")
  (let [context (:context encl-env)
        [bes env]
        (disallowing-recur
         (loop [bes []
                env (assoc encl-env :context :expr)
                bindings (seq (partition 2 bindings))]
           (if-let [[name init] (first bindings)]
             (do
               (assert (not (or (namespace name) (.contains (str name) "."))) (str "Invalid local name: " name))
               (let [init-expr (analyze env init)
                     be {:name name :init init-expr}]
                 (recur (conj bes be)
                        (assoc-in env [:locals name] be)
                        (next bindings))))
             [bes env])))
        recur-frame (when is-loop {:names (vec (map :name bes)) :flag (atom nil)})
        {:keys [statements ret children]}
        (binding [*recur-frames* (if recur-frame (cons recur-frame *recur-frames*) *recur-frames*)]
          (analyze-block (assoc env :context (if (= :expr context) :return context)) exprs))]
    {:env encl-env :op :let :loop is-loop
     :bindings bes :statements statements :ret ret :form form :children (into (vec children) (map :init bes))}))

(defmethod parse 'let*
  [op encl-env form _]
  (analyze-let encl-env form false))

(defmethod parse 'loop*
  [op encl-env form _]
  (analyze-let encl-env form true))

(defmethod parse 'recur
  [op env [_ & exprs] _]
  (let [context (:context env)
        frame (first *recur-frames*)]
    (assert frame "Can't recur here")
    (assert (= (count exprs) (count (:names frame))) "recur argument count mismatch")
    (reset! (:flag frame) true)
    (assoc {:env env :op :recur}
      :frame frame
      :exprs (disallowing-recur (vec (map #(analyze (assoc env :context :expr) %) exprs))))))

(defmethod parse 'quote
  [_ env [_ x] _]
  {:op :constant :env env :form x})

(defmethod parse 'new
  [_ env [_ ctor & args] _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         ctorexpr (analyze enve ctor)
         argexprs (vec (map #(analyze enve %) args))]
     {:env env :op :new :ctor ctorexpr :args argexprs :children (conj argexprs ctorexpr)})))

(defmethod parse 'set!
  [_ env [_ target val] _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         targetexpr (if (symbol? target)
                      (do
                        (assert (nil? (-> env :locals target))
                                "Can't set! local var")
                        (analyze-symbol enve target))
                      (when (seq? target)
                        (let [targetexpr (analyze-seq enve target nil)]
                          (when (:field targetexpr)
                            targetexpr))))
         valexpr (analyze enve val)]
     (assert targetexpr "set! target must be a field or a symbol naming a var")
     {:env env :op :set! :target targetexpr :val valexpr :children [targetexpr valexpr]})))

(declare analyze-namespace)

(defmethod parse 'ns
  [_ env [_ name & args] _]
  (let [args (if (string? (first args))
               (rest args)
               args)
        excludes
        (reduce (fn [s [k exclude xs]]
                  (if (= k :refer-clojure)
                    (do
                      (assert (= exclude :exclude) "Only [:refer-clojure :exclude [names]] form supported") ;TODO :only, :rename keys
                      (into s xs))
                    s))
                #{} args)
        {imports :import uses :use requires :require :as params}
        (reduce (fn [m [k & libs]]
                  (assert (#{:import :use :refer-clojure :require} k)
                          (str "Only :import, :refer-clojure, :require and :use libspecs supported, found " k name))
                  (assoc m k (into {}
                                   (mapcat (fn [form]
                                             (case k
                                               ;; Supports (:import ns.class), (:import [ns.class]), (:import [ns class1 class2])
                                               :import
                                               (let [form (if (symbol? form) [form] form)
                                                     [prefix & suffixes] form]
                                                 (if (seq suffixes)
                                                   (map #(let [cobj (resolve (symbol (str prefix "." %)))
                                                               _ (assert cobj)]
                                                           [% cobj])
                                                        suffixes)
                                                   (let [ssym (str prefix)
                                                         cobj (resolve prefix)
                                                         _ (assert cobj)]
                                                     [[(symbol (subs ssym (inc (.lastIndexOf ssym "."))))
                                                       cobj]])))

                                               ;; Supports (:require my.ns), (:require [my.ns]), (:require [my.ns :as myalias])
                                               ;TODO prefix-suffixes forms
                                               :require
                                               (if (symbol? form)
                                                 [[form form]]
                                                 (let [[lib kw expr] form]
                                                   (assert (or (not kw)
                                                               (and expr (= :as kw)))
                                                           "Unsupported :require syntax")
                                                   (if kw
                                                     [[expr lib]]
                                                     [[expr expr]])))

                                               ;; Supports (:use my.ns), (:use [my.ns]), (:use [my.ns :only [var1 var2]])
                                               ;TODO prefix-suffixes forms
                                               :use
                                               (if (symbol? form) 
                                                 [[form form]]
                                                 (let [[lib kw expr] form]
                                                   (assert (or (not kw)
                                                               (and expr (= :only kw)))
                                                           "Unsupported :use syntax")
                                                   (if kw
                                                     (map vector expr (repeat lib))
                                                     (map vector (keys (ns-publics lib)) (repeat lib)))))))
                                           libs))))
                {} (remove (fn [[r]] (= r :refer-clojure)) args))
        imports (merge (into {} (filter #(re-seq #"java.lang.*" (str (val %))) (ns-imports 'clojure.core))) imports)]
    (set! *analyzer-ns* name)
    (doseq [nsym (set (concat (vals requires) (vals uses)))]
      (require nsym))
    (swap! namespaces #(-> %
                           (assoc-in [name :name] name)
                           (assoc-in [name :excludes] excludes)
                           (assoc-in [name :imports] imports)
                           (assoc-in [name :uses] uses)
                           (assoc-in [name :requires] requires)))
    {:env env :op :ns :name name :excludes excludes :imports imports :uses uses :requires requires}))

(defmethod parse 'deftype*
  [_ env [_ tsym fields] _]
  (let [t (:name (resolve-var (dissoc env :locals) tsym))]
    (swap! namespaces assoc-in [(-> env :ns :name) :defs tsym] t)
    {:env env :op :deftype* :t t :fields fields}))

(defmethod parse 'defrecord*
  [_ env [_ tsym fields] _]
  (let [t (:name (resolve-var (dissoc env :locals) tsym))]
    (swap! namespaces assoc-in [(-> env :ns :name) :defs tsym] t)
    {:env env :op :defrecord* :t t :fields fields}))

(defmethod parse '.
  [_ env [_ target & member+] _]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         targetexpr (analyze enve target)
         children [enve]]
     (if (and (symbol? (first member+)) (nil? (next member+))) ;;(. target field)
       {:env env :op :dot :target targetexpr :field (first member+) :children children}
       (let [[method args]
             (if (symbol? (first member+))
               [(first member+) (next member+)]
               [(ffirst member+) (nfirst member+)])
             argexprs (map #(analyze enve %) args)]
         {:env env :op :dot :target targetexpr :method method :args argexprs :children (into children argexprs)})))))

(defn parse-invoke
  [env [f & args]]
  (disallowing-recur
   (let [enve (assoc env :context :expr)
         fexpr (analyze enve f)
         argexprs (vec (map #(analyze enve %) args))]
     {:env env :op :invoke :f fexpr :args argexprs :children (conj argexprs fexpr)})))

(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (let [ret {:env env :form sym}]
    (merge ret (resolve-existing env sym))))

;; TODO look at this again
(defn get-expander [sym env]
  ;; TODO ensure "sym" actually points to either a Class or a var
  (let [imported-class (if-let [nsym (namespace sym)]
                         (or (-> env :ns :imports (get (symbol nsym)))
                             (try
                               (class? (resolve (symbol nsym)))
                               (catch RuntimeException e)))
                         (or (-> env :ns :imports (get (symbol sym)))
                             (try
                               (class? (resolve sym))
                               (catch RuntimeException e))))

        is-implicit (fn [^clojure.lang.Symbol csym]
                      (let [c (resolve (symbol csym))]
                        (and (class? c)
                             (.startsWith (str c) "java.lang"))))

        implicit-import (if-let [nsym (namespace sym)]
                          (try
                            (is-implicit (symbol nsym))
                            (catch RuntimeException e
                              nil))
                          (try
                            (is-implicit (symbol sym))
                            (catch RuntimeException e
                              nil)))
        mvar
        (when (and (not imported-class)
                   (not implicit-import)
                   (not (-> env :locals sym)))  ;locals hide macros
          (if-let [nstr (namespace sym)]
            ;; Could be a class
            (when-let [ns (cond
                            (= "clojure.core" nstr) 'clojure.core
                            (.contains nstr ".") (symbol nstr)
                            :else
                            (or (-> env :ns :requires-macros (get (symbol nstr)))
                                (-> env :ns :requires (get (symbol nstr)))))]
              (.findInternedVar ^clojure.lang.Namespace (find-ns ns) (symbol (name sym))))
            (if-let [nsym (or (-> env :ns :uses-macros sym)
                              (-> env :ns :uses sym))]
              (when-let [ns (find-ns nsym)]
                (.findInternedVar ^clojure.lang.Namespace (find-ns nsym) sym))
              (.findInternedVar ^clojure.lang.Namespace (find-ns 'clojure.core) sym))))]
    (when (and mvar (.isMacro ^clojure.lang.Var mvar))
      @mvar)))

(defn macroexpand-1 [env form]
  (let [op (first form)]
    (if (*specials* op)
      form
      (if-let [mac (and (symbol? op) (get-expander op env))]
        (binding [*ns* (if-let [ns (-> env :ns :name)]
                         (find-ns ns)
                         *ns*)]
          (apply mac form env (rest form)))
        (if (symbol? op)
          (let [opname (str op)]
            (cond
             (= (first opname) \.) (let [[target & args] (next form)]
                                     (list* '. target (symbol (subs opname 1)) args))
             (= (last opname) \.) (list* 'new (symbol (subs opname 0 (dec (count opname)))) (next form))
             :else form))
          form)))))

(defn analyze-seq
  [env form name]
  (let [env (assoc env :line (-> form meta :line))]
    (let [op (first form)]
      (assert (not (nil? op)) "Can't call nil")
      (let [mform (macroexpand-1 env form)]
        (if (identical? form mform)
          (if (*specials* op)
            (parse op env form name)
            (parse-invoke env form))
          (analyze env mform name))))))

(declare analyze-wrap-meta)

(defn analyze-map
  [env form name]
  (let [expr-env (assoc env :context :expr)
        simple-keys? (every? #(or (string? %) (keyword? %))
                             (keys form))
        ks (disallowing-recur (vec (map #(analyze expr-env % name) (keys form))))
        vs (disallowing-recur (vec (map #(analyze expr-env % name) (vals form))))]
    (analyze-wrap-meta {:op :map :env env :form form :children (vec (concat ks vs))
                        :keys ks :vals vs :simple-keys? simple-keys?}
                       name)))

(defn analyze-vector
  [env form name]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env % name) form)))]
    (analyze-wrap-meta {:op :vector :env env :form form :children items} name)))

(defn analyze-set
  [env form name]
  (let [expr-env (assoc env :context :expr)
        items (disallowing-recur (vec (map #(analyze expr-env % name) form)))]
    (analyze-wrap-meta {:op :set :env env :form form :children items} name)))

(defn analyze-wrap-meta [expr name]
  (let [form (:form expr)]
    (if (meta form)
      (let [env (:env expr) ; take on expr's context ourselves
            expr (assoc-in expr [:env :context] :expr) ; change expr to :expr
            meta-expr (analyze-map (:env expr) (meta form) name)]
        {:op :meta :env env :form form :children [meta-expr expr]
         :meta meta-expr :expr expr})
      expr)))

(defn analyze
  "Given an environment, a map containing {:locals (mapping of names to bindings), :context
  (one of :statement, :expr, :return), :ns (a symbol naming the
  compilation ns)}, and form, returns an expression object (a map
  containing at least :form, :op and :env keys). If expr has any (immediately)
  nested exprs, must have :children [exprs...] entry. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (analyze env form nil))
  ([env form name]
     (let [form (if (instance? clojure.lang.LazySeq form)
                  (or (seq form) ())
                  form)]
       (cond
        (symbol? form) (analyze-symbol env form)
        (and (seq? form) (seq form)) (analyze-seq env form name)
        (map? form) (analyze-map env form name)
        (vector? form) (analyze-vector env form name)
        (set? form) (analyze-set env form name)
        :else {:op :constant :env env :form form}))))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (java.io.PushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (read rdr nil nil)]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defmacro with-core-clj
  "Ensure that clojure.core has been loaded."
  [[nssym] & body]
  `(do (when (not= ~nssym 'clojure.core)
         (doseq [[defsym# _#] (ns-publics 'clojure.core)]
           (swap! namespaces assoc-in ['clojure.core :defs defsym#] (symbol "clojure.core" (name defsym#)))))
       ~@body))

(defn analyze-namespace [nssym]
  (require nssym) ;; require macroexpanders
  (reset-namespaces!)
  (with-core-clj [nssym]
    (binding [*analyzer-ns* 'clojure.user]
      (let [file-name (-> (ns-publics nssym) first second meta :file) ;; TODO better way to get file name
            _ (assert file-name)
            res (.getResource (RT/baseLoader) file-name)
            _ (assert res) 
            strm (.getResourceAsStream (RT/baseLoader) file-name)]
        (with-open [rdr (PushbackReader. (InputStreamReader. strm))]
          (doall
            (map #(let [env {:ns (@namespaces *analyzer-ns*) :context :statement :locals {}}]
                    (analyze env %))
                 (forms-seq nil rdr))))))))

(defmacro with-specials [specials & body]
  `(binding [*specials* (set (concat ~specials analyze/*specials*))]
     ~@body))
