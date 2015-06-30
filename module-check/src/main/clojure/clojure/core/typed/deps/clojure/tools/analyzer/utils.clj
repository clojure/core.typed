;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.utils
  (:refer-clojure :exclude [record?])
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.env :as env])
  (:import (clojure.lang IRecord IType IObj
                         IReference Var)
           java.util.regex.Pattern))

(defn into!
  "Like into, but for transients"
  [to from]
  (reduce conj! to from))

(defn rseqv
  "Same as (comp vec rseq)"
  [v]
  (vec (rseq v)))

(defn ctx
  "Returns a copy of the passed environment with :context set to ctx"
  [env ctx]
  (assoc env :context ctx))

(defn dissoc-env
  "Dissocs :env from the ast"
  [ast]
  (dissoc ast :env))

(defn update-vals
  "Applies f to all the vals in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} (or m {})))

(defn update-keys
  "Applies f to all the keys in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m (f k) v)) {} (or m {})))

(defn update-kv
  "Applies f to all the keys and vals in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m (f k) (f v))) {} (or m {})))

(defn record?
  "Returns true if x is a record"
  [x]
  (instance? IRecord x))
(defn type?
  "Returns true if x is a type"
  [x]
  (instance? IType x))
(defn obj?
  "Returns true if x implements IObj"
  [x]
  (instance? IObj x))
(defn reference?
  "Returns true if x implements IReference"
  [x]
  (instance? IReference x))
(defn regex?
  "Returns true if x is a regex"
  [x]
  (instance? Pattern x))
(defn boolean?
  "Returns true if x is a boolean"
  [x]
  (or (true? x) (false? x)))

(defn classify
  "Returns a keyword describing the form type"
  [form]
  (cond
   (nil? form)     :nil
   (boolean? form) :bool
   (keyword? form) :keyword
   (symbol? form)  :symbol
   (string? form)  :string
   (number? form)  :number
   (type? form)    :type
   (record? form)  :record
   (map? form)     :map
   (vector? form)  :vector
   (set? form)     :set
   (seq? form)     :seq
   (char? form)    :char
   (regex? form)   :regex
   (class? form)   :class
   (var? form)     :var
   :else           :unknown))

(defn private?
  "Returns true if the var is private"
  [var]
  (:private (meta var)))
(defn macro?
  "Returns true if the var maps to a macro"
  [var]
  (:macro (meta var)))
(defn constant?
  "Returns true if the var is a const"
  [var]
  (:const (meta var)))
(defn dynamic?
  "Returns true if the var is dynamic"
  [var]
  (or (:dynamic (meta var))
      (when (var? var) ;; workaround needed since Clojure doesn't always propagate :dynamic
        (.isDynamic ^Var var))))
(defn protocol-node?
  "Returns true if the var maps to a protocol function"
  [var]
  (boolean (:protocol (meta var)))) ;; conveniently this is true in both clojure and clojurescript

(defn resolve-ns
  "Resolves the ns mapped by the given sym in the env"
  [ns-sym {:keys [ns]}]
  (when ns-sym
    (let [namespaces (:namespaces (env/deref-env))]
      (or (get-in namespaces [ns :aliases ns-sym])
          (:ns (namespaces ns-sym))))))

(defn resolve-var
  "Resolves the var mapped by the given sym in the env"
  [sym {:keys [ns] :as env}]
  (when (symbol? sym)
    (let [sym-ns (when-let [ns (namespace sym)]
                   (symbol ns))
          full-ns (resolve-ns sym-ns env)]
      (when (or (not sym-ns) full-ns)
        (let [name (if sym-ns (-> sym name symbol) sym)]
          (-> (env/deref-env) :namespaces (get (or full-ns ns)) :mappings (get name)))))))

(defn arglist-for-arity
  "Takes a fn node and an argc and returns the matching arglist"
  [fn argc]
  (let [arglists (->> fn :arglists (sort-by count))
        arglist (->> arglists (filter #(= argc (count %))) first)
        last-arglist (last arglists)]
    (or arglist
        (when (and (some '#{&} last-arglist)
                   (>= argc (- (count last-arglist) 2)))
          last-arglist))))

(defn get-line
  "Returns the line number of x"
  [x env]
  (-> x meta :line))

(defn get-end-line
  "Returns the end line number of x"
  [x env]
  (-> x meta :end-line))

(defn get-col
  "Returns the column number of x"
  [x env]
  (-> x meta :column))

(defn get-end-column
  "Returns the end column number of x"
  [x env]
  (-> x meta :end-column))

(defn source-info
  "Returns the source-info from an env"
  [env]
  (select-keys env #{:file :line :column}))

(defn -source-info
  "Returns the source-info of x"
  [x env]
  (merge
   (source-info env)
   (when-let [file (or (-> x meta :file)
                       (and (not= *file* "NO_SOURCE_FILE")
                            *file*))]
     {:file file})
   (when-let [line (get-line x env)]
     {:line line})
   (when-let [column (get-col x env)]
     {:column column})
   (when-let [end-line (get-end-line x env)]
     {:end-line end-line})
   (when-let [end-column (get-end-column x env)]
     {:end-column end-column})))

(defn const-val
  "Returns the value of a constant node (either :quote or :const)"
  [{:keys [op val expr]}]
  (if (= :quote op)
    (:val expr)
    val))

(defmacro compile-if
  [exp then & else]
  (if (try (eval exp)
           (catch Throwable _ false))
    `(do ~then)
    `(do ~@else)))
