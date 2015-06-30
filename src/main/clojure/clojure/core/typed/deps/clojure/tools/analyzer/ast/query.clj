;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.ast.query
  "Utilities for querying tools.analyzer ASTs with Datomic"
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.ast :as ast]
            [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [compile-if]]))

(defn query-map
  "Transoforms a Datomic query from its vector representation to its map one.
   If the given query is already in its map representation, the original query
   is returned."
  [query]
  (if (map? query)
    query
    (loop [ret {:find [] :in [] :where []} query query op nil]
      (if (seq query)
        (let [[el & query] query]
          (if (keyword? el)
            (recur ret query el)
            (recur (update-in ret [op] conj el) query op)))
        (reduce-kv (fn [m k v] (if (seq v) (assoc m k v) m)) {} ret)))))

(defn unfold-expression-clauses
  "Given a Datomic query, walk the :where clauses searching for
   expression clauses with nested calls, unnesting those calls.

   E.g {:where [[(inc (dec ?foo)) ?bar] ..] ..} will be transformed in
   {:where [[(dec ?foo) ?1234] [(inc ?1234) ?bar] ..] ..}"
  [query]
  (let [{:keys [where] :as query} (query-map query)]
    (if-not where
      query
      (assoc query :where
             (mapcat (fn [[op & rest :as form]]
                       (if-let [[f & args] (and (seq? op) op)]
                         (if (some seq? args)
                           (loop [args args to-ssa {} cur [f] binds rest ret []]
                             (if (seq args)
                               (let [[a & args] args]
                                 (if (and (seq? a)
                                          (not= 'quote (first a)))
                                   (let [g (gensym "?")]
                                     (recur args (assoc to-ssa g a) (conj cur g) binds ret))
                                   (recur args to-ssa (conj cur a) binds ret)))
                               (let [ret (conj ret (into [(seq cur)] binds))]
                                 (if (seq to-ssa)
                                   (let [[k [f & args]] (first to-ssa)]
                                     (recur args (dissoc to-ssa k) [f] [k] ret))
                                   ret))))
                           [form])
                         [form])) where)))))

(defn resolve-calls
  "Automatically replace fn name symbols in expression clauses with
   their namespace qualified one if the symbol can be resolved in the
   current namespace."
    [query]
    (let [{:keys [where] :as query} (query-map query)]
      (if-not where
        query
        (assoc query :where
               (mapv (fn [[op & rest :as form]]
                       (if-let [[f & args] (and (seq? op) op)]
                         (if-let [f-var (and (symbol? f) (resolve f))]
                           (into [(seq (into [(symbol (str (ns-name (.ns f-var)))
                                                      (str (.sym f-var)))] args))]
                                 rest)
                           form)
                         form)) where)))))

(defn db
  "Given a list of ASTs, returns a representation of those
   that can be used as a database in a Datomic Datalog query"
  [asts]
  (mapcat ast/ast->eav asts))

(defn q
  "Execute a Datomic Datalog query against the ASTs.
   The first input is always assumed to be an AST database, if more
   are required, it's required to call `db` on them.
   `unfold-expression-clauses` is automatically applied to the
   query."
  [query asts & inputs]
  (compile-if (Class/forName "datomic.Datom")
    (do (require '[datomic.api :as d])
        (apply (resolve 'datomic.api/q) (unfold-expression-clauses query) (db asts) inputs))
    (throw (Exception. "Datomic is required"))))
