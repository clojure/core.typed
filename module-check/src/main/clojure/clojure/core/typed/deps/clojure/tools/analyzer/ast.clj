;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.ast
  "Utilities for AST walking/updating"
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [into! rseqv]]))

(defn cycling
  "Combine the given passes in a single pass that will be applieed repeatedly
   to the AST until applying it another time will have no effect"
  [& fns*]
  (let [fns (cycle fns*)]
    (fn [ast]
      (loop [[f & fns] fns ast ast res (zipmap fns* (repeat nil))]
        (let [ast* (f ast)]
          (if (= ast* (res f))
            ast
            (recur fns ast* (assoc res f ast*))))))))

(defn children*
  "Return a vector of vectors of the children node key and the children expression
   of the AST node, if it has any.
   The returned vector returns the childrens in the order as they appear in the
   :children field of the AST, and the children expressions may be either a node
   or a vector of nodes."
  [{:keys [children] :as ast}]
  (when children
    (mapv #(find ast %) children)))

(defn children
  "Return a vector of the children expression of the AST node, if it has any.
   The children expressions are kept in order and flattened so that the returning
   vector contains only nodes and not vectors of nodes."
  [ast]
  (persistent!
   (reduce (fn [acc [_ c]] ((if (vector? c) into! conj!) acc c))
           (transient []) (children* ast))))

(defmulti -update-children   (fn [ast f] (:op ast)))
(defmulti -update-children-r (fn [ast f] (:op ast)))

(defmethod -update-children :default
  [ast f]
  (persistent!
   (reduce (fn [ast [k v]]
             (assoc! ast k (if (vector? v) (mapv f v) (f v))))
           (transient ast)
           (children* ast))))

(defmethod -update-children-r :default
  [ast f]
  (persistent!
   (reduce (fn [ast [k v]]
             (assoc! ast k (if (vector? v) (rseqv (mapv f (rseq v))) (f v))))
           (transient ast)
           (rseq (children* ast)))))

(defn update-children
  "Applies `f` to each AST children node, replacing it with the returned value.
   If reversed? is not-nil, `pre` and `post` will be applied starting from the last
   children of the AST node to the first one."
  ([ast f] (update-children ast f false))
  ([ast f reversed?]
     (if (:children ast)
       (if reversed?
         (-update-children-r ast f)
         (-update-children   ast f))
       ast)))

(defn walk
  "Walk the ast applying `pre` when entering the nodes, and `post` when exiting.
   Both functions must return a valid node since the returned value will replace
   the node in the AST which was given as input to the function.
   If reversed? is not-nil, `pre` and `post` will be applied starting from the last
   children of the AST node to the first one."
  ([ast pre post]
     (walk ast pre post false))
  ([ast pre post reversed?]
     (let [walk #(walk % pre post reversed?)]
       (post (update-children (pre ast) walk reversed?)))))

(defn prewalk
  "Shorthand for (walk ast f identity)"
  [ast f]
  (let [walk #(prewalk % f)]
    (update-children (f ast) walk)))

(defn postwalk
  "Shorthand for (walk ast identity f reversed?)"
  ([ast f]
     (postwalk ast f false))
  ([ast f reversed?]
     (let [walk #(postwalk % f reversed?)]
       (f (update-children ast walk reversed?)))))

(defn nodes
  "Returns a lazy-seq of all the nodes in the given AST, in depth-first pre-order."
  [ast]
  (lazy-seq
   (cons ast (mapcat nodes (children ast)))))

(defn ast->eav
  "Returns an EAV representation of the current AST that can be used by
   Datomic's Datalog."
  [ast]
  (let [children (set (:children ast))]
    (mapcat (fn [[k v]]
              (if (children k)
                (if (map? v)
                  (into [[ast k v]] (ast->eav v))
                  (mapcat (fn [v] (into [[ast k v]] (ast->eav v))) v))
                [[ast k v]])) ast)))
