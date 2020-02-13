;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from clojure.tools.analyzer.ast
(ns clojure.core.typed.analyzer.common.ast
  "Utilities for AST walking/updating"
  (:refer-clojure :exclude [unreduced])
  (:require [clojure.core.typed.analyzer.common.utils :refer [into! rseqv mapv']]))

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

;; return transient or reduced holding transient
(defn ^:private -update-children
  [ast f r?]
  (let [fix (if r? rseqv identity)]
    (reduce (fn [ast [k v]]
              (let [multi (vector? v)
                    val (if multi (mapv' f (fix v)) (f v))]
                (if (reduced? val)
                  (reduced (reduced (assoc! ast k (if multi (fix @val) @val))))
                  (assoc! ast k (if multi (fix val) val)))))
            (transient ast)
            (fix (children* ast)))))

(defn update-children-reduced
  "Like update-children but returns a reduced holding the AST if f short-circuited."
  ([ast f] (update-children-reduced ast f false))
  ([ast f reversed?]
     (if (and (not (reduced? ast))
              (:children ast))
       (let [ret (-update-children ast f reversed?)]
         (if (reduced? ret)
           (reduced (persistent! @ret))
           (persistent! ret)))
       ast)))

(defn ^:private unreduced [x]
  (if (reduced? x)
    @x
    x))

(defn update-children
  "Applies `f` to each AST children node, replacing it with the returned value.
   If reversed? is not-nil, `pre` and `post` will be applied starting from the last
   children of the AST node to the first one.
   Short-circuits on reduced."
  ([ast f] (update-children ast f false))
  ([ast f reversed?]
     (unreduced (update-children-reduced ast f reversed?))))

(defn walk
  "Walk the ast applying `pre` when entering the nodes, and `post` when exiting.
   Both functions must return a valid node since the returned value will replace
   the node in the AST which was given as input to the function.
   If reversed? is not-nil, `pre` and `post` will be applied starting from the last
   children of the AST node to the first one.
   Short-circuits on reduced."
  ([ast pre post]
     (walk ast pre post false))
  ([ast pre post reversed?]
     (unreduced
      ((fn walk [ast pre post reversed?]
         (let [walk #(walk % pre post reversed?)]
           (if (reduced? ast)
             ast
             (let [ret (update-children-reduced (pre ast) walk reversed?)]
               (if (reduced? ret)
                 ret
                 (post ret))))))
       ast pre post reversed?))))

(defn prewalk
  "Shorthand for (walk ast f identity)"
  [ast f]
  (walk ast f identity))

(defn postwalk
  "Shorthand for (walk ast identity f reversed?)"
  ([ast f]
     (postwalk ast f false))
  ([ast f reversed?]
     (walk ast identity f reversed?)))

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
