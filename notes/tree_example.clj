(ns typed-clojure.tree
  (:use [typed-clojure.checker :only [deftypeT +T new-type union fun arity]]))

(declare Tree)

(deftypeT Leaf
  [[val :- Number]])

(deftypeT Node
  [[left :- Tree]
   [right :- Tree]])

(new-type Tree (union Leaf Node))

(+T tree-height (fun (arity [Tree] Number)))
(defn tree-height [t]
  (cond 
    (instance? Leaf t) 1
    :else (max (+ 1 (tree-height (.left t)))
               (+ 1 (tree-height (.right t))))))
