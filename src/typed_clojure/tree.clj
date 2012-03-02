(ns typed-clojure.tree
  (:use [typed-clojure.test-rep]))

(declare Tree)

(deftypeT Leaf
  [[val :- Number]])

(deftypeT Node
  [[left :- Tree]
   [right :- Tree]])

(new-type Tree (union Leaf Node))

(+T tree-height [Tree :-> Number])
(defn tree-height [t]
  (cond 
    (instance? Leaf t) 1
    :else (max (+ 1 (tree-height (.left t)))
               (+ 1 (tree-height (.right t))))))
