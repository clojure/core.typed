;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.index-vector-nodes)

(defn index-vector-nodes
  "Adds an :idx attribute to nodes in a vector children, representing the position
   of the node vector."
  {:pass-info {:walk :any :depends #{}}}
  [ast]
  (merge ast
         (reduce (fn [m c]
                   (let [v (c ast)
                         v (if (vector? v)
                             (mapv (fn [x i] (assoc x :idx i ))
                                   v (range))
                             v)]
                     (assoc m c v))) {} (:children ast))))
