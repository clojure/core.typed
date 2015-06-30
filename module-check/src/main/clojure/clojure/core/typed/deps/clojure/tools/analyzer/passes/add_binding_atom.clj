;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.add-binding-atom
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [prewalk]]))

(def ^:dynamic ^:private *bindings*)

(defn ^:private -add-binding-atom
  [ast]
  (case (:op ast)
    :binding
    (let [a (atom {})]
      (swap! *bindings* assoc (:name ast) a)
      (assoc ast :atom a))
    :local
    (assoc ast :atom (or (@*bindings* (:name ast))
                         (atom {})))
    ast))

(defn add-binding-atom
  "Walks the AST and adds an atom-backed-map to every local binding,
   the same atom will be shared between all occurences of that local.

   The atom is put in the :atom field of the node."
  [ast]
  (binding [*bindings* (atom {})]
    (prewalk ast -add-binding-atom)))
