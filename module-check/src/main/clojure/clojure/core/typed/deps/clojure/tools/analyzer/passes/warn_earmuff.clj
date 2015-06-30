;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.warn-earmuff
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [dynamic?]]))

(defn warn-earmuff
  "Prints a warning to *err* if the AST node is a :def node and the
   var name contains earmuffs but the var is not marked dynamic"
  [ast]
  (let [name (str (:name ast))]
    (when (and (= :def (:op ast))
               (> (count name) 2)  ;; Allow * and ** as non-dynamic names
               (.startsWith name "*")
               (.endsWith name "*")
               (not (dynamic? (:var ast))))
      (binding [*out* *err*]
        (println "Warning:" name "not declared dynamic and thus is not dynamically rebindable,"
                 "but its name suggests otherwise."
                 "Please either indicate ^:dynamic" name "or change the name"))))
  ast)
