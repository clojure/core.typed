;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.source-info
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [-source-info]]
            [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [update-children]]))

(defn -merge-source-info [source-info]
  (fn [ast]
    (update-in ast [:env] merge source-info)))

(defn source-info
  "Adds (when avaliable) :line, :column, :end-line, :end-column and :file info to the AST :env"
  [ast]
  (let [source-info (-source-info (:form ast) (:env ast))
        merge-source-info (-merge-source-info source-info)]
    (update-children (merge-source-info ast) merge-source-info)))
