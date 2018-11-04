;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; copied from tools.analyzer.js
(ns clojure.core.typed.analyzer2.passes.js.collect-keywords
  (:require [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.passes.elide-meta :refer [elide-meta]]))

(defn collect-keywords
  "Assoc compilation-unit shared id to each :const node with :type :keyword,
   The keyword to id map is available in the global env under ::keywords"
  {:pass-info {:walk :any :depends #{#'elide-meta}}}
  [ast]
  (if (and (= (:op ast) :const)
           (= (:type ast) :keyword))
    (let [v (:val ast)
          id (or (get-in (env/deref-env) [::keywords v])
                 (let [c (count (::keywords (env/deref-env)))]
                   (swap! env/*env* assoc-in [::keywords v] c)
                   c))]
      (assoc ast :id id))
    ast))
