;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; copied from clojure.tools.analyzer.passes.jvm.constant-lifter
(ns clojure.core.typed.analyzer.jvm.passes.constant-lifter
  (:require [clojure.core.typed.analyzer.common :as ana2]
            [clojure.core.typed.analyzer.common.passes.constant-lifter :as orig]
            [clojure.core.typed.analyzer.common.passes.elide-meta :as elide-meta]
            [clojure.core.typed.analyzer.common.utils :as cu]
            [clojure.core.typed.analyzer.jvm.passes.analyze-host-expr :as analyze-host-expr]))

(defn constant-lift*
  [ast]
  (if (= :var (:op ast))
    (let [{:keys [var env form meta]} ast]
     (if (cu/constant? var meta)
       (let [val @var]
         (assoc (ana2/analyze-const val env (cu/classify val))
           :form form))
       ast))
    (orig/constant-lift ast)))

(defn constant-lift
  "Like clojure.core.typed.analyzer.common.passes.constant-lifter/constant-lift but
   transforms also :var nodes where the var has :const in the metadata
   into :const nodes and preserves tag info"
  {:pass-info {:walk :post :depends #{} :after #{#'elide-meta/elide-meta #'analyze-host-expr/analyze-host-expr}}}
  [ast]
  (merge (constant-lift* ast)
         (select-keys ast [:tag :o-tag :return-tag :arglists])))

