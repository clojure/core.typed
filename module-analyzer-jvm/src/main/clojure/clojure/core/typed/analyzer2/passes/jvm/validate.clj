;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.analyzer2.passes.jvm.validate
  (:require [clojure.tools.analyzer.passes.jvm.validate :as validate]
            [clojure.tools.analyzer.passes.jvm.analyze-host-expr :as analyze-host-expr]
            [clojure.core.typed.analyzer2.passes.jvm.infer-tag :as infer-tag]))

;;redefine passes mainly to move dependency on `uniquify-locals`
;; to `uniquify2/uniquify-locals`
(defn validate
  "Validate tags, classes, method calls.
   Throws exceptions when invalid forms are encountered, replaces
   class symbols with class objects.

   Passes opts:
   * :validate/throw-on-arity-mismatch
      If true, validate will throw on potential arity mismatch
   * :validate/wrong-tag-handler
      If bound to a function, will invoke that function instead of
      throwing on invalid tag.
      The function takes the tag key (or :name/tag if the node is :def and
      the wrong tag is the one on the :name field meta) and the originating
      AST node and must return a map (or nil) that will be merged into the AST,
      possibly shadowing the wrong tag with Object or nil.
   * :validate/unresolvable-symbol-handler
      If bound to a function, will invoke that function instead of
      throwing on unresolvable symbol.
      The function takes three arguments: the namespace (possibly nil)
      and name part of the symbol, as symbols and the originating
      AST node which can be either a :maybe-class or a :maybe-host-form,
      those nodes are documented in the tools.analyzer quickref.
      The function must return a valid tools.analyzer.jvm AST node."
  {:pass-info {:walk :post :depends #{;; replace
                                      #'infer-tag/infer-tag
                                      #'analyze-host-expr/analyze-host-expr
                                      ;; validate-recur doesn't seem to play nicely with core.async/go
                                      #_#'validate-recur/validate-recur}}}
  [& args]
  (apply validate/validate args))
