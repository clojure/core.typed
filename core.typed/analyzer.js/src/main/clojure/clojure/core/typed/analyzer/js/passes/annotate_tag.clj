;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from tools.analyzer.js
(ns clojure.core.typed.analyzer.js.passes.annotate-tag)

(defmulti -annotate-tag :op)

(defmethod -annotate-tag :seq
  [ast]
  (assoc ast :tag 'cljs.core/IList))

(defmethod -annotate-tag :vector
  [ast]
  (assoc ast :tag 'cljs.core/IVector))

(defmethod -annotate-tag :map
  [ast]
  (assoc ast :tag 'cljs.core/IMap))

(defmethod -annotate-tag :set
  [ast]
  (assoc ast :tag 'cljs.core/ISet))

(defmethod -annotate-tag :js-array
  [ast]
  (assoc ast :tag 'array))

(defmethod -annotate-tag :js-object
  [ast]
  (assoc ast :tag 'object))

(defmethod -annotate-tag :js
  [{:keys [form] :as ast}]
  (if (-> form meta :numeric)
    (assoc ast :tag 'number)
    ast))

(defmethod -annotate-tag :fn
  [ast]
  (assoc ast :tag 'function))

; copied from analyze-form in cljs.analyzer
(defn tag-const [form]
  (cond
    (nil? form) 'clj-nil
    (number? form) 'number
    (string? form) 'string
    (instance? Character form) 'string
    (true? form) 'boolean
    (false? form) 'boolean
    (= () form) 'cljs.core/IList))

(defmethod -annotate-tag :const
  [ast]
  (if-let [tag (tag-const (:form ast))]
    (assoc ast :tag tag)
    (assoc ast :tag 'any)))

(defmethod -annotate-tag :default [ast] ast)

(defn annotate-tag
  "If the AST node type is a constant object or contains :tag metadata,
   attach the appropriate :tag to the node."
  {:pass-info {:walk :any :depends #{}}}
  [ast]
  (if-let [tag (or (-> ast :form meta :tag)
                   (-> ast :val meta :tag))]
    (assoc ast :tag tag)
    (-annotate-tag ast)))

