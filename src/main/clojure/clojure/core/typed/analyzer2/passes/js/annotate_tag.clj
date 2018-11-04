;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from tools.analyzer.js
(ns clojure.core.typed.analyzer2.passes.js.annotate-tag)

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

(defmethod -annotate-tag :const
  [ast]
  (let [ast ((get-method -annotate-tag (:type ast)) ast)]
    (if (:tag ast)
      ast
      (assoc ast :tag 'any))))

(defmethod -annotate-tag :nil
  [ast]
  (assoc ast :tag 'clj-nil))

(defmethod -annotate-tag :number
  [ast]
  (assoc ast :tag 'number))

(defmethod -annotate-tag :string
  [ast]
  (assoc ast :tag 'string))

(defmethod -annotate-tag :bool
  [ast]
  (assoc ast :tag 'boolean))

(defmethod -annotate-tag :symbol
  [ast]
  (assoc ast :tag 'cljs.core/Symbol))

(defmethod -annotate-tag :keyword
  [ast]
  (assoc ast :tag 'cljs.core/Keyword))

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

