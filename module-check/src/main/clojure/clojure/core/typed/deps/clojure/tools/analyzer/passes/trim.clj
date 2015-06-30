;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.trim)


(defmulti trim
  "Trims the AST from duplicate :do nodes or useless :let nodes.
   WARNING: Still experimental, possibly useful metadata-stored info
            might get lost"
  :op)

(defmethod trim :default [ast] ast)

(defmethod trim :do
  [{:keys [statements ret] :as ast}]
  (if (or (and (every? :literal? statements)
               (:literal? ret))
          (empty? statements))
    ret
    ast))

(defmethod trim :let
  [{:keys [bindings body] :as ast}]
  (if (and (every? (comp :literal? :init) bindings)
           (:literal? body))
    body
    ast))
