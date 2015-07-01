;; Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.trim
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.passes.elide-meta :refer [elide-meta]]
            [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [postwalk]]))

(defmulti -trim :op)

(defmethod -trim :default [ast] ast)

(defn preserving-raw-forms [{:keys [form raw-forms] :as ast} body]
  (let [raw-forms (reverse (cons form raw-forms))]
    (update-in (into ast body) [:raw-forms] into raw-forms)))

(defmethod -trim :do
  [{:keys [statements ret form] :as ast}]
  (if (and (every? :literal? statements)
           (not (:tag (meta form))))
    (preserving-raw-forms (dissoc ast :children :statements :ret) ret)
    ast))

;;TODO: letfn/loop
(defmethod -trim :let
  [{:keys [bindings body form] :as ast}]
  (if (and (or (and (every? (comp :literal? :init) bindings)
                    (:literal? body))
               (empty? bindings))
           (not (:tag (meta form))))
    (preserving-raw-forms (dissoc ast :children :bindings :body) body)
    ast))

(defmethod -trim :try
  [{:keys [catches finally body form] :as ast}]
  (if (and (empty? catches)
           (empty? finally)
           (not (:tag (meta form))))
    (preserving-raw-forms (dissoc ast :children :body :finally :catches) body)
    ast))

(defn trim
  "Trims the AST of unnecessary nodes, e.g. (do (do 1)) -> 1"
  {:pass-info {:walk :none :depends #{} :after #{#'elide-meta}}}
  [ast]
  (postwalk ast -trim))
