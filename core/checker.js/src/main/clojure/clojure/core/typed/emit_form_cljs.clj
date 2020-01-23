;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.js.emit-form
(ns ^:skip-wiki clojure.core.typed.emit-form-cljs
  (:require [clojure.tools.analyzer.passes
             [emit-form :as default]
             [uniquify :refer [uniquify-locals]]]
            [clojure.string :as s]
            [cljs.tagged-literals :refer [->JSValue]])
  (:import cljs.tagged_literals.JSValue
           java.io.Writer))

(defmulti -emit-form (fn [{:keys [op]} _] op))

(defn -emit-form*
  [{:keys [form] :as ast} opts]
  (let [expr (-emit-form ast opts)]
    (if-let [m (and (instance? clojure.lang.IObj expr)
                    (meta form))]
      (with-meta expr (merge m (meta expr)))
      expr)))

(defn emit-form
  "Return the form represented by the given AST
   Opts is a set of options, valid options are:
    * :hygienic
    * :qualified-symbols"
  {:pass-info {:walk :none :depends #{#'uniquify-locals} :compiler true}}
  ([ast] (emit-form ast #{}))
  ([ast opts]
     (binding [default/-emit-form* -emit-form*]
       (-emit-form* ast opts))))

(defn emit-hygienic-form
  "Return an hygienic form represented by the given AST"
  {:pass-info {:walk :none :depends #{#'uniquify-locals} :compiler true}}
  [ast]
  (binding [default/-emit-form* -emit-form*]
    (-emit-form* ast #{:hygienic})))

(defmethod -emit-form :default
  [ast opts]
  (default/-emit-form ast opts))

(defmethod -emit-form :js
  [{:keys [segs args]} opts]
  (list* 'js* (s/join "~{}" segs) (mapv #(-emit-form* % opts) args)))

(defmethod -emit-form :js-object
  [{:keys [keys vals]} opts]
  (->JSValue (zipmap keys (map #(-emit-form* % opts) vals))))

(defmethod -emit-form :js-array
  [{:keys [items]} opts]
  (->JSValue (mapv #(-emit-form* % opts) items)))

(defmethod print-method JSValue [^JSValue o ^Writer w]
  (.write w "#js ")
  (.write w (str (.val o))))

(defmethod -emit-form :deftype
  [{name :t :keys [fields pmask body]} opts]
  (list 'deftype* name (map #(-emit-form* % opts) fields) pmask
        (-emit-form* body opts)))

(defmethod -emit-form :defrecord
  [{name :t :keys [fields pmask body]} opts]
  (list 'defrecord* name (map #(-emit-form* % opts) fields) pmask
        (-emit-form* body opts)))

(defmethod -emit-form :case-then
  [{:keys [then]} opts]
  (-emit-form* then opts))

(defmethod -emit-form :case-test
  [{:keys [test]} opts]
  (-emit-form* test opts))

(defmethod -emit-form :case
  [{:keys [test nodes default]} opts]
  `(case* ~(-emit-form* test opts)
          ~@(reduce (fn [acc {:keys [tests then]}]
                      (-> acc
                        (update-in [0] conj (mapv #(-emit-form* % opts) tests))
                        (update-in [1] conj (-emit-form* then opts))))
                    [[] []] nodes)
          ~(-emit-form* default opts)))

