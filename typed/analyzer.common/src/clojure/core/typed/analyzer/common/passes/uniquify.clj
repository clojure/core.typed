;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.analyzer.common.passes.uniquify
  (:require [clojure.core.typed.analyzer.common.ast :refer [update-children children]]
            [clojure.core.typed.analyzer.common.utils :refer [update-vals]]
            [clojure.core.typed.analyzer.common.env :as env]))

(def ^:dynamic *locals-counter*) ;; global counter, map sym -> count
(def ^:dynamic *locals-frame*)   ;; holds the id for the locals in the current frame

(defn normalize [name]
  (or (@*locals-frame* name) name))

(defn uniquify [name]
  (swap! *locals-counter* #(update-in % [name] (fnil inc -1)))
  (swap! *locals-frame* #(assoc-in % [name] (symbol (str
                                                      ;; Add extra gensym so bindings
                                                      ;; don't get clobbered when they
                                                      ;; travel between `do` expressions.
                                                      ;; eg. 
                                                      ;;   (do (let [m (ann-form 1 Any)]
                                                      ;;         (assert (number? m))
                                                      ;;         m)
                                                      ;;       (let [m (ann-form 1 Any)]
                                                      ;;         (ann-form m Number)))
                                                      ;;
                                                      ;; Actually, unsure if this is possible in practice.
                                                      ;; I don't have a test case to prove it, so better
                                                      ;; safe than sorry.
                                                      ;; - Ambrose
                                                      (identity #_gensym name)
                                                      "__#" (@*locals-counter* name))))))

(defmulti -uniquify-locals :op)

(defn pre-uniquify-child
  [ast]
  (-> ast
      (assoc-in [:env ::locals-frame] *locals-frame*)
      ;; immutable copy for type resolution later
      (assoc-in [:env ::locals-frame-val] @*locals-frame*)
      (assoc-in [:env ::locals-counter] *locals-counter*)))

(defn uniquify-locals-around
  [ast]
  (let [ast (if (-> (env/deref-env) :passes-opts :uniquify/uniquify-env)
              (update-in ast [:env :locals]
                         update-vals #(update-in % [:name] normalize))
              ast)]
   (-uniquify-locals ast)))

(defn uniquify-locals* [ast]
  (update-children ast pre-uniquify-child #_uniquify-locals-around))

(defmethod -uniquify-locals :local
  [ast]
  (if (= :field (:local ast)) ;; deftype fields cannot be uniquified
    ast                       ;; to allow field access/set! to work
    (let [name (normalize (:name ast))]
      (assoc ast :name name))))

(defn uniquify-binding
  [b]
  (let [i (binding [*locals-frame* (atom @*locals-frame*)] ;; inits need to be uniquified before the local
            (pre-uniquify-child (:init b)))            ;; to avoid potential shadowings
        name (:name b)]
    (uniquify name)
    (let [name (normalize name)]
      (assoc b
        :name name
        :init i))))

(defmethod -uniquify-locals :letfn
  [ast]
  (doseq [{:keys [name]} (:bindings ast)] ;; take into account that letfn
    (uniquify name))                      ;; accepts parallel bindings
  (uniquify-locals* ast))

(defmethod -uniquify-locals :binding
  [{:keys [name local] :as ast}]
  (case local
    (:let :loop)
    (uniquify-binding ast)

    :letfn
    (-> ast
      (assoc :name (normalize name))
      uniquify-locals*)

    :field
    ast

    (do (uniquify name)
        (assoc ast :name (normalize name)))))

(defmethod -uniquify-locals :default
  [ast]
  (if (some #(= :binding (:op %)) (children ast))
    (binding [*locals-frame* (atom @*locals-frame*)] ;; set up frame so locals won't leak
      (uniquify-locals* ast))
    (uniquify-locals* ast)))

(defn uniquify-locals
  "Walks the AST performing alpha-conversion on the :name field
   of :local/:binding nodes, invalidates :local map in :env field

  Passes opts:
  * :uniquify/uniquify-env  If true, uniquifies the :env :locals map"
  {:pass-info {:walk :pre :depends #{}}}
  [{{:keys [::locals-counter ::locals-frame]
     :or {locals-counter (atom {})
          locals-frame   (atom {})}}
    :env
    :as ast}]
  (binding [*locals-counter* locals-counter
            *locals-frame*   locals-frame]
    (uniquify-locals-around ast)))
