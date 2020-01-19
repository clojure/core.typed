;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.emit-form
(ns clojure.core.typed.analyzer.common.passes.emit-form
  (:require [clojure.tools.analyzer.passes.uniquify :refer [uniquify-locals]]))

(defmulti -emit-form (fn [{:keys [op]} _] op))

(defn ^:dynamic -emit-form*
  "Extension point for custom emit-form implementations, should be rebound
   to a multimethod with custom emit-form :opts."
  [{:keys [form] :as ast} opts]
  (let [expr (-emit-form ast opts)]
    (if-let [m (and (instance? clojure.lang.IObj expr)
                    (meta form))]
      (with-meta expr (merge m (meta expr)))
      expr)))

(defn emit-form
  "Return the form represented by the given AST.
   Opts is a set of options, valid options are:
    * :hygienic"
  {:pass-info {:walk :none :depends #{#'uniquify-locals} :compiler true}}
  ([ast] (emit-form ast #{}))
  ([ast opts] (-emit-form* ast opts)))

(defn emit-hygienic-form
  "Return an hygienic form represented by the given AST"
  {:pass-info {:walk :none :depends #{#'uniquify-locals} :compiler true}}
  [ast]
  (-emit-form* ast #{:hygienic}))

(defmethod -emit-form :maybe-class
  [{:keys [class]} opts]
  class)

(defmethod -emit-form :maybe-host-form
  [{:keys [class field]} opts]
  (symbol (name class) (name field)))

(defmethod -emit-form :host-call
  [{:keys [target method args]} opts]
  (list '. (-emit-form* target opts)
        (list* method (mapv #(-emit-form* % opts) args))))

(defmethod -emit-form :host-field
  [{:keys [target field]} opts]
  (list '. (-emit-form* target opts)
        (symbol (str "-" (name field)))))

(defmethod -emit-form :host-interop
  [{:keys [target m-or-f]} opts]
  (list '. (-emit-form* target opts) m-or-f))

(defmethod -emit-form :local
  [{:keys [name form]} opts]
  (if (:hygienic opts) (with-meta name (meta form)) form))

(defmethod -emit-form :binding
  [{:keys [name form]} opts]
  (if (:hygienic opts) (with-meta name (meta form)) form))

(defmethod -emit-form :var
  [{:keys [form]} opts]
  form)

(defn emit-bindings [bindings opts]
  (mapcat (fn [{:keys [name form init]}]
            [(if (:hygienic opts) name form) (-emit-form* init opts)])
          bindings))

(defmethod -emit-form :letfn
  [{:keys [bindings body]} opts]
  `(letfn* [~@(emit-bindings bindings opts)]
           ~(-emit-form* body opts)))

(defmethod -emit-form :let
  [{:keys [bindings body]} opts]
  `(let* [~@(emit-bindings bindings opts)]
           ~(-emit-form* body opts)))

(defmethod -emit-form :loop
  [{:keys [bindings body]} opts]
  `(loop* [~@(emit-bindings bindings opts)]
           ~(-emit-form* body opts)))

(defmethod -emit-form :const
  [{:keys [form]} _]
  form)

(defmethod -emit-form :quote
  [{:keys [expr]} opts]
  (list 'quote (-emit-form* expr opts)))

(defmethod -emit-form :vector
  [{:keys [items]} opts]
  (mapv #(-emit-form* % opts) items))

(defmethod -emit-form :set
  [{:keys [items]} opts]
  (set (mapv #(-emit-form* % opts) items)))

(defmethod -emit-form :map
  [{:keys [keys vals]} opts]
  (apply hash-map (interleave (mapv #(-emit-form* % opts) keys)
                              (mapv #(-emit-form* % opts) vals))))

(defmethod -emit-form :with-meta
  [{:keys [expr meta]} opts]
  (with-meta (-emit-form* expr opts)
    (-emit-form* meta opts)))

(defmethod -emit-form :do
  [{:keys [ret statements body?]} opts]
  (if (and body? (empty? statements))
    (-emit-form* ret opts)
    `(do ~@(mapv #(-emit-form* % opts) statements)
         ~(-emit-form* ret opts))))

(defmethod -emit-form :if
  [{:keys [test then else]} opts]
  `(if ~(-emit-form* test opts)
     ~(-emit-form* then opts)
     ~@(when-not (nil? (:form else))
         [(-emit-form* else opts)])))

(defmethod -emit-form :new
  [{:keys [class args]} opts]
  `(new ~(-emit-form* class opts) ~@(mapv #(-emit-form* % opts) args)))

(defmethod -emit-form :set!
  [{:keys [target val]} opts]
  `(set! ~(-emit-form* target opts) ~(-emit-form* val opts)))

(defmethod -emit-form :recur
  [{:keys [exprs]} opts]
  `(recur ~@(mapv #(-emit-form* % opts) exprs)))

(defmethod -emit-form :fn-method
  [{:keys [variadic? params body form]} opts]
  (let [params-form (mapv #(-emit-form* % opts) params)]
    `(~(with-meta
         (if variadic? (into (pop params-form)
                             (conj '[&] (peek params-form)))
             params-form)
         (meta (first form)))
      ~(-emit-form* body opts))))

(defmethod -emit-form :fn
  [{:keys [local methods]} opts]
  `(fn* ~@(when local [(-emit-form* local opts)])
        ~@(mapv #(-emit-form* % opts) methods)))

(defmethod -emit-form :def
  [{:keys [name doc init]} opts]
  (let [name (if-let [arglists (:arglists (meta name))]
               (with-meta name (assoc (meta name) :arglists (list 'quote arglists)))
               name)]
    `(def ~name ~@(when doc [doc]) ~@(when init [(-emit-form* init opts)]))))

(defmethod -emit-form :invoke
  [{:keys [fn args meta]} opts]
  (let [expr `(~(-emit-form* fn opts)
               ~@(mapv #(-emit-form* % opts) args))]
    (if meta
      (with-meta expr meta)
      expr)))

(defmethod -emit-form :try
  [{:keys [body catches finally]} opts]
  `(try ~(-emit-form* body opts)
        ~@(mapv #(-emit-form* % opts) catches)
        ~@(when finally
            [`(finally ~(-emit-form* finally opts))])))

(defmethod -emit-form :catch
  [{:keys [class local body]} opts]
  `(catch ~(-emit-form* class opts) ~(-emit-form* local opts)
     ~(-emit-form* body opts)))

(defmethod -emit-form :throw
  [{:keys [exception]} opts]
  `(throw ~(-emit-form* exception opts)))
