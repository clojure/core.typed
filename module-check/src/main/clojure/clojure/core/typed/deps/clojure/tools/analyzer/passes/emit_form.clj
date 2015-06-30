;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.emit-form)

(defmulti -emit-form (fn [{:keys [op]} _] op))

(defn ^:dynamic -emit-form*
  "Extension point for custom emit-form implementations, should be rebound
   to a multimethod with custom emit-form :ops."
  [{:keys [form] :as ast} ops]
  (let [expr (-emit-form ast ops)]
    (if-let [m (and (instance? clojure.lang.IObj expr)
                    (meta form))]
      (with-meta expr (merge (meta expr) m))
      expr)))

(defn emit-form
  "Return the form represented by the given AST.
   Ops is a set of options, valid options are:
    * :hygienic"
  ([ast] (emit-form ast #{}))
  ([ast ops] (-emit-form* ast ops)))

(defn emit-hygienic-form
  "Return an hygienic form represented by the given AST"
  [ast]
  (-emit-form* ast #{:hygienic}))

(defmethod -emit-form :maybe-class
  [{:keys [class]} ops]
  class)

(defmethod -emit-form :maybe-host-form
  [{:keys [class field]} ops]
  (symbol (name class) (name field)))

(defmethod -emit-form :host-call
  [{:keys [target method args]} ops]
  (list '. (-emit-form* target ops)
        (list* method (mapv #(-emit-form* % ops) args))))

(defmethod -emit-form :host-field
  [{:keys [target field]} ops]
  (list (symbol (str ".-" (name field)))
        (-emit-form* target ops)))

(defmethod -emit-form :host-interop
  [{:keys [target m-or-f]} ops]
  (list '. (-emit-form* target ops) m-or-f))

(defmethod -emit-form :local
  [{:keys [name form]} ops]
  (if (:hygienic ops) (with-meta name (meta form)) form))

(defmethod -emit-form :binding
  [{:keys [name form]} ops]
  (if (:hygienic ops) (with-meta name (meta form)) form))

(defmethod -emit-form :var
  [{:keys [form]} ops]
  form)

(defn emit-bindings [bindings ops]
  (mapcat (fn [{:keys [name form init]}]
            [(if (:hygienic ops) name form) (-emit-form* init ops)])
          bindings))

(defmethod -emit-form :letfn
  [{:keys [bindings body]} ops]
  `(letfn* [~@(emit-bindings bindings ops)]
           ~(-emit-form* body ops)))

(defmethod -emit-form :let
  [{:keys [bindings body]} ops]
  `(let* [~@(emit-bindings bindings ops)]
           ~(-emit-form* body ops)))

(defmethod -emit-form :loop
  [{:keys [bindings body]} ops]
  `(loop* [~@(emit-bindings bindings ops)]
           ~(-emit-form* body ops)))

(defmethod -emit-form :const
  [{:keys [form]} _]
  form)

(defmethod -emit-form :quote
  [{:keys [expr]} ops]
  (list 'quote (-emit-form* expr ops)))

(defmethod -emit-form :vector
  [{:keys [items]} ops]
  (mapv #(-emit-form* % ops) items))

(defmethod -emit-form :set
  [{:keys [items]} ops]
  (set (mapv #(-emit-form* % ops) items)))

(defmethod -emit-form :map
  [{:keys [keys vals]} ops]
  (apply hash-map (interleave (mapv #(-emit-form* % ops) keys)
                              (mapv #(-emit-form* % ops) vals))))

(defmethod -emit-form :with-meta
  [{:keys [expr meta]} ops]
  (with-meta (-emit-form* expr ops)
    (-emit-form* meta ops)))

(defmethod -emit-form :do
  [{:keys [ret statements body?]} ops]
  (if (and body? (empty? statements))
    (-emit-form* ret ops)
    `(do ~@(mapv #(-emit-form* % ops) statements)
         ~(-emit-form* ret ops))))

(defmethod -emit-form :if
  [{:keys [test then else]} ops]
  `(if ~(-emit-form* test ops)
     ~(-emit-form* then ops)
     ~@(when-not (nil? (:form else))
         [(-emit-form* else ops)])))

(defmethod -emit-form :new
  [{:keys [class args]} ops]
  `(new ~class ~@(mapv #(-emit-form* % ops) args)))

(defmethod -emit-form :set!
  [{:keys [target val]} ops]
  `(set! ~(-emit-form* target ops) ~(-emit-form* val ops)))

(defmethod -emit-form :recur
  [{:keys [exprs]} ops]
  `(recur ~@(mapv #(-emit-form* % ops) exprs)))

(defmethod -emit-form :fn-method
  [{:keys [variadic? params body form]} ops]
  (let [params-form (mapv #(-emit-form* % ops) params)]
    `(~(with-meta
         (if variadic? (into (pop params-form)
                             (conj '[&] (peek params-form)))
             params-form)
         (meta (first form)))
      ~(-emit-form* body ops))))

(defmethod -emit-form :fn
  [{:keys [local methods]} ops]
  `(fn* ~@(when local [(-emit-form* local ops)])
        ~@(mapv #(-emit-form* % ops) methods)))

(defmethod -emit-form :def
  [{:keys [name doc init]} ops]
  (let [name (if-let [arglists (:arglists (meta name))]
               (with-meta name (assoc (meta name) :arglists (list 'quote arglists)))
               name)]
    `(def ~name ~@(when doc [doc]) ~@(when init [(-emit-form* init ops)]))))

(defmethod -emit-form :invoke
  [{:keys [fn args meta]} ops]
  (let [expr `(~(-emit-form* fn ops)
               ~@(mapv #(-emit-form* % ops) args))]
    (if meta
      (with-meta expr meta)
      expr)))

(defmethod -emit-form :try
  [{:keys [body catches finally]} ops]
  `(try ~(-emit-form* body ops)
        ~@(mapv #(-emit-form* % ops) catches)
        ~@(when finally
            [`(finally ~(-emit-form* finally ops))])))

(defmethod -emit-form :catch
  [{:keys [class local body]} ops]
  `(catch ~class ~(-emit-form* local ops)
     ~(-emit-form* body ops)))

(defmethod -emit-form :throw
  [{:keys [exception]} ops]
  `(throw ~(-emit-form* exception ops)))
