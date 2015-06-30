;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm.emit-form
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.passes.emit-form :as default]))

(defmulti -emit-form (fn [{:keys [op]} _] op))

(defn -emit-form*
  [{:keys [form] :as ast} ops]
  (let [expr (-emit-form ast ops)]
    (if-let [m (and (instance? clojure.lang.IObj expr)
                    (meta form))]
      (with-meta expr (merge (meta expr) m))
      expr)))

(defn emit-form
  "Return the form represented by the given AST
   Ops is a set of options, valid options are:
    * :hygienic
    * :qualified-vars"
  ([ast] (emit-form ast #{}))
  ([ast ops]
     (binding [default/-emit-form* -emit-form*]
       (-emit-form* ast ops))))

(defn emit-hygienic-form
  "Return an hygienic form represented by the given AST"
  [ast]
  (binding [default/-emit-form* -emit-form*]
    (-emit-form* ast #{:hygienic})))

(defmethod -emit-form :default
  [ast ops]
  (default/-emit-form ast ops))

(defmethod -emit-form :monitor-enter
  [{:keys [target]} ops]
  `(monitor-enter ~(-emit-form* target ops)))

(defmethod -emit-form :monitor-exit
  [{:keys [target]} ops]
  `(monitor-exit ~(-emit-form* target ops)))

(defmethod -emit-form :import
  [{:keys [class]} ops]
  `(clojure.core/import* ~class))

(defmethod -emit-form :the-var
  [{:keys [^clojure.lang.Var var]} ops]
  `(var ~(symbol (name (ns-name (.ns var))) (name (.sym var)))))

(defmethod -emit-form :method
  [{:keys [params body this name form]} ops]
  (let [params (into [this] params)]
    `(~(with-meta name (meta (first form)))
      ~(with-meta (mapv #(-emit-form* % ops) params)
         (meta (second form)))
      ~(-emit-form* body ops))))

(defn class->sym [class]
  (symbol (.getName ^Class class)))

(defmethod -emit-form :catch
  [{:keys [class local body]} ops]
  `(catch ~(class->sym class) ~(-emit-form* local ops)
     ~(-emit-form* body ops)))

(defmethod -emit-form :deftype
  [{:keys [name class-name fields interfaces methods]} ops]
  `(deftype* ~name ~(class->sym class-name) ~(mapv #(-emit-form* % ops) fields)
     :implements ~(mapv class->sym interfaces)
     ~@(mapv #(-emit-form* % ops) methods)))

(defmethod -emit-form :reify
  [{:keys [interfaces methods]} ops]
  `(reify* ~(mapv class->sym (disj interfaces clojure.lang.IObj))
           ~@(mapv #(-emit-form* % ops) methods)))

(defmethod -emit-form :case
  [{:keys [test default tests thens shift mask low high switch-type test-type skip-check?]} ops]
  `(case* ~(-emit-form* test ops)
          ~shift ~mask
          ~(-emit-form* default ops)
          ~(apply sorted-map
                  (mapcat (fn [{:keys [hash test]} {:keys [then]}]
                            [hash [(-emit-form* test ops) (-emit-form* then ops)]])
                          tests thens))
          ~switch-type ~test-type ~skip-check?))

(defmethod -emit-form :static-field
  [{:keys [class field]} ops]
  (symbol (.getName ^Class class) (name field)))

(defmethod -emit-form :static-call
  [{:keys [class method args]} ops]
  `(~(symbol (.getName ^Class class) (name method))
    ~@(mapv #(-emit-form* % ops) args)))

(defmethod -emit-form :instance-field
  [{:keys [instance field]} ops]
  `(~(symbol (str ".-" (name field))) ~(-emit-form* instance ops)))

(defmethod -emit-form :instance-call
  [{:keys [instance method args]} ops]
  `(~(symbol (str "." (name method))) ~(-emit-form* instance ops)
    ~@(mapv #(-emit-form* % ops) args)))

(defmethod -emit-form :host-interop
  [{:keys [target m-or-f]} ops]
  `(~(symbol (str "." (name m-or-f))) ~(-emit-form* target ops)))

(defmethod -emit-form :prim-invoke
  [{:keys [fn args]} ops]
  `(~(-emit-form* fn ops)
    ~@(mapv #(-emit-form* % ops) args)))

(defmethod -emit-form :protocol-invoke
  [{:keys [fn args]} ops]
  `(~(-emit-form* fn ops)
    ~@(mapv #(-emit-form* % ops) args)))

(defmethod -emit-form :keyword-invoke
  [{:keys [fn args]} ops]
  `(~(-emit-form* fn ops)
    ~@(mapv #(-emit-form* % ops) args)))

(defmethod -emit-form :instance?
  [{:keys [class target]} ops]
  `(instance? ~class ~(-emit-form* target ops)))

(defmethod -emit-form :var
  [{:keys [form ^clojure.lang.Var var]} ops]
  (if (:qualified-vars ops)
    (with-meta (symbol (-> var .ns ns-name name) (-> var .sym name))
      (meta form))
    form))
