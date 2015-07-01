;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm.validate
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [prewalk]]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.cleanup :refer [cleanup]]
            [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [arglist-for-arity source-info resolve-var resolve-ns]]
            [clojure.core.typed.deps.clojure.tools.analyzer.jvm.utils :as u :refer [tag-match? try-best-match]])
  (:import (clojure.lang IFn ExceptionInfo)))

(defmulti -validate :op)

(defmethod -validate :maybe-class
  [{:keys [class env] :as ast}]
  (if (not (.contains (str class) "."))
    (throw (ex-info (str "Could not resolve var: " class)
                    (merge {:var class}
                           (source-info env))))

    (throw (ex-info (str "Class not found: " class)
                    (merge {:class class}
                           (source-info env))))))

(defmethod -validate :maybe-host-form
  [{:keys [class field form env]}]
  (if (resolve-ns class env)
    (throw (ex-info (str "No such var: " class)
                    (merge {:form form}
                           (source-info env))))
    (throw (ex-info (str "No such namespace: " class)
                    (merge {:ns   class
                            :form form}
                           (source-info env))))))

(defn validate-class
  [{:keys [class form env] :as ast}]
  (if-let [the-class (u/maybe-class class)]
    (assoc ast :class the-class)
    (throw (ex-info (str "Class not found: " class)
                    (merge {:class      class
                            :form       form}
                           (source-info env))))))

(defmethod -validate :set!
  [{:keys [target form env] :as ast}]
  (when (and (not (:assignable? target))
             (not (= :host-interop (:op target))))
    (throw (ex-info "Cannot set! non-assignable target"
                    (merge {:target (prewalk target cleanup)
                            :form   form}
                           (source-info env)))))
  ast)

(defmethod -validate :new
  [{:keys [args] :as ast}]
  (if (:validated? ast)
    ast
    (let [^Class class (-> ast :class :val)
          c-name (symbol (.getName class))
          argc (count args)
          tags (mapv :tag args)]
      (let [[ctor & rest] (->> (filter #(= (count (:parameter-types %)) argc)
                                       (u/members class c-name))
                               (try-best-match tags))]
        (if ctor
          (if (empty? rest)
            (let [arg-tags (mapv u/maybe-class (:parameter-types ctor))
                  args (mapv (fn [arg tag] (assoc arg :tag tag)) args arg-tags)]
              (assoc ast
                :args       args
                :validated? true))
            ast)
          (throw (ex-info (str "no ctor found for ctor of class: " class " and given signature")
                          (merge {:class class
                                  :args  (mapv (fn [a] (prewalk a cleanup)) args)}
                                 (source-info (:env ast))))))))))

(defn validate-call [{:keys [class instance method args tag env op] :as ast}]
  (let [argc (count args)
        instance? (= :instance-call op)
        f (if instance? u/instance-methods u/static-methods)
        tags (mapv :tag args)]
    (if-let [matching-methods (seq (f class method argc))]
      (let [[m & rest :as matching] (try-best-match tags matching-methods)]
        (if m
          (let [all-ret-equals? (apply = (mapv :return-type matching))]
            (if (or (empty? rest)
                    (and all-ret-equals? ;; if the method signature is the same just pick the first one
                         (apply = (mapv #(mapv u/maybe-class (:parameter-types %)) matching))))
             (let [ret-tag  (:return-type m)
                   arg-tags (mapv u/maybe-class (:parameter-types m))
                   args (mapv (fn [arg tag] (assoc arg :tag tag)) args arg-tags)
                   class (u/maybe-class (:declaring-class m))]
               (merge ast
                      {:method     (:name m)
                       :validated? true
                       :class      class
                       :o-tag      ret-tag
                       :tag        (or tag ret-tag)
                       :args       args}
                      (if instance?
                        {:instance (assoc instance :tag class)})))
             (if all-ret-equals?
               (let [ret-tag (:return-type m)]
                 (assoc ast
                   :o-tag   Object
                   :tag     (or tag ret-tag)))
               ast)))
          (if instance?
            (assoc (dissoc ast :class) :tag Object :o-tag Object)
            (throw (ex-info (str "No matching method: " method " for class: " class " and given signature")
                            (merge {:method method
                                    :class  class
                                    :args   (mapv (fn [a] (prewalk a cleanup)) args)}
                                   (source-info env)))))))
      (if instance?
        (assoc (dissoc ast :class) :tag Object :o-tag Object)
        (throw (ex-info (str "No matching method: " method " for class: " class " and arity: " argc)
                        (merge {:method method
                                :class  class
                                :argc   argc}
                               (source-info env))))))))

(defmethod -validate :static-call
  [ast]
  (if (:validated? ast)
    ast
    (validate-call (assoc ast :class (u/maybe-class (:class ast))))))

(defmethod -validate :static-field
  [ast]
  (if (:validated? ast)
    ast
    (assoc ast :class (u/maybe-class (:class ast)))))

(defmethod -validate :instance-call
  [{:keys [class validated? instance] :as ast}]
  (let [class (or class (:tag instance))]
    (if (and class (not validated?))
      (validate-call (assoc ast :class (u/maybe-class class)))
      ast)))

(defmethod -validate :instance-field
  [{:keys [instance class] :as ast}]
  (let [class (u/maybe-class class)]
    (assoc ast :class class :instance (assoc instance :tag class))))

(defmethod -validate :import
  [{:keys [^String class validated? env form] :as ast}]
  (if-not validated?
    (let [class-sym (-> class (subs (inc (.lastIndexOf class "."))) symbol)
          sym-val (resolve-var class-sym env)]
      (if (and (class? sym-val) (not= (.getName ^Class sym-val) class)) ;; allow deftype redef
        (throw (ex-info (str class-sym " already refers to: " sym-val
                             " in namespace: " (:ns env))
                        (merge {:class     class
                                :class-sym class-sym
                                :sym-val   sym-val
                                :form      form}
                               (source-info env))))
        (assoc ast :validated? true)))
    ast))

(defmethod -validate :def
  [ast]
  (when-let [tag (-> ast :name meta :tag)]
    (let [c (u/maybe-class tag)
          s (if (symbol? tag) (name tag) tag)]
      (when-not (and c (not (or (u/specials s) (u/special-arrays s))))
        (throw (ex-info (str "Wrong tag: " (eval tag) " in def: " (:name ast))
                        (merge {:ast      (prewalk ast cleanup)}
                               (source-info (:env ast))))))))

  #_(let [init (:init ast)]
      (when-let [tag (:tag init)]
        (alter-meta! var assoc :tag tag))
      (when-let [arglists (:arglists init)]
        (alter-meta! var assoc :arg-lists arglists)))
  ast)

(defmethod -validate :invoke
  [{:keys [args env fn form] :as ast}]
  (let [argc (count args)]
    (when (and (= :const (:op fn))
               (not (instance? IFn (:form fn))))
      (throw (ex-info (str (class (:form fn)) " is not a function, but it's used as such")
                      (merge {:form form}
                             (source-info env)))))
    (if (and (:arglists fn)
             (not (arglist-for-arity fn argc)))
      (assoc ast :maybe-mismatch-arity true)
      #_(throw (ex-info (str "No matching arity found for function: " (:name fn))
                        {:arity (count args)
                         :fn    fn}))
      ast)))

(defn validate-interfaces [{:keys [env form interfaces]}]
  (when-not (every? #(.isInterface ^Class %) (disj interfaces Object))
    (throw (ex-info "only interfaces or Object can be implemented by deftype/reify"
                    (merge {:interfaces interfaces
                            :form       form}
                           (source-info env))))))

(defmethod -validate :deftype
  [{:keys [class-name] :as ast}]
  (validate-interfaces ast)
  (assoc ast :class-name (u/maybe-class class-name)))

(defmethod -validate :reify
  [{:keys [class-name] :as ast}]
  (validate-interfaces ast)
  (assoc ast :class-name (u/maybe-class class-name)))

(defmethod -validate :default [ast] ast)

(defn validate-tag [t {:keys [env] :as ast}]
  (let [tag (ast t)]
    (if-let [the-class (u/maybe-class tag)]
      {t the-class}
      (throw (ex-info (str "Class not found: " tag)
                      (merge {:class    tag
                              :ast      (prewalk ast cleanup)}
                             (source-info env)))))))

(defn validate
  "Validate tags, classes, method calls.
   Throws exceptions when invalid forms are encountered, replaces
   class symbols with class objects."
  [{:keys [tag form env] :as ast}]
  (when-let [t (:tag (meta form))]
    (when-not (u/maybe-class t)
      (throw (ex-info (str "Class not found: " t)
                      (merge {:class    t
                              :ast      (prewalk ast cleanup)}
                             (source-info env))))))
  (let [ast (merge (-validate ast)
                   (when tag
                     {:tag tag}))]
    (merge ast
           (when (:tag ast)
             (validate-tag :tag ast))
           (when (:o-tag ast)
             (validate-tag :o-tag ast))
           (when (:return-tag ast)
             (validate-tag :return-tag ast)))))
