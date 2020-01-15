;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from tools.analyzer.js
(ns clojure.core.typed.analyzer.passes.js.infer-tag
  (:require [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.utils :refer [arglist-for-arity]]
            [clojure.core.typed.analyzer.passes.add-binding-atom :refer [add-binding-atom]]
            [clojure.core.typed.analyzer.passes.js.annotate-tag :refer [annotate-tag]]
            [clojure.core.typed.analyzer.passes.js.analyze-host-expr :refer [analyze-host-expr]]))

(defmulti -infer-tag :op)
(defmethod -infer-tag :default [ast] ast)

(defmethod -infer-tag :recur
  [ast]
  (assoc ast :tag 'ignore :ignore-tag true))

(defmethod -infer-tag :throw
  [ast]
  (assoc ast :tag 'ignore :ignore-tag true))

(defmethod -infer-tag :with-meta
  [{:keys [expr] :as ast}]
  (merge ast (select-keys expr [:return-tag :arglists :ignore-tag :tag])))

(defmethod -infer-tag :do
  [{:keys [ret] :as ast}]
  (merge ast (select-keys ret [:return-tag :arglists :ignore-tag :tag])))

(defmethod -infer-tag :let
  [{:keys [body] :as ast}]
  (merge ast (select-keys body [:return-tag :arglists :ignore-tag :tag])))

(defmethod -infer-tag :letfn
  [{:keys [body] :as ast}]
  (merge ast (select-keys body [:return-tag :arglists :ignore-tag :tag])))

(defmethod -infer-tag :loop
  [{:keys [body] :as ast}]
  (merge ast (select-keys body [:return-tag :arglists :ignore-tag :tag])))

(defmethod -infer-tag :binding
  [{:keys [init atom] :as ast}]
  (let [ast (if init
              (merge (select-keys init [:return-tag :arglists :ignore-tag :tag]) ast)
              ast)]
    (swap! atom merge (select-keys ast [:return-tag :arglists :ignore-tag :tag]))
    ast))

(defmethod -infer-tag :local
  [{:keys [atom] :as ast}]
  (merge ast @atom))

(defmethod -infer-tag :def
  [{:keys [init var] :as ast}]
  (let [info (select-keys init [:return-tag :arglists :ignore-tag :tag])]
    (swap! env/*env* update-in [:namespaces (:ns var) :mappings (:name var)] merge info)
    (merge ast info)))

(defmethod -infer-tag :var
  [{:keys [var] :as ast}]
  (let [info (-> (env/deref-env)
               (get-in [:namespaces (:ns var) :mappings (:name var)])
               (select-keys [:return-tag :arglists :ignore-tag :tag]))]
    (merge ast info)))

(defmethod -infer-tag :set!
  [{:keys [target] :as ast}]
  (if-let [tag (:tag target)]
    (assoc ast :tag tag)
    ast))

(defmethod -infer-tag :invoke
  [{:keys [fn args] :as ast}]
  (if (:arglists fn)
    (let [argc (count args)
          arglist (arglist-for-arity fn argc)
          tag (or (:tag (meta arglist))
                  (:return-tag fn)
                  (and (= :var (:op fn))
                       (:tag (meta (:var fn)))))]
      (merge ast
             (when tag
               {:tag     tag})))
    (if-let [tag (:return-tag fn)]
      (assoc ast :tag tag)
      ast)))

(defn =-arglists? [a1 a2]
  (let [tag (fn [x] (-> x meta :tag))]
    (and (= a1 a2)
         (every? true? (mapv (fn [a1 a2]
                       (and (= (tag a1) (tag a2))
                            (= (mapv tag a1)
                               (mapv tag a2))))
                     a1 a2)))))

(defmethod -infer-tag :if
  [{:keys [then else] :as ast}]
  (let [then-tag (:tag then)
        else-tag (:tag else)
        ignore-then? (:ignore-tag then)
        ignore-else? (:ignore-tag else)]
    (cond
     (and then-tag
          (or ignore-else? (= then-tag else-tag)))
     (merge ast
            {:tag then-tag}
            (when-let [return-tag (:return-tag then)]
              (when (or ignore-else?
                        (= return-tag (:return-tag else)))
                {:return-tag return-tag}))
            (when-let [arglists (:arglists then)]
              (when (or ignore-else?
                        (=-arglists? arglists (:arglists else)))
                {:arglists arglists})))

     (and else-tag ignore-then?)
     (merge ast
            {:tag else-tag}
            (when-let [return-tag (:return-tag else)]
              {:return-tag return-tag})
            (when-let [arglists (:arglists else)]
              {:arglists arglists}))

     (and (:ignore-tag then) (:ignore-tag else))
     (assoc ast :tag 'ignore :ignore-tag true)

     :else
     ast)))

;;TODO: handle catches
(defmethod -infer-tag :try
  [{:keys [body catches] :as ast}]
  (let [{:keys []} body]
    (merge ast (select-keys [:tag :return-tag :arglists :ignore-tag] body))))

;;TODO: handle :ignore-tag ?
(defmethod -infer-tag :fn-method
  [{:keys [form body params local] :as ast}]
  (let [annotated-tag (or (:tag (meta (first form)))
                          (:tag (meta (:form local))))
        body-tag (:tag body)
        tag (or annotated-tag body-tag)]
    (merge ast
           (when tag
             {:tag   tag})
           {:arglist (with-meta (vec (mapcat (fn [{:keys [form variadic?]}]
                                               (if variadic? ['& form] [form]))
                                             params))
                       (when tag {:tag tag}))})))

(defmethod -infer-tag :fn
  [{:keys [local methods] :as ast}]
  (merge ast
         {:arglists (seq (mapv :arglist methods))}
         (when-let [tag (:tag (meta (:form local)))]
           {:return-tag tag})))

(defn var-sym [var]
  (when-let [{:keys [ns name]} var]
    (symbol (str (or ns 'js)) (str name))))

(defmethod -infer-tag :new
  [{:keys [class] :as ast}]
  (if-let [v (var-sym (:var class))]
    (assoc ast :tag (case v
                      js/Object   'object
                      js/String   'string
                      js/Array    'array
                      js/Number   'number
                      js/Function 'function
                      js/Boolean  'boolean
                      v))
    ast))

(defn infer-tag
  "Performs local type inference on the AST adds, when possible,
   one or more of the following keys to the AST:
   * :tag        represents the type the expression represented by the node
   * :return-tag implies that the node will return a function whose
                 invocation will result in a object of this type
   * :arglists   implies that the node will return a function with
                 this arglists
   * :ignore-tag true when the node is untyped, does not imply that
                 all untyped node will have this"
  {:pass-info {:walk :post :depends #{#'analyze-host-expr #'annotate-tag #'add-binding-atom}}}
  [{:keys [tag] :as ast}]
  (merge (-infer-tag ast)
         (when tag
           {:tag tag})))
