;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm.annotate-internal-name
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [update-children]]
            [clojure.string :as s]))

(defmulti annotate-internal-name
  "Adds a :internal-name to :fn nodes containing a string that represents
   the name of the class that will be generated for that fn, not including
   the ns prefix"
  :op)

(defn propagate-internal-name
  [ast internal-name]
  (update-children ast (fn [ast] (assoc-in ast [:env :internal-name] internal-name))))

(defmethod annotate-internal-name :default
  [{:keys [env] :as ast}]
  (if-let [internal-name (:internal-name env)]
    (propagate-internal-name ast internal-name)
    ast))

(defmethod annotate-internal-name :def
  [{:keys [name] :as ast}]
  (propagate-internal-name ast (s/replace (str name) "." "_DOT_")))

(defmethod annotate-internal-name :fn
  [{:keys [env local] :as ast}]
  (let [internal-name (str (when-let [n (:internal-name env)]
                             (str n "$"))
                           (s/replace (or (:form local) "fn") "." "_DOT_")
                           (gensym "__"))]
    (-> ast
      (assoc :internal-name internal-name)
      (propagate-internal-name internal-name))))

(defmethod annotate-internal-name :binding
  [{:keys [form env] :as ast}]
  (let [internal-name (str (when-let [n (:internal-name env)]
                             (str n "$"))
                           form)]
   (propagate-internal-name ast internal-name)))
