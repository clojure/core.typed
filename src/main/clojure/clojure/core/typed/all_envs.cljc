;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc ^:skip-wiki clojure.core.typed.all-envs
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-if-needed :refer [load-if-needed]]))

(def ^:private unparse-type (delay (impl/dynaload 'clojure.core.typed.checker.jvm.parse-unparse/unparse-type)))

(let [nme-env (delay (impl/dynaload 'clojure.core.typed.checker.name-env/name-env))]
  (defn name-env []
    (load-if-needed)
    (binding [vs/*verbose-types* true]
      (into {}
            (for [[k v] (@nme-env)]
              (when-not (keyword? v)
                [k (@unparse-type v)]))))))

(let [venv (delay (impl/dynaload 'clojure.core.typed.checker.var-env/var-annotations))]
  (defn var-env []
    (load-if-needed)
      (assert var-env)
      (binding [vs/*verbose-types* true]
        (into {}
              (for [[k v] (@venv)]
                [k (@unparse-type (force v))])))))

(defn all-envs-clj []
  (impl/with-clojure-impl
    {:aliases (name-env)
     :vars (var-env)}))
