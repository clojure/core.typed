;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.method-param-nilables
  (:require [clojure.core.typed.env :as env]
            [clojure.core.typed.current-impl :as impl]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defn reset-method-nilable-param-env! [m]
  (env/swap-checker! assoc impl/method-param-nilable-env-kw m)
  nil)

(defn merge-method-nilable-param-env! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/method-param-nilable-env-kw merge m)
  nil)

(def add-method-nilable-param impl/add-method-nilable-param)

(defn nilable-param-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/method-param-nilable-env-kw {}))

(defn nilable-param? [sym arity param]
  (boolean 
    (when-let [nilables (get (nilable-param-env) sym)]
      (when-let [params (or (nilables :all)
                            (nilables arity))]
        (or (#{:all} params)
            (params param))))))
