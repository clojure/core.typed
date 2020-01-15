;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.method-return-nilables
  (:require [clojure.core.typed.env :as env]
            [clojure.core.typed.current-impl :as impl]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Return non-nilables

(def add-nonnilable-method-return impl/add-nonnilable-method-return)

(defn reset-nonnilable-method-return-env! [m]
  (env/swap-checker! assoc impl/method-return-nonnilable-env-kw m)
  nil)

(defn merge-nonnilable-method-return-env! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/method-return-nonnilable-env-kw merge m)
  nil)

(defn nonnilable-method-return-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/method-return-nonnilable-env-kw {}))

(defn nonnilable-return? [sym arity]
  (let [as (get (nonnilable-method-return-env) sym)]
    (boolean (or (= :all as)
                 (when as
                   (as arity))))))
