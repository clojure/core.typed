;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.init
  (:require [clojure.core.typed.current-impl :as impl]))

(defonce ^:private attempted-loading? (atom false))
(defonce ^:private successfully-loaded? (atom false))
(defonce ^:private cljs-loaded? (atom false))

(defonce ^:private cljs-present? (atom false))

(defn loaded? []
  @successfully-loaded?)

(defn has-cljs-loaded? []
  @cljs-loaded?)

(defn load-impl 
  ([] (load-impl false))
  ([cljs?]
  (cond 
    (and @attempted-loading?
         (not @successfully-loaded?))
    (throw (Exception. 
             (str "There was previously an unrecoverable internal error while loading core.typed." 
                  " Please restart your process.")))

    (and @successfully-loaded? @attempted-loading?
         (if cljs?
           @cljs-loaded?
           true))
    nil

    :else
    (do
      (try
        (reset! attempted-loading? true)
        (catch Exception e
          (reset! successfully-loaded? false)
          (throw e)))
      (reset! successfully-loaded? true)
      (println "Building core.typed base environments ...")
      (flush)
      ;(impl/with-clojure-impl
      ;  ((impl/dynaload 'clojure.core.typed.checker.reset-env/reset-envs!)))
      (impl/register!)
      (impl/with-clojure-impl
        ((impl/dynaload 'clojure.core.typed.checker.reset-env/load-core-envs!)))
      (when cljs?
        (impl/with-cljs-impl
          ;; FIXME should be load-core-envs!
          ((impl/dynaload 'clojure.core.typed.checker.reset-env/reset-envs!) cljs?)))
      (println "Finished building base environments")
      (flush)
      nil))))
