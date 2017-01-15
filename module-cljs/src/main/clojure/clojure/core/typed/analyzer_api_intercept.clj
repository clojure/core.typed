(ns clojure.core.typed.analyzer-api-intercept
  (:require [cljs.analyzer.api :as api]
            [clojure.core.typed.ast-utils :as ast-u]))

(def ops-found (atom #{}))

(defn reset-found []
  (reset! ops-found #{}))

(defn walk-collect-ops [{:keys [op children] :as node}]
  (when-not (some (partial = op) @ops-found)
    (swap! ops-found #(conj % op)))
  (when children
    (doseq [c children]
      (walk-collect-ops (c node)))))

(defn analyze
  ([env form] (analyze env form nil {}))
  ([env form x opts]
   (let [result (api/analyze env form x opts)]
     (walk-collect-ops result)
     ;(println ops-found)
     result)))

(defn empty-env []
  (api/empty-env))
