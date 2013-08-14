(ns ^:skip-wiki clojure.core.typed.js-env
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]))

(defonce JS-ENV (atom {} :validator (u/hash-c? symbol? r/Type?)))

(defn add-js-var [sym type]
  (swap! JS-ENV assoc sym type))

(defn get-js-var [sym]
  (get-in @JS-ENV [sym]))

(defn resolve-js-var [sym]
  (let [t (get-js-var sym)]
    (assert t "JS var not found")
    t))

(defn reset-js-env! [jsenv]
  (reset! JS-ENV jsenv))
