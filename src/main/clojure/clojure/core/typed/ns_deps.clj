(ns clojure.core.typed.ns-deps
  (:require [clojure.tools.namespace
             [dependency :as dep]]))

(defn init-deps [] 
  (dep/graph))

(defonce TYPED-DEPS (atom (init-deps)))

(defn add-ns-deps [nsym deps]
  (swap! TYPED-DEPS 
         (fn [dm]
           (reduce #(dep/depend %1 nsym %2)
                   dm deps))))

(defn immediate-deps [target-ns]
  (dep/immediate-dependencies @TYPED-DEPS target-ns))

(defn reset-deps! []
  (reset! TYPED-DEPS (init-deps)))
