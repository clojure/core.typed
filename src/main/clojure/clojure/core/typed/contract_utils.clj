(ns clojure.core.typed.contract-utils
  {:skip-wiki true
   :core.typed {:collect-only true}}
  (:require [clojure.set :as set]))

(alter-meta! *ns* assoc :skip-wiki true
             :core.typed {:collect-only true})



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint shorthands

(defn every-c? [c]
  #(every? c %))

(def nat? (every-pred integer? (complement neg?)))

(def boolean? (some-fn true? false?))

(def namespace? #(instance? clojure.lang.Namespace %))

(defn =-c? [& as]
  #(apply = (concat as %&)))

(defn hvector-c? [& ps]
  (apply every-pred vector?
         (map (fn [p i] #(p (nth % i false))) ps (range))))

(defn array-map-c? [ks-c? vs-c?]
  (every-pred #(instance? clojure.lang.PersistentArrayMap %)
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn hmap-c? [& key-vals]
  (every-pred map?
              #(every? identity 
                       (for [[k vc] (partition 2 key-vals)]
                         (and (contains? % k)
                              (vc (get % k)))))))

(defn hash-c? [ks-c? vs-c?]
  (every-pred map?
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn set-c? [c?]
  (every-pred set?
              #(every? c? %)))

(defn sequential-c? [c?]
  (every-pred sequential?
              (every-c? c?)))
