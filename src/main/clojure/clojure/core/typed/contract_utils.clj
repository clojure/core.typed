(ns clojure.core.typed.contract-utils
  (:require [clojure.set :as set]))

;(t/ann ^:no-check nat? (predicate t/AnyInteger))
;(t/ann ^:no-check hash-c? [[Any -> Any] [Any -> Any] -> [Any -> Any]])
;;can't express the alternating args
;(t/ann ^:no-check hmap-c? [Any * -> [Any -> Any]])
;(t/ann ^:no-check set-c? [[Any -> Any] -> [Any -> Any]])
;(t/ann ^:no-check every-c? [[Any -> Any] -> [(U nil (t/Seqable Any)) -> Any]])


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
