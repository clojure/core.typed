(ns clojure.core.typed.test.atom
  (:require [clojure.core.typed :as t :refer [ann ann-form check-ns cf Atom1 fn> def-alias]]
            [clojure.repl :refer [pst]])
  (:import (clojure.lang IPersistentMap Symbol)))

(ann my-atom (Atom1 Number))
(def my-atom (atom 2))

(reset! my-atom 1)
(swap! my-atom (fn> [x :- Number] (+ x 2 3)))

(def-alias InnerEntry (HMap {:c 
                             (HMap {:d 
                                    String})}))
(def-alias Entry (HMap {:a 
                        (HMap {:b 
                               (IPersistentMap Symbol 
                                               (Atom1 InnerEntry))})}))

(ann complicated (Atom1 Entry))
(def complicated (atom {:a {:b {}}}))

;(swap! complicated update-in [:a :b 'a] #(swap! (or % (atom {})) assoc-in [:c :d] "b"))

(swap! complicated (ann-form
                     (fn [c] 
                       (-> c
                        (update-in [:a :b 'a]
                                   (fn [a] (swap! (or a (atom {}))
                                                  (fn [i]
                                                    (-> i (assoc-in [:c :d] "b"))))))))
                     [Entry -> Entry]))
