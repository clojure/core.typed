(ns clojure.core.typed.annotator.test.rt-infer.vector
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}}
  (:require [clojure.core.typed :as t]))

(defn b [coll]
  (if (< 0.5 (rand))
    [1 2 3]
    [1 2 3 4]))

(defn nilable [coll]
  (if (< 0.5 (rand))
    nil
    coll))

(dotimes [_ 10]
  (b (vec (repeat (int (* 10 (rand) )) 10)))
  (nilable (vec (repeat (int (* 10 (rand) )) 10))))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step (fn step [v-seqs]
               (let [increment
                     (fn [v-seqs]
                       (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                         (if (= i -1) nil
                           (if-let [rst (next (v-seqs i))]
                             (assoc v-seqs i rst)
                             (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
                 (when v-seqs
                   (cons (map first v-seqs)
                         (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn selections
  "All the ways of taking n (possibly the same) elements from the sequence of items"
  [items n]
  (apply cartesian-product (take n (repeat items))))

(cartesian-product [1 2] [3 4])
(cartesian-product [1 2 3] [3 4 5])
(selections [1 2] 3)
(selections [1 2 3] 3)
