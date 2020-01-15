(ns clojure.core.typed.annotator.test.runtime-infer.bench-track
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}
   }
  (:require [clojure.core.typed :as t]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]))

(defn stack-depth [i]
  (if (zero? i)
    nil
    (stack-depth (dec i))))

(defn no-rewrap-f [i f]
  (if (zero? i)
    f
    (no-rewrap-f (dec i) f)))

(defn traverse-map-once [i m]
  (if (zero? i)
    (:a m)
    (traverse-map-once (dec i) m)))

(defn call-if-zero [i f]
  (if (= 0 i)
    (f)
    (call-if-zero (dec i) f)))

(comment
(defmacro bench
  "Evaluates expr and returns the time it took."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         msduration# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     [ret# msduration#]))

(defn bench-iteratively 
  ([f n] (bench-iteratively f 0 n))
  ([f start n]
   (loop [times []
          i start]
     (if (< n i)
       times
       (let [[_ t] (f i)]
         (recur (conj times t)
                (inc i)))))))

(defn write-csv [n v]
  {:pre [(vector? v)]}
  (spit n (apply str (interpose "," v))))

  ;; tracking overhead?
(write-csv
  "no-track-stack-depth.csv"
  (bench-iteratively
    (bench (stack-depth i))))

  (stack-depth 1000)
  (traverse-map-once 
    3000
    {:a 1})

  (call-if-zero 1000 (fn [] (prn "called")))
  )
