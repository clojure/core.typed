(ns clojure.core.typed.test.runtime-infer.bench-track
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

(defn traverse-map-once [i m]
  (if (zero? i)
    (:a m)
    (traverse-map-once (dec i) m)))

(defn call-if-zero [i f]
  (if (= 0 i)
    (f)
    (call-if-one (dec i) f)))

(comment
  (stack-depth 1000)
  (traverse-map-once 
    3000
    {:a 1})

  (call-if-one 1000 (fn [] (prn "called")))
  )
