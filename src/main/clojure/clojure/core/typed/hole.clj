(ns 
  ^{:see-also [["http://matthew.brecknell.net/post/hole-driven-haskell/" "Hole Driven Development"]]
    :doc "This namespace contains easy tools for hole driven development"}
  clojure.core.typed.hole
  (:require [clojure.core.typed :refer [ann ann-datatype] :as t]))

(ann silent-hole [-> t/Nothing])
(defn silent-hole
  "A silent hole. (silent-hole) passes for any other type
  when type checking.
  Use (silent-hole) as a placeholder for code.
  Throws an exception when evaluted."
  [] 
  (throw (Exception. "silent hole")))

(ann-datatype NoisyHole [])
(deftype NoisyHole [])

(ann noisy-hole [-> NoisyHole])
(defn noisy-hole
  "A noisy hole. The type system will complain when
  (noisy-hole) is used in positions that expect a type
  more specific than Object or Any.
  Use (noisy-hole) as a placeholder for code.
  Throws an exception when evaluted."
  [] 
  (throw (Exception. "noisy hole")))
