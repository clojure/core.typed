;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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
