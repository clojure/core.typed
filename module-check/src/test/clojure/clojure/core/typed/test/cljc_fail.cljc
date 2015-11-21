#?(:clj
   (ns clojure.core.typed.test.cljc-fail
     (:require [clojure.core.typed :as t])))

(t/ann fail-func [Number -> Number])
(defn fail-func
  "A function designed to generate a type error."
  [a]
  (+ a []))


