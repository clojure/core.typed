(ns clojure.core.typed.test.runtime-check
  {:lang :core.typed
   :core.typed {:features #{:runtime-check}}}
  (:require [clojure.core.typed :as t]))


(t/ann a t/Int)
(def a 1)

;(t/ann b t/Int)
;(def b nil)

#(inc nil)
