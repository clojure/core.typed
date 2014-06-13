(ns clojure.core.typed.test.hvec-dotted
  (:require [clojure.core.typed :as t]))

(declare vector*)

(t/ann ^:no-check vector* (t/All [x ...]
                            [x ... x -> '[x ... x]]))

(t/ann foo [-> '[Number Number Number]])
(defn foo []
  (vector* 1 2 3))
