(ns clojure.core.typed.test.pdot-cs-gen
  (:require [clojure.core.typed :as t]))

; because too many test case have constraint on clojure.core/assoc,
; so we first use this to ensure <* & <... works and then annotate assoc as it
(t/ann ^:no-check dummy-assoc (All [m k v c ...]
                                (Fn
                                  [m k v (HSequential [c c] :repeat true) <... c
                                   -> (Assoc m k v c ... c)]
                                  [m k v (HSequential [k v] :repeat true) <*
                                   -> (Assoc m k v)]
                                  )))
(defn dummy-assoc [m k v & rst] nil)

(t/ann m (t/Map String Number))
(def m (dummy-assoc {} "b" 2 "c" 3 "d" 4))
