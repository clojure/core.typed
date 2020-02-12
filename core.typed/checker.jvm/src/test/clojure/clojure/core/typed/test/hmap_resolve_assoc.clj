;CTYP-79
(ns clojure.core.typed.test.hmap-resolve-assoc
  (:require [clojure.core.typed :as t]))

(t/defalias TA (t/HMap :optional {:d t/Any}))

(t/ann a [TA -> TA])
(defn a [m] (assoc m :d "foo"))
