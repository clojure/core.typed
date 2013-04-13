(ns clojure.core.typed.test.assoc
  (:require [clojure.core.typed :refer [ann-form def-alias check-ns]]))

(ann-form (assoc {:a 1} :b 2)
          '{:a Number :b Number})
