(ns clojure.core.typed.test.finally
  (:require [clojure.core.typed :refer :all]))

(ann f [-> String])
(defn f []
  (try
    "a"
    (finally 1)))
