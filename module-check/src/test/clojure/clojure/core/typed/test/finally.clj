(ns clojure.core.typed.test.finally
  (:require [clojure.core.typed :as t]))

(t/ann f [-> String])
(defn f []
  (try
    "a"
    (finally 1)))
