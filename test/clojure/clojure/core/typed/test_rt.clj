(ns clojure.core.typed.test-rt
  (:require clojure.core.typed)
  (:use clojure.test))

(deftest typed-clojure-loaded
  (is clojure.core.typed/load-if-needed)
  (println "Successfully required TC without dependencies"))
