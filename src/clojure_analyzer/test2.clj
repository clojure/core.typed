(ns clojure-analyzer.test2
  (:use [clojure-analyzer.test :only [testfn blah2]])
  (:use clojure-analyzer.test)
  (:require clojure-analyzer.test))

(defn blah [a]
  (testfn a 1))
