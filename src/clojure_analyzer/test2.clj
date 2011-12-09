(ns clojure-analyzer.test2
  (:use [clojure-analyzer.test :only [testfn blah2]]))

(defn blah [a]
  (testfn a 1))
