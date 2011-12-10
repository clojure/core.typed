(ns clojure-analyzer.test-run
  (:require [clojure.pprint :as pprint]
            [clojure-analyzer.compiler :as a])
  (:import [clojure.lang.RT]))

(a/analyze-namespace 'clojure-analyzer.test)
;(a/analyze-namespace 'clojure-analyzer.test)
;(a/analyze-namespace 'typed-clojure.types)
