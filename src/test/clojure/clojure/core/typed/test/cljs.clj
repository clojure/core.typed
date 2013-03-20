(ns clojure.core.typed.test.core
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer :refer [ast]]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed :as tc, :refer :all, :exclude [subtype? check]]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))
