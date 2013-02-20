(ns typed.test.core
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.test :refer :all]
            [analyze.core :refer [ast]]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [typed.core :as tc, :refer :all, :exclude [subtype? check]]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))
