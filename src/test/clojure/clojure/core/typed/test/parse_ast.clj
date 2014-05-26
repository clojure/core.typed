(ns clojure.core.typed.test.parse-ast
  (:require [clojure.core.typed :as t]))

(t/ann-record Top [])
(defrecord Top [])

(t/defalias A Top)
