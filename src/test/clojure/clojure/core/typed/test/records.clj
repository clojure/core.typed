(ns clojure.core.typed.test.records
  (:require [clojure.core.typed :refer [check-ns]]
            [clojure.repl :refer [pst]]))

(ann-record MyRecord [])
(defrecord MyRecord [])
