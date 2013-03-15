(ns clojure.core.typed.test.records
  (:require [clojure.core.typed :refer [check-ns]]
            [clojure.repl :refer [pst]]))

#_(ann-record MyRecord [])
(defrecord MyRecord [])
