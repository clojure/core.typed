(ns typed.test.mm
  (:import (clojure.lang IPersistentMap))
  (:require [typed.core :refer [def-alias ann-multi check-ns print-env cf]]
            [clojure.repl :refer [pst]]))

(def-alias Expr
  (U '{:op ':test1
       :a Number
       :b Number}
     '{:op ':test2}))

(ann-multi MapToString [Expr -> String])

(defmulti MapToString #(:op %))

(defmethod MapToString :test1
  [a]
  (print-env "mm")
  (str a))
