(ns typed.test.mm
  (:import (clojure.lang IPersistentMap))
  (:require [typed.core :refer [ann]]))

(ann MapToString [(IPersistentMap Any Any) -> String])

(defmulti MapToString :op)

(defmethod MapToString :test1
  [{:keys [a b]}]
  (str a b))
