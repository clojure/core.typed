(ns typed.test.mm
  (:require [typed.core :as [ann]]))

(ann MapToString [(IPersistentMap Any Any) -> String])

(defmulti MapToString :op)

(defmethod MapToString :test1
  [{:keys [a b]}]
  (str a b))
