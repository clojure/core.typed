(ns clojure.core.typed.test.mm-warn-on-unannotated
  (:require [clojure.core.typed :as t :refer [check-ns]]))

(t/warn-on-unannotated-vars)

(defmulti foo class)

(defmethod foo Object [a] 1)
