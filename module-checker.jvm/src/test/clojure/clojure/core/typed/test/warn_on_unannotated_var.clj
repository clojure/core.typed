(ns clojure.core.typed.test.warn-on-unannotated-var
  (:require [clojure.core.typed :as t :refer [ann check-ns ann-form print-env cf]])
  (:import (clojure.lang IPersistentMap IPersistentVector)))

(t/warn-on-unannotated-vars)

(defn foo [a]
  (fn [s] (+ s 1)))

(def bar (+ 1 1.2))
