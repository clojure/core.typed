(ns cljs.core.typed.test.ann
  (:require-macros [cljs.core.typed :refer [ann check-ns]]))

(ann foo number)
(def foo 1)
