(ns cljs.core.typed.test.ann
  (:require [cljs.core.typed :refer-macros [ann check-ns]]))

(ann foo number)
(def foo 1)
