(ns clojure.core.typed.test.pred-scoping
  (:require [clojure.core.typed :as unique123 :refer [Int pred]]))

(pred unique123/Int)
