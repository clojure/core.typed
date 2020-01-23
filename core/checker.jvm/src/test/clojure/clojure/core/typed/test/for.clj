(ns clojure.core.typed.test.for
  (:require [clojure.core.typed :as t :refer [IFn]]))

(t/for [x :- t/Num, [1 2 3 4]] :- t/Num x)
