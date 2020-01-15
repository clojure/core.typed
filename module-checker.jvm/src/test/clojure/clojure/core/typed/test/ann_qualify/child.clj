(ns clojure.core.typed.test.ann-qualify.child
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.test.ann-qualify.parent :as p]))

(t/ann p/a t/Int)
(inc p/a)
