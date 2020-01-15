(ns clojure.core.typed.test.binding
  (:require [clojure.core.typed :as t]))

(t/def ^:dynamic *foo* :- Boolean, true)
(binding [*foo* false])
