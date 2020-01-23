(ns clojure.core.typed.test.duplicated
  (:require [clojure.core.typed :as t]))

; this is intentional type error for test purposes
(inc 'a)
