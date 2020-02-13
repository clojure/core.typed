(ns clojure.core.typed.test.set-bang
  (:import (clojure.lang Keyword))
  (:require [clojure.core.typed :as t]))

(t/ann foo Keyword)
(def ^:dynamic foo :foo)

(t/ann bar [-> Keyword])
(defn bar []
  (set! foo :bar))

