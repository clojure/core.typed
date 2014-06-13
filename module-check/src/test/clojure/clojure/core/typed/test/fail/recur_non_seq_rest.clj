(ns clojure.core.typed.test.fail.recur-non-seq-rest
  (:require [clojure.core.typed :as t]))

(t/ann recur-args-fail [Number * -> t/Any])
(defn recur-args-fail [& args]
  (recur [1 2 3]))
