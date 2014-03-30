(ns clojure.core.typed.test.fail.recur-non-seq-rest
  (:require [clojure.core.typed :as t]))

(t/ann recur-args-fail [Number * -> Any])
(defn recur-args-fail [& args]
  (recur [1 2 3]))
