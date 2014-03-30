(ns clojure.core.typed.test.fail.recur-empty-seq
  (:require [clojure.core.typed :as t]))

(t/ann recur-args-fail [Number * -> Any])
(defn recur-args-fail [& args]
  (recur ()))
