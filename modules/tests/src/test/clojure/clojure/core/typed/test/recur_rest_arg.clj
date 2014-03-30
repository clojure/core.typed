(ns clojure.core.typed.test.recur-rest-arg
  (:require [clojure.core.typed :as t]))

(t/ann recur-args [Number * -> Any])
(defn recur-args 
  [& args]
  (recur args))
