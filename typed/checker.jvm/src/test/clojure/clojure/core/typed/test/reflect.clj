(ns clojure.core.typed.test.reflect
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.test.destructure]))

(deftest type-reflect-test
  (is-tc-e #(reflect 1) [-> (Map Any Any)]
           :requires [[clojure.reflect :refer [reflect]]])
  (is-tc-err #(reflect 1) [-> String]
             :requires [[clojure.reflect :refer [reflect]]]))
