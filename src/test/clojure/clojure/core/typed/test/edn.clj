(ns clojure.core.typed.test.edn
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]
             [clojure.core.typed.test.destructure]))

(deftest read-string-test
  (is-tc-e #(edn/read-string "abc")
             :requires [[clojure.edn :as edn]])
  (is-tc-err #(edn/read-string "abc") [-> String]
             :requires [[clojure.edn :as edn]]))
