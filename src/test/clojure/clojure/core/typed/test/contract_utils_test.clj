(ns clojure.core.typed.test.contract-utils-test
  (:refer-clojure :exclude [boolean?])
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.contract-utils :as con :refer :all]))

(deftest hmap-c-test
  (is ((hmap-c?) 
       {}))
  (is (not ((hmap-c?) 
            nil)))
  (is ((hmap-c? :k symbol?) 
       {:k 'a}))
  (is (not ((hmap-c? :k symbol?) 
            {})))
  (is ((hmap-c? (optional :k) symbol?) 
       {:k 'a}))
  (is (not ((hmap-c? (optional :k) symbol?) 
            {:k :a})))
  (is ((hmap-c? (optional :k) symbol?) 
       {})))
