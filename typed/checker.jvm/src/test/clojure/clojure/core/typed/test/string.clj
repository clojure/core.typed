(ns clojure.core.typed.test.string
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest escape-test
  (is-tc-e (escape "I want 1 < 2 as HTML, & other good things."
                   {\< "&", \> "&", \& "&"}) String            
           :requires [[clojure.string :refer [escape]]])
  (is-tc-e (escape "I want 1 < 2 as HTML, & other good things."
                   (fn [a]
                     (case a
                       \< "&"
                       \> "&" 
                       \& "&"
                       nil)))
           String            
           :requires [[clojure.string :refer [escape]]])
  (is-tc-err (escape "I want 1 < 2 as HTML, & other good things."
                     {\< "&", \> "&", \& "&"}) Boolean            
             :requires [[clojure.string :refer [escape]]])
  (is-tc-err (escape 1 {\< "&", \> "&", \& "&"}) String
             :requires [[clojure.string :refer [escape]]]))

(deftest split-lines-test
  (is-tc-e (split-lines "abc\n abc") (Vec String)            
           :requires [[clojure.string :refer [split-lines]]])
  (is-tc-err (split-lines "abc\n abc") Boolean             
             :requires [[clojure.string :refer [split-lines]]])
  (is-tc-err (split-lines 1) (Vec String)
             :requires [[clojure.string :refer [split-lines]]]))
