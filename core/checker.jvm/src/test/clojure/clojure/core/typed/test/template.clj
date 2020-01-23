(ns clojure.core.typed.test.template
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]))

(deftest apply-template-test
  (is-tc-e   (apply-template '[a b c d e] '[d a b e c e b a d] '(1 2 3 4 5))
             :requires [[clojure.template :refer [apply-template]]]) 
  (is-tc-e   (apply-template '[a b c d e] {:a 1 :b 2 :c 3 :d 4 :e 5} '(1 2 3 4 5))
             :requires [[clojure.template :refer [apply-template]]])
  (is-tc-e   (apply-template '[a b c d e] 3 '(1 2 3 4 5))
             :requires [[clojure.template :refer [apply-template]]])  
  (is-tc-err (apply-template '[a b c d e] '[d a b e c e b a d] '(1 2 3 4 5)) 
             (U (Vec Any) (HVec [Any]) (List Any))
             :requires [[clojure.template :refer [apply-template]]]))
