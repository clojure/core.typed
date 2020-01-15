(ns clojure.core.typed.test.browse
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]
             [clojure.core.typed.test.destructure]))

(deftest browse-url-test
  (is-tc-e #(browse-url "www.typedclojure.org") [-> Any]
           :requires [[clojure.java.browse :refer [browse-url]]])
  (is-tc-err #(browse-url "www.typedclojure.org") [-> Boolean]
             :requires [[clojure.java.browse :refer [browse-url]]]))
