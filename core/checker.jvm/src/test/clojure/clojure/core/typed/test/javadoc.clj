(ns clojure.core.typed.test.javadoc
  (:require  [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.test.destructure]))


(deftest javadoc-test
  (is-tc-e #(javadoc 1) [-> Any]
           :requires [[clojure.java.javadoc :refer [javadoc]]]))

(deftest add-local-javadoc-test
  (is-tc-e #(add-local-javadoc 1) [-> (List Any)]
           :requires [[clojure.java.javadoc :refer [add-local-javadoc]]])
  (is-tc-err #(add-local-javadoc 1) [-> String]
             :requires [[clojure.java.javadoc :refer [add-local-javadoc]]]))

(deftest add-remote-javadoc-test
  (is-tc-e #(add-remote-javadoc 
              "org.apache.commons.csv." 
              "http://commons.apache.org/proper/commons-csv/apidocs/index.html") 
           [-> (Map Any Any)]
           :requires [[clojure.java.javadoc :refer [add-remote-javadoc]]])
  (is-tc-err #(add-remote-javadoc 
                "org.apache.commons.csv."
                "http://commons.apache.org/proper/commons-csv/apidocs/index.html")
             [-> String]
             :requires [[clojure.java.javadoc :refer [add-remote-javadoc]]])
  (is-tc-err #(add-remote-javadoc 
                1 
                "http://commons.apache.org/proper/commons-csv/apidocs/index.html") 
             [-> (Map Any Any)]
             :requires [[clojure.java.javadoc :refer [add-remote-javadoc]]]))
