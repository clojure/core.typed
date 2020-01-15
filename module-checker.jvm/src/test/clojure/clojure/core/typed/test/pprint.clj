(ns clojure.core.typed.test.pprint
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]))

(deftest cl-format-test
  (is-tc-e #(cl-format nil "abc")
           :requires [[clojure.pprint :refer [cl-format]]])
  (is-tc-e #(cl-format nil "abc" 'a :foo "bar")
           :requires [[clojure.pprint :refer [cl-format]]])
  (is-tc-e #(cl-format true "abc" 'a :foo "bar")
           :requires [[clojure.pprint :refer [cl-format]]])
  (is-tc-e #(let [so System/out
                  _ (assert so)
                  o (java.io.OutputStreamWriter. so)]
              (assert o)
              (cl-format (java.io.BufferedWriter. o) "abc"))
           :requires [[clojure.pprint :refer [cl-format]]])
  (is-tc-err #(cl-format "abc" "abc")
             :requires [[clojure.pprint :refer [cl-format]]])
  (is-tc-err #(cl-format true true)
             :requires [[clojure.pprint :refer [cl-format]]]))

(deftest fresh-line-test
  (is-tc-e #(fresh-line)
           :requires [[clojure.pprint :refer [fresh-line]]]))
 
(deftest get-pretty-writer-test
  (is-tc-e #(let [so System/out
                  _ (assert so)
                  o (java.io.OutputStreamWriter. so)]
              (assert o)
              (get-pretty-writer o))
           [-> java.io.Writer]
           :requires [[clojure.pprint :refer [get-pretty-writer]]])) 

(deftest pprint-test
  (is-tc-e #(pprint 1) [-> nil]
           :requires [[clojure.pprint :refer [pprint]]])
  (is-tc-e #(let [so System/out
                  _ (assert so)
                  o (java.io.OutputStreamWriter. so)]
              (assert o)
              (pprint 1 o))
           [-> nil]
           :requires [[clojure.pprint :refer [pprint]]]))
