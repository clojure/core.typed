(ns clojure.core.typed.test.inspector
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]))

(deftest atom?-test
  (is-tc-e #(atom? "abc") [-> Boolean]
           :requires [[clojure.inspector :refer [atom?]]])
  (is-tc-err #(atom? "abc") [-> String]
             :requires [[clojure.inspector :refer [atom?]]]))

(deftest collection-tag-test
  (is-tc-e #(collection-tag "abc") [-> Keyword]
           :requires [[clojure.inspector :refer [collection-tag]]])
  (is-tc-err #(collection-tag "abc") [-> String]
             :requires [[clojure.inspector :refer [collection-tag]]]))

(deftest tree-model-test
  (is-tc-e #(tree-model "abc")
           :requires [[clojure.inspector :refer [tree-model]]]))

(deftest old-table-model-test
  (is-tc-e #(old-table-model [[1 2 3] [4 5 6] [7 8 9] [10 11 12]])
           :requires [[clojure.inspector :refer [old-table-model]]])
  (is-tc-err #(old-table-model 1)
             :requires [[clojure.inspector :refer [old-table-model]]]))

(deftest inspect-test
  (is-tc-e #(inspect "abc") [-> javax.swing.JFrame]
           :requires [[clojure.inspector :refer [inspect]]])
  (is-tc-err #(inspect "abc") [-> String]
             :requires [[clojure.inspector :refer [inspect]]]))

(deftest inspect-tree-test
  (is-tc-e #(inspect-tree "abc") [-> javax.swing.JFrame]
           :requires [[clojure.inspector :refer [inspect-tree]]])
  (is-tc-err #(inspect-tree "abc") [-> String]
             :requires [[clojure.inspector :refer [inspect-tree]]]))

(deftest inspect-table-test
  (is-tc-e #(inspect-table [[1 2 3] [4 5 6] [7 8 9] [10 11 12]]) [-> javax.swing.JFrame]
             :requires [[clojure.inspector :refer [inspect-table]]])
  (is-tc-err #(inspect-table 1)
             :requires [[clojure.inspector :refer [inspect-table]]]))
