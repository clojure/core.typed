(ns clojure.core.typed.test.mainnew
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]))

(deftest demunge-test
  (is-tc-e (demunge "abc") String 
           :requires [[clojure.main :refer [demunge]]])
  (is-tc-err (demunge "abc") Boolean
             :requires [[clojure.main :refer [demunge]]])
  (is-tc-err (demunge 1) String 
             :requires [[clojure.main :refer [demunge]]]))

(deftest repl-prompt-test
  (is-tc-e #(repl-prompt) [-> Any] 
           :requires [[clojure.main :refer [repl-prompt]]]))

(deftest repl-test
  (is-tc-e #(repl)
           :requires [[clojure.main :refer [repl]]])
  (is-tc-e #(repl :init (fn [])
                  :need-prompt (fn [] (identity true))
                  :flush flush
                  :read repl-read
                  :eval eval
                  :print prn
                  :caught repl-caught)
           :requires [[clojure.main :refer [repl repl-read repl-caught]]]))

(deftest main-test
  (is-tc-e #(main)
           :requires [[clojure.main :refer [main]]])
  (is-tc-e #(main "foo")
           :requires [[clojure.main :refer [main]]]))
           
(deftest load-script-test
  (is-tc-e #(load-script "sample.clj")
           :requires [[clojure.main :refer [load-script]]])
  (is-tc-err #(load-script 1)
             :requires [[clojure.main :refer [load-script]]]))

(deftest repl-caught-test
  (is-tc-e #(repl-caught (Exception. "a"))
           :requires [[clojure.main :refer [repl-caught]]])
  (is-tc-err #(repl-caught "A") Any
             :requires [[clojure.main :refer [repl-caught]]]))

(deftest repl-exception-test
  (is-tc-e #(repl-exception (Exception. "a"))
           :requires [[clojure.main :refer [repl-exception]]])
  (is-tc-err #(repl-exception "A")
             :requires [[clojure.main :refer [repl-exception]]]))

(deftest root-cause-test
  (is-tc-e (root-cause (Exception. "a")) Exception 
             :requires [[clojure.main :refer [root-cause]]])
  (is-tc-err #(root-cause "A")
             :requires [[clojure.main :refer [root-cause]]])
  (is-tc-err #(root-cause (Exception. "a")) [-> String]
             :requires [[clojure.main :refer [root-cause]]]))
