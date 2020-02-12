(ns clojure.core.typed.test.tap
  (:require  [clojure.core.typed :as t] 
             [clojure.test :refer :all]                
             [clojure.core.typed.test.test-utils :refer :all]
             [clojure.core.typed.test.destructure]))

(deftest print-tap-diagnostic-test
  (is-tc-e #(print-tap-diagnostic "abc") [-> Any]
           :requires [[clojure.test.tap :refer [print-tap-diagnostic]]])
  (is-tc-err #(print-tap-diagnostic 1) [-> Any]
             :requires [[clojure.test.tap :refer [print-tap-diagnostic]]]))

(deftest print-tap-plan-test
  (is-tc-e #(print-tap-plan 1) [-> Any]
           :requires [[clojure.test.tap :refer [print-tap-plan]]]))

(deftest print-tap-pass-test
  (is-tc-e #(print-tap-pass 1) [-> Any]
           :requires [[clojure.test.tap :refer [print-tap-pass]]]))

(deftest print-tap-fail-test
  (is-tc-e #(print-tap-fail 1) [-> Any]
           :requires [[clojure.test.tap :refer [print-tap-fail]]]))
