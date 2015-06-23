(ns clojure.core.typed.test.repl_new
  (:require [clojure.core.typed :as t]
            [clojure.test :refer :all]                
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest apropos-test
  (is-tc-e (apropos "clojure") (t/Seq t/Str)        
             :requires [[clojure.repl :refer [apropos]]])
  (is-tc-err (apropos "clojure") t/Str 
             :requires [[clojure.repl :refer [apropos]]])
  (is-tc-err (apropos 'clojure) (t/Seq t/Str) 
             :requires [[clojure.repl :refer [apropos]]]))
             
(deftest demunge-test
  (is-tc-e (demunge "clojure.repl$demunge") t/Str
           :requires [[clojure.repl :refer [demunge]]])
  (is-tc-err (demunge "clojure.repl$demunge") (t/Vec t/Any) 
             :requires [[clojure.repl :refer [demunge]]])
  (is-tc-err (demunge 'clojure.repl$demunge) t/Str 
             :requires [[clojure.repl :refer [demunge]]]))
             
(deftest source-fn-test
  (is-tc-e (source-fn 'source) t/Str
           :requires [[clojure.repl :refer [source-fn]]])
  (is-tc-err (source-fn 'source) (t/Vec t/Any) 
             :requires [[clojure.repl :refer [source-fn]]])
  (is-tc-err (source-fn "source") t/Str 
             :requires [[clojure.repl :refer [source-fn]]]))
