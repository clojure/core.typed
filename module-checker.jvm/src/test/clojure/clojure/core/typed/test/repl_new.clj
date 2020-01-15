(ns clojure.core.typed.test.repl-new
  (:require [clojure.core.typed :as t]
            [clojure.test :refer :all]                
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest apropos-test
  (is-tc-e #(apropos "clojure") [-> (Seq Sym)]
             :requires [[clojure.repl :refer [apropos]]])
  (is-tc-e #(apropos #"") [-> (Seq Sym)]
             :requires [[clojure.repl :refer [apropos]]])
  (is-tc-err #(apropos "clojure") [-> (Seq String)]
             :requires [[clojure.repl :refer [apropos]]])
  (is-tc-err #(apropos 'clojure) [-> (Seq Str)]
             :requires [[clojure.repl :refer [apropos]]]))
             
(deftest demunge-test
  (is-tc-e #(demunge "clojure.repl$demunge") [-> Str]
           :requires [[clojure.repl :refer [demunge]]])
  (is-tc-err #(demunge "clojure.repl$demunge") [-> (Vec Any)]
             :requires [[clojure.repl :refer [demunge]]])
  (is-tc-err #(demunge 'clojure.repl$demunge) [-> Str]
             :requires [[clojure.repl :refer [demunge]]]))
             
(deftest source-fn-test
  (is-tc-e #(source-fn 'source) [-> (U nil Str)]
           :requires [[clojure.repl :refer [source-fn]]])
  (is-tc-err #(source-fn 'source) [-> (Vec Any)]
             :requires [[clojure.repl :refer [source-fn]]])
  (is-tc-err #(source-fn "source") [-> Str]
             :requires [[clojure.repl :refer [source-fn]]]))
